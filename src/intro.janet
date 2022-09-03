# Hello, and welcome to an extremely
# early and unfinished demo!

# Bauble is a playground for creating
# and rendering 3D shapes using signed
# distance functions. Like this one:

(spoon :r 15
  (torus :z 50 25)
  (move :y 50 | rotate :y tau/4)
| fresnel 1)

# Drag the viewport around with your
# mouse, and scroll to move the camera
# in and out.

# This text field is a Janet program
# that is re-evaluated every time you
# make a change. This program "returns"
# whatever the final expression is --
# in this case, those interlocking
# donuts up there. Uncomment the next
# line to return something else:

# (morph 2.50 (sphere 50) (box 50))

# Janet is a fully-featured language, so
# you can define variables, functions,
# macros, loops -- anything your heart
# desires. Here's a nonsense example --
# to uncomment it, select the whole
# paragraph and press "cmd-/"
# or "ctrl-/":

# (var top 0)
# (defn hollow-box [size]
#   (subtract :r 5
#     (box size :r 2)
#     (sphere (* 1.20 size))))
# (defn stack-box [size]
#   (let [result (move :y (+ top size)
#                  (hollow-box size))]
#     (+= top (* 2 size))
#     result))
# (move :y -45
#   (union :r 10
#     ;(map stack-box [40 30 20])))

# You can also edit values with your
# mouse. Uncomment the next block of
# code, then ctrl-click and drag the
# value 0.00 left to right.

# (def r 0.00)
# (-> (box 80)
#   (rotate-pi :y r :z (* r 0.7) :x (* 0.5 r))
#   (symmetry))

# You can also hold down cmd-shift to
# move your mouse without clicking
# anywhere to edit the value under the
# text cursor. This is a workaround for
# Firefox on macOS, which due to a
# long-standing bug cannot report
# ctrl-click events correctly.
# https://bugzilla.mozilla.org/show_bug.cgi?id=1504210

# When editing values with your mouse,
# Bauble will increment the smallest
# digit of the number, so you can
# increase the precision by adding
# zeroes to the end. In other words,
# editing a value like 3.0 will
# increment by 0.1, but editing 3.000
# will increment by 0.001.

# There will be keyboard shortcuts for
# these things eventually but I haven't
# implemented them yet.

#### Surfacing ####

# So far everything has been weird
# shades of pastel, which is a mapping
# of the XYZ normal vector into RGB
# space. That's the default surface for
# new shapes, but you can apply other
# surfaces:

# (union
#   (shade (box 50 :r 10)
#     (hsv 0.00 1 1)
#     :gloss 4
#     :shine 0.5
#     :ambient 0.2)
#   (shade (half-space :-y -50)
#     [0.9 0.9 0.9]))

# (shade) is an alias for (blinn-phong),
# a simple material shader. Try tweaking
# the parameters to see how they work,
# and remember that you can use your
# mouse to edit numbers! Also note that
# specular highlights depend on the
# viewing angle, so rotate the viewport
# a little too.

# When you combine shapes together, you
# also combine their surfaces. For
# example, here are a couple shapes:

# (def green-box (shade [0 1 0] (box 50 :r 5) :gloss 12 :shine 1))
# (def red-sphere (shade [1 0 0] (sphere 60)))

# Now uncomment each of these one at a
# time to see how the colors interact:

# (union green-box red-sphere)
# (intersect green-box red-sphere)
# (subtract green-box red-sphere)

# And now let's try it with smooth
# transitions:

# (union :r 5 green-box red-sphere)
# (intersect :r 5 green-box red-sphere)
# (subtract :r 5 green-box red-sphere)

# That's interesting, but sometimes you
# might not want to see that yellow
# bleeding through. Sometimes you want
# a smooth shape transition, but a sharp
# color transition. And you can have it:

# (resurface
#   (subtract :r 5 green-box red-sphere)
#   (subtract green-box red-sphere))

# (resurface) works to transplant the
# color field from any shape to
# another shape. In that case the shapes
# were very similar, but they don't have
# to be.

# (resurface
#   green-box
#   (union green-box red-sphere))

# The way this works is that the
# raymarcher uses the signed distance
# field from the first shape to
# determine the geometry, but when it
# hits the surface it uses the second
# shape to determine the color.

# This is a useful technique for
# "painting" complex colors onto shapes,
# but you can also use (resurface) to
# save a material to apply to multiple
# shapes. Instead of this:

# (shade [1 1 0] (sphere 50))

# You can write:

# (def yellow (shade [1 1 0]))
# (resurface (sphere 50) yellow)

# The way this works is that (shade) and
# other material primitives, when not
# given a shape to act on, default to
# the entirety of ℝ³ -- the shape that
# is a distance 0 away from every point.
# So a "material" is still a pair of
# distance and color functions, but the
# distance function isn't really useful.

# Last thing: Bauble also has functions
# to modify the underlying color field
# in some way. Actually, just one at the
# moment:

# (fresnel green-box [1 1 0] 0.5 :exponent 5)

# That adds a little bit of (simulated)
# fresnel reflectivity to a surface.
# Move the camera around a bit to see
# what it does. Note that Bauble doesn't
# actually support reflection yet, so it
# just tints the edges, but it still
# looks pretty nice.

# All of the arguments are optional,
# so you can quickly apply it to a shape
# and add a little depth. Note that it
# works even with the default
# normal-coloring:

# (sphere 50)
# (fresnel (sphere 50))

#### Lisp heresy ####

# So far our examples have mostly stuck
# to "vanilla" Janet, which, of course,
# has a lot of parentheses. But Bauble
# provides a helpful macro that you can
# use to invoke functions with a little
# less typing. Let's take a look,
# starting without any helpers:

# (shade [1 0.1 0.1] (rotate :y pi/4 (box 50 :r 5)))

# First of all, the Bauble DSL is very
# forgiving about named and positional
# argument order. So that's actually the
# same as:

# (shade (rotate (box :r 5 50) :y pi/4) [0.1 1 0.1])

# Janet provides a useful threading
# macro that we can use to write this
# as a subject and then a series of
# transformations, so that the
# expression is not as nested:

# (-> (box 50 :r 5) (rotate :y pi/4) (shade [0.1 0.1 1]))

# Which is very useful. Bauble lets you
# go a little bit further:

# (box 50 :r 5 | rotate :y pi/4 | shade [1 1 0.1])

# At first this might not look like Lisp
# at all, but it's a pretty simple macro
# that has the same effect as the (->)
# threading macro -- but it's a lot
# easier to type out.

#### Expressions ####

# Alright, now we're getting to the good stuff.

# (cone :y 100 100)

# We made a cone. Boring. Let's make it
# less boring:

# (cone :y 100 (+ 100 (* 10 (cos (/ p.x 5)))))

# Whoa, okay; there's a lot to unpack
# there.

# First of all: p.x is more lisp heresy.
# This is equivalent to writing the
# more verbose:

# (cone :y 100 (+ 100 (* 10 (cos (/ ~(. ,p :x) 5)))))

# But what is p? And why can you divide
# by this weird symbolic expression?

# p is a magic variable that represents
# the current point in space. It's a
# symbolic expression, and the /
# operator -- and all other
# operators -- are overloaded to work
# on symbolic expressions. (+ 1 2)
# produces 3, but (+ 1 p.x) produces
# the Janet tuple ~(+ 1 (. ,p :x)), and
# Bauble knows how to compiles that
# into a string of GLSL code that will
# execute on the GPU.

# Symbolic expressions nest, so when we
# take the cosine of ~(/ (. ,p :x) 5),
# we wind up with the symbolic
# expression ~(cos (/ (. ,p :x) 5)).
# And so on, until we finally have an
# expression for the cone's height.

# Oh, hey! S-expressions! That's what
# that stands for. Look at us: we're
# doing lisp. Real live lisp.

# When writing a distance expression,
# the only magic variables you can use
# are p and world-p. p is the point in
# space local to the current shape
# (so translated, rotated, etc), while
# world-p is the global position of the
# ray (this is useful for lighting, to
# calculate reflection or specular
# highlights).

# When writing a color expression, you
# also have access to these other magic
# variables:

# - camera: (the position of the camera,
#   in global (world) coordinates)
# - normal: an approximation of the
#   normal vector at the point you're
#   shading

#### Spatial artifacts ####

# Let's return to our cone.

# (cone :y 100 (+ 100 (* 10 (cos (/ p.x 5)))))

# Actually, let's really lean on the
# pipe operator for a second:

# (cone :y 100 (p.x | / 5 | cos | * 10 | + 100))

# What do you think? Neat? Horrifying? I
# kinda like that, but it's definitely an
# acquired taste.

# Anyway, drag the camera around, and
# direct your attention to the tip of
# the cone. See how it seems to flicker?

# That's because we no longer have an
# accurate distance field to raymarch.
# Because the shape depends on the
# position of the ray, the distance
# field is only correct when the ray is
# very close to the distorted cone.

# This means that sometimes the
# raymarcher will overshoot the surface
# of the cone -- landing inside the
# cone or, in this case, passing
# through it entirely.

# Here's a more extreme example:

# (torus :x 100 25 | rotate :y (* p.y 0.020))

# We're rotating this torus by an angle
# that varies over the y axis, which
# gives us a twisting effect. But it
# looks really bad! Especially if you
# view it from the top down.

# We can mitigate this sort of error by
# slowing down the raymarcher as it
# approaches the torus. Since the
# distance field is no longer giving us
# accurate values, we can choose to
# advance our rays by only half the
# reported distance value. This is called
# slow:

# (torus :x 100 25 | rotate :y (* p.y 0.020) | slow 0.5)

# It's aptly named, because it will
# increase the number of raymarching
# steps we have to take.

# Try increasing the twist amount.
# Eventually you will notice that a
# slow coefficient of 0.5 isn't enough,
# and you'll have to reduce it. But
# reducing it too much will start to
# introduce new artifacts, as the
# raymarcher might begin to hit the
# maximum steps per fragment
# (currently hardcoded to 256) before
# it finds the torus. So there's sort
# of a limit to how distorted you can
# make space.

# Also note that slowing down the
# raymarcher has other effects as well.
# Functions that rely on the distance
# field, like boolean operations, will
# not be as accurate. For example, look
# at this snowman:

# (sphere 50 | move :y -10 | union :r 10 (sphere 40 | move :y 45))

# If we slow down space around one of
# the spheres, the smooth union will no
# longer be symmetric:

# (sphere 50 | move :y -10 | slow 0.5 | union :r 10 (sphere 40 | move :y 45))

# But if we slow down space around the
# whole shape, we won't have a
# problem:

# (sphere 50 | move :y -10 | union :r 10 (sphere 40 | move :y 45) | slow 0.5)

# Lastly, slowing down space will cause
# soft shadows to become too soft, for
# complicated reasons that I don't want
# to explain right now this is so long
# already.

#### Overloading ####

# I already mentioned that many of the
# built-in operators are overloaded to
# work on symbolic expressions. They're
# also overloaded to work on vectors,
# in ways that mirror GLSL functions.

# For example, you can write (+ [1 2 3]
# [4 5 6]) to add the elements of two
# tuples together. That would normally
# be a type error in Janet, but inside
# Bauble, + has been overloaded to
# match GLSL's semantics.

# In addition many -- but not all --
# GLSL functions have been ported to
# Janet, and when you call them with
# constant arguments they will execute
# on the CPU. For example, (distance
# [0 0] [1 1]) will give you the number
# 1.414. But(distance [0 0] p.xy) will
# produce a symbolic expression that
# will execute on the GPU.

# One notable exception is length
# (), since that's already a very
# common Janet function that returns
# the length of an array or tuple. As
# such (length [1 2 3]) is 3, but
# (length p) is the symbolic expression
# ~(length ,p). The generic version of
# GLSL's length() is called
# (vec-length).

# If for some reason you want to *force*
# a computation to occur on the GPU,
# you can quote it like this:
# ~(distance [0 0] ,p.xy). Note that you
# have to unquote p.xy, because the
# magic variable p is more than just a
# symbol.

# Also note that some functions -- for
# example, the procedural noise
# functions -- always produce symbolic
# expressions, even with constant
# arguments. So they'll always execute
# on the GPU.

#### Helpers ####

# smoothstep is a cubic interpolation
# function with native GPU support that
# shows up all the time in procedural
# art.

# Bauble has a helper that makes it a
# little more convenient to use:

# (ss x) = (smoothstep 0 1 x)
# (ss x hi) = (smoothstep 0 hi x)
# (ss x lo hi) = (smoothstep lo hi x)
# (ss x lo hi to-hi) = (* (smoothstep lo hi x) to-hi)
# (ss x lo hi to-lo to-hi) = (+ to-lo (* (smoothstep lo hi x) (- to-hi to-lo)))

# Also, there are many functions that
# return values in the range -1 to 1,
# when often you want values in the
# range 0 to 1 (to use as a color, for
# example). There is a helper function
# called remap+ defined like this:

# (defn remap+ [x]
#   (* 0.5 (+ x 1)))

# For example, you can use it to
# re-create the default normal
# coloring, but with lighting and
# shadows:

# (sphere 50)
# (sphere 50 | shade (remap+ normal))

# There are also "+" versions of a few
# functions that you might frequently
# want to remap into the 0 to 1 range:

# (sin+ x)    = (remap+ (sin x))
# (cos+ x)    = (remap+ (cos x))
# (perlin+ x) = (remap+ (perlin x))

#### Procedural noise ####

# Bauble currently has a single noise
# function:

# (box 100 :r 10 | color [0 (perlin+ (* 0.1 p)) 0])

# (perlin) can take a vec2, vec3, or
# vec4. Each one is more expensive to
# compute than the last, so only use
# what you need. (The vec4 version is
# useful if you want a 3D noise signal
# that varies over time.)

# (box 100 :r 10 | color [0 (perlin+ (* 0.1 p.xz)) 0])

# You can use noise to compute complex
# procedural textures:

# (def spots (+ 0.15 (perlin+ (* 0.103 p))))
# (def outline (step (abs (- 0.5 spots)) 0.016))
# (def brown (hsv 0.01 0.63 0.5))
# (def tan   (hsv 0.07 0.63 0.9))
# (sphere 100 | shade (mix brown tan (round spots) | * (max outline 0.05)))

# Just beautiful. Let's hold on to that
# one, I have a feeling we're going to
# do great things together:

# (def leppard (shade (mix brown tan (round spots) | * (max outline 0.05))))
# (line [20 0 32] [50 -50 50] 5
# | mirror :x :z
# | union :r 10
#   (box [33 20 41] :r 10)
#   (sphere 20 | move :z 51 :y 24)
# | resurface leppard
# | union (sphere 5 | move [7 34 67] | mirror :x | shade [1 1 1]))

# We will never speak of this again.

#### Light and shadow ####

# Currently there's no way to customize
# anything about the lighting used by
# the built-in Blinn-Phong shader. But
# it's coming soon!

#### Getting Help ####

# Uhhh okay look you have just read
# literally all of the documentation.

# Sorry about that.

# You can print values for debugging
# with (print "string") or
# (pp expression). Error messages are
# extremely bad right now, so don't
# make any mistakes. If you write an
# infinite loop it *will* just hang the
# browser tab and you will have no way
# to get out of it except to refresh
# the page.

# Your changes will automatically save,
# but if you want to restore this
# initial tutorial, just empty out this
# text field and refresh the page.

# For more info... maybe check out the
# source?

# https://github.com/ianthehenry/bauble/blob/master/src/dsl.janet
# https://github.com/ianthehenry/bauble/blob/master/src/helpers.janet

# Or try studying these examples:

# (union (box 50) (sphere 70))
# (union :r 10 (box 50) (cone :z 40 100))
# (sphere 100 | onion 5 | intersect (half-space :-z))
# (sphere 100 | onion 5 | intersect :r 5 (half-space :-z 60))
# (morph 0.5 (box 50) (sphere 50))
# (box 50 | subtract (cylinder :z 30 100))
# (subtract :r 30 (box 50 | rotate :y tau/8 :z tau/8) (sphere 50 | move :x 50))
# (cone :x 50 100)
# (cone :x 50 100 | reflect :x)
# (cone :x 50 100 | rotate :y pi/4 | mirror :x :z)
# (union :r 50 (line [-50 0 0] [50 0 0] 10) (sphere 50 | move :x 100 | mirror :x))
# (sphere 50 | tile [100 100 100] :limit [3 10 2])
# (cone :-z 50 100 :r 10)
# (cone :-z 50 100 | offset 10)
# (box 40 | scale 1.5)
# (box 40 | scale :x 0.5)
# (box 40 | scale [1 2 0.5])
# (torus :y 100 25)
# (box 50 | twist :y 0.010 | slow 0.5)
# (box [50 10 50] | bend :x :y 0.010 | slow 0.5)
# (box 50 | swirl :y 0.040 | slow 0.5)

# Comments? Questions? Requests?
# https://github.com/ianthehenry/bauble/discussions

# Found a bug? Let me know!
# https://github.com/ianthehenry/bauble/issues