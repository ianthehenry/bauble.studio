(declare-project
  :name "bauble-cli"
  :description "CLI for Bauble"
  :dependencies [
  "https://github.com/ianthehenry/jaylib.git"
  "https://github.com/ianthehenry/judge.git"
  "https://github.com/ianthehenry/cmd.git"
  "https://github.com/ianthehenry/janet-module.git"

  # we don't actually depend on these. these are transient dependencies from jlsl.
  # there's no way good way for multiple janet libraries to cohabitate in a single
  # git repo so here we are.
  "https://github.com/ianthehenry/pat.git"
  ])

(declare-executable
 :name "main"
 :description "bauble CLI"
 :entry "main.janet")
