import {basicSetup} from 'codemirror';
import {EditorView, keymap, ViewUpdate} from '@codemirror/view';
import {indentWithTab} from '@codemirror/commands';
import {syntaxTree} from '@codemirror/language';
import {SyntaxNode} from '@lezer/common';
import {janet} from 'codemirror-lang-janet';
import {
  EditorState, EditorSelection, Transaction,
} from '@codemirror/state';
import Big from 'big.js';

const TAU = 2 * Math.PI;
const cameraRotateSpeed = 1 / 512;
const cameraZoomSpeed = 0.01;
const LOCAL_STORAGE_KEY = "script";

function clearOutput() {
  const output = document.getElementById('output')!;
  output.innerHTML = "";
}

function print(text: string, isErr=false) {
  if (isErr) {
    console.error(text);
  } else {
    console.log(text);
  }
  const output = document.getElementById('output')!;
  const span = document.createElement('span');
  span.classList.toggle('err', isErr);
  span.appendChild(document.createTextNode(text));
  span.appendChild(document.createTextNode('\n'));
  output.appendChild(span);
  output.scrollTop = output.scrollHeight;
}

interface Camera { x: number, y: number, zoom: number; }
let evaluateJanet: ((_code: string, _cameraX: number, _cameraY: number, _cameraZoom: number) => number) | null = null;
let ready: (() => void) | null = function() { ready = null; };

function onReady(f: (() => void)) {
  if (ready == null) {
    f();
  } else {
    const old = ready;
    ready = function() {
      old();
      f();
    };
  }
}

const preamble = '(use ./helpers) (use ./dsl) (use ./pipe-syntax) (use ./dot-syntax) (resolve-dots (pipe \n';
const postamble = '\n))'; // newline is necessary in case the script ends in a comment

function executeJanet(code: string, camera: Camera) {
  if (evaluateJanet === null) {
    console.error('not ready yet');
    return;
  }

  const result = evaluateJanet(preamble + code + postamble, TAU * camera.x, TAU * camera.y, camera.zoom);
  if (result !== 0) {
    console.error('compilation error: ', result.toString());
  }
}

interface MyEmscripten extends EmscriptenModule {
  cwrap: typeof cwrap;
}

declare global {
  interface Window { Module: Partial<MyEmscripten>; }
}

let resolveInitialScript = null;
const initialScript = new Promise((x) => { resolveInitialScript = x; });

const Module: Partial<MyEmscripten> = {
  preRun: [],
  print: function(x: string) {
    print(x, false);
  },
  printErr: function(x: string) {
    print(x, true);
  },
  postRun: [function() {
    evaluateJanet = Module.cwrap!("run_janet", 'number', ['string', 'number', 'number', 'number']);
    ready();
    resolveInitialScript(FS.readFile('intro.janet', {encoding: 'utf8'}));
  }],
  locateFile: function(path, prefix) {
    if (prefix === '') {
      return '/js/' + path;
    } else {
      return prefix + path;
    }
  },
};

function isNumberNode(node: SyntaxNode) {
  return node.type.name === 'Number';
}

interface StateCommandInput {state: EditorState, dispatch: (_: Transaction) => void}

function alterNumber({state, dispatch}: StateCommandInput, amount: Big) {
  const range = state.selection.ranges[state.selection.mainIndex];
  const tree = syntaxTree(state);

  let node = tree.resolveInner(range.head, -1);
  if (!isNumberNode(node)) {
    node = tree.resolveInner(range.head, 1);
  }
  if (!isNumberNode(node)) {
    return false;
  }

  // TODO: we shouldn't be doing any floating point math; we should
  // parse this as a decimal number and increment it as a decimal number
  const numberText = state.sliceDoc(node.from, node.to);
  let number;
  try {
    number = Big(numberText);
  } catch (e) {
    console.error('unable to parse number: ', numberText);
    return false;
  }
  const decimalPointIndex = numberText.indexOf('.');
  const digitsAfterDecimalPoint = decimalPointIndex < 0 ? 0 : numberText.length - decimalPointIndex - 1;
  const increment = Big('10').pow(-digitsAfterDecimalPoint);

  const newNumber = number.add(amount.times(increment));
  const newNumberText = newNumber.toFixed(digitsAfterDecimalPoint);

  const lengthDifference = newNumberText.length - numberText.length;

  dispatch(state.update({
    changes: {
      from: node.from,
      to: node.to,
      insert: newNumberText,
    },
    selection: EditorSelection.single(node.from, node.to + lengthDifference),
    scrollIntoView: true,
    userEvent: 'increment',
  }));
  return true;
}

function clamp(value: number, min: number, max: number) {
  return Math.max(Math.min(value, max), min);
}

function save({state}: StateCommandInput) {
  const script = state.doc.toString();
  if (script.trim().length > 0) {
    localStorage.setItem(LOCAL_STORAGE_KEY, script);
  } else {
    localStorage.removeItem(LOCAL_STORAGE_KEY);
  }
  return true;
}

function mod(a: number, b: number) {
  return ((a % b) + b) % b;
}

interface GestureEvent extends TouchEvent {
  scale: number
}

function initialize(script) {
  const camera = {
    x: -0.125,
    y: 0.125,
    zoom: 2.0,
  };

  let drawScheduled = false;
  function draw() {
    if (drawScheduled) {
      return;
    }
    drawScheduled = true;
    requestAnimationFrame(function() {
      drawScheduled = false;
      clearOutput();
      executeJanet(editor.state.doc.toString(), camera);
    });
  }

  const incrementNumber = (editor: StateCommandInput) => alterNumber(editor, Big('1'));
  const decrementNumber = (editor: StateCommandInput) => alterNumber(editor, Big('-1'));

  const editorContainer = document.getElementById('editor-container')!;

  const editor = new EditorView({
    extensions: [
      basicSetup,
      janet(),
      keymap.of([
        indentWithTab,
        { key: "Alt-h", run: incrementNumber, shift: decrementNumber },
        { key: "Mod-s", run: save },
      ]),
      EditorView.updateListener.of(function(viewUpdate: ViewUpdate) {
        if (viewUpdate.docChanged) {
          draw();
        }
      }),
      EditorView.theme({
        ".cm-content": {
          fontFamily: "Menlo, monospace",
          fontSize: "13px",
        },
      }),
    ],
    parent: editorContainer,
    doc: script,
  });

  // honestly this is so annoying on firefox that
  // i'm not even gonna bother
  const usePointerLock = false;
  if (usePointerLock) {
    document.addEventListener('keydown', (e) => {
      if (e.key === 'Control') {
        document.body.requestPointerLock();
      }
    });
    document.addEventListener('keyup', (e) => {
      if (e.key === 'Control') {
        document.exitPointerLock();
      }
    });
  }

  const canvas = document.getElementById('render-target')! as HTMLCanvasElement;
  let canvasPointerAt = [0, 0];
  let rotatePointerId = null;
  canvas.addEventListener('pointerdown', (e) => {
    if (rotatePointerId === null) {
      e.preventDefault();
      canvasPointerAt = [e.offsetX, e.offsetY];
      canvas.setPointerCapture(e.pointerId);
      rotatePointerId = e.pointerId;
    }
  });

  canvas.addEventListener('pointerup', (e) => {
    e.preventDefault();
    if (e.pointerId === rotatePointerId) {
      rotatePointerId = null;
    }
  });

  let isGesturing = false;
  let gestureEndedAt = 0;
  canvas.addEventListener('pointermove', (e) => {
    if (e.pointerId === rotatePointerId) {
      e.preventDefault();
      const pointerWasAt = canvasPointerAt;
      canvasPointerAt = [e.offsetX, e.offsetY];

      if (isGesturing) {
        return;
      }
      // if you were just trying to zoom,
      // we don't want to do a little tiny
      // pan as you lift your second finger.
      // so we wait 100ms before we allow
      // panning to continue
      if (performance.now() - gestureEndedAt < 100) {
        return;
      }

      const movementX = canvasPointerAt[0] - pointerWasAt[0];
      const movementY = canvasPointerAt[1] - pointerWasAt[1];
      // TODO: pixelScale shouldn't be hardcoded
      const pixelScale = 0.5;
      const scaleAdjustmentX = pixelScale * canvas.width / canvas.clientWidth;
      const scaleAdjustmentY = pixelScale * canvas.height / canvas.clientHeight;
      // TODO: invert the meaning of camera.x/y so that this actually makes sense
      camera.x = mod(camera.x - scaleAdjustmentY * cameraRotateSpeed * movementY, 1.0);
      camera.y = mod(camera.y - scaleAdjustmentX * cameraRotateSpeed * movementX, 1.0);
      draw();
    }
  });

  // TODO: I haven't actually tested if this is anything
  let initialZoom = 1;
  canvas.addEventListener('gesturestart', (_e: GestureEvent) => {
    initialZoom = camera.zoom;
    isGesturing = true;
  });
  canvas.addEventListener('gestureend', (_e: GestureEvent) => {
    initialZoom = camera.zoom;
    isGesturing = false;
    gestureEndedAt = performance.now();
  });
  canvas.addEventListener('gesturechange', (e: GestureEvent) => {
    camera.zoom = initialZoom / e.scale;
    draw();
  });

  canvas.addEventListener('wheel', (e) => {
    e.preventDefault();
    // Linux Firefox users who do not set MOZ_USE_XINPUT2
    // will report very large values of deltaY, resulting
    // in very choppy scrolling. I don't really know a good
    // way to fix this without explicit platform detection.
    camera.zoom += cameraZoomSpeed * e.deltaY;
    draw();
  });

  const outputContainer = document.getElementById('output')!;
  const outputResizeHandle = document.getElementById('output-resize-handle')!;
  let handlePointerAt = [0, 0];
  outputResizeHandle.addEventListener('pointerdown', (e) => {
    outputResizeHandle.setPointerCapture(e.pointerId);
    handlePointerAt = [e.screenX, e.screenY];
  });
  outputResizeHandle.addEventListener('pointermove', (e) => {
    if (outputResizeHandle.hasPointerCapture(e.pointerId)) {
      const outputStyle = getComputedStyle(outputContainer);
      const verticalPadding = parseFloat(outputStyle.paddingTop) + parseFloat(outputStyle.paddingBottom);
      const oldHeight = outputContainer.offsetHeight - verticalPadding;
      const oldScrollTop = outputContainer.scrollTop;
      const handlePointerWasAt = handlePointerAt;
      handlePointerAt = [e.screenX, e.screenY];
      const delta = handlePointerAt[1] - handlePointerWasAt[1];
      outputContainer.style.height = `${oldHeight - delta}px`;
      outputContainer.scrollTop = clamp(oldScrollTop + delta, 0, outputContainer.scrollHeight - outputContainer.offsetHeight);
    }
  });

  let ctrlClickedAt = 0;
  const isTryingToEngageNumberDrag = () => {
    return performance.now() - ctrlClickedAt < 100;
  };

  editorContainer.addEventListener('pointerdown', (e) => {
    if ((e.buttons === 1 || e.buttons === 2) && e.ctrlKey) {
      ctrlClickedAt = performance.now();
      editorContainer.setPointerCapture(e.pointerId);
      e.preventDefault();
    }
  });
  editorContainer.addEventListener('contextmenu', (e) => {
    if (isTryingToEngageNumberDrag()) {
      e.preventDefault();
    }
  });
  editorContainer.addEventListener('pointermove', (e) => {
    if (editorContainer.hasPointerCapture(e.pointerId)) {
      alterNumber(editor, Big(e.movementX).times('1'));
    }
  });

  // There is a bug in Firefox where ctrl-click fires as
  // a pointermove event instead of a pointerdown event,
  // and then will not respect setPointerCapture() when
  // called from the pointermove event.
  //
  // https://bugzilla.mozilla.org/show_bug.cgi?id=1504210
  //
  // So on Firefox you have to use an actual right-click.
  // It's very annoying. This is an *okay* workaround.
  document.addEventListener('pointermove', (e) => {
    if (e.shiftKey && e.metaKey) {
      alterNumber(editor, Big(e.movementX).times('1'));
    }
  });

  document.addEventListener('pagehide', (_e) => {
    save(editor);
  });
  let savedBefore = false;
  // iOS Safari doesn't support beforeunload,
  // but it does support unload.
  window.addEventListener('beforeunload', (_e) => {
    savedBefore = true;
    save(editor);
  });
  window.addEventListener('unload', (_e) => {
    if (!savedBefore) {
      save(editor);
    }
  });

  onReady(draw);
  editor.focus();
}

document.addEventListener("DOMContentLoaded", (_) => {
  const saved = localStorage.getItem(LOCAL_STORAGE_KEY);
  if (saved == null) {
    initialScript.then(initialize);
  } else {
    initialize(saved);
  }
});

window.Module = Module;
