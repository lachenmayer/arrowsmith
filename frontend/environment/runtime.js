var Display = {
  FULLSCREEN: 0,
  COMPONENT: 1,
  NONE: 2
};

function makeRuntime(display, container, args) {
  var inputs = [];

  /* OFFSET
   * Elm's time traveling debugger lets you pause time. This means
   * "now" may be shifted a bit into the past. By wrapping Date.now()
   * we can manage this.
   */
  var timer = {
    programStart: Date.now(),
    now: function()
    {
      return Date.now();
    }
  };

  var updateInProgress = false;
  function notify(id, v)
  {
    if (updateInProgress)
    {
      throw new Error(
        'The notify function has been called synchronously!\n' +
        'This can lead to frames being dropped.\n' +
        'Definitely report this to <https://github.com/elm-lang/Elm/issues>\n');
    }
    updateInProgress = true;
    var timestep = timer.now();
    for (var i = inputs.length; i--; )
    {
      inputs[i].notify(timestep, id, v);
    }
    updateInProgress = false;
  }
  function setTimeout(func, delay)
  {
    return window.setTimeout(func, delay);
  }

  var listeners = [];
  function addListener(relevantInputs, domNode, eventName, func)
  {
    domNode.addEventListener(eventName, func);
    var listener = {
      relevantInputs: relevantInputs,
      domNode: domNode,
      eventName: eventName,
      func: func
    };
    listeners.push(listener);
  }

  var argsTracker = {};
  for (var name in args)
  {
    argsTracker[name] = {
      value: args[name],
      used: false
    };
  }

  function swap(newModule)
  {
    removeListeners(listeners);
    var div = document.createElement('div');
    var newElm = init(display, div, newModule, args, elm);
    inputs = [];
    // elm.swap = newElm.swap;
    return newElm;
  }

  function dispose()
  {
    removeListeners(listeners);
    inputs = [];
  }

  // create the actual RTS. Any impure modules will attach themselves to this
  // object. This permits many Elm programs to be embedded per document.
  return {
    args: args,

    notify: notify,
    setTimeout: setTimeout,
    node: container,
    listeners: listeners,
    addListener: addListener,
    inputs: inputs,
    timer: timer,
    argsTracker: argsTracker,
    ports: {},

    swap: swap,
    dispose: dispose,

    isFullscreen: function() { return display === Display.FULLSCREEN; },
    isEmbed: function() { return display === Display.COMPONENT; },
    isWorker: function() { return display === Display.NONE; }
  };
}

function initWithRuntime(elm, module, moduleToReplace, fieldName, customRenderer)
{
  var Module = {};
  try
  {
    Module = module.make(elm);
    checkInputs(elm.argsTracker);
  }
  catch (error)
  {
    if (typeof elm.node.appendChild == 'undefined')
    {
      console.log(error.message);
    }
    else
    {
      elm.node.appendChild(errorNode(error.message));
    }
    throw error;
  }

  if (!elm.isWorker())
  {
    var graphicsNode = initGraphics(elm, Module, fieldName, customRenderer);
  }

  var rootNode = { kids: elm.inputs };
  trimDeadNodes(rootNode);
  inputs = rootNode.kids;
  filterListeners(elm.inputs, elm.listeners);

  addReceivers(elm.ports);

  if (typeof moduleToReplace !== 'undefined')
  {
    hotSwap(moduleToReplace, elm);

    // rerender scene if graphics are enabled.
    if (typeof graphicsNode !== 'undefined')
    {
      graphicsNode.notify(0, true, 0);
    }
  }
}

function init(display, container, module, args, moduleToReplace, fieldName, customRenderer)
{
  var elm = makeRuntime(display, container, args);
  initWithRuntime(elm, module, moduleToReplace, fieldName, customRenderer);

  return elm;
}

function checkInputs(argsTracker)
{
  for (var name in argsTracker)
  {
    if (!argsTracker[name].used)
    {
      throw new Error(
        "Port Error:\nYou provided an argument named '" + name +
        "' but there is no corresponding port!\n\n" +
        "Maybe add a port '" + name + "' to your Elm module?\n" +
        "Maybe remove the '" + name + "' argument from your initialization code in JS?"
      );
    }
  }
}

function errorNode(message)
{
  var code = document.createElement('code');

  var lines = message.split('\n');
  code.appendChild(document.createTextNode(lines[0]));
  code.appendChild(document.createElement('br'));
  code.appendChild(document.createElement('br'));
  for (var i = 1; i < lines.length; ++i)
  {
    code.appendChild(document.createTextNode('\u00A0 \u00A0 ' + lines[i].replace(/  /g, '\u00A0 ')));
    code.appendChild(document.createElement('br'));
  }
  code.appendChild(document.createElement('br'));
  code.appendChild(document.createTextNode("Open the developer console for more details."));
  return code;
}


//// FILTER SIGNALS ////

// TODO: move this code into the signal module and create a function
// Signal.initializeGraph that actually instantiates everything.

function filterListeners(inputs, listeners)
{
  loop:
  for (var i = listeners.length; i--; )
  {
    var listener = listeners[i];
    for (var j = inputs.length; j--; )
    {
      if (listener.relevantInputs.indexOf(inputs[j].id) >= 0)
      {
        continue loop;
      }
    }
    listener.domNode.removeEventListener(listener.eventName, listener.func);
  }
}

function removeListeners(listeners)
{
  for (var i = listeners.length; i--; )
  {
    var listener = listeners[i];
    listener.domNode.removeEventListener(listener.eventName, listener.func);
  }
}

// add receivers for built-in ports if they are defined
function addReceivers(ports)
{
  if ('title' in ports)
  {
    if (typeof ports.title === 'string')
    {
      document.title = ports.title;
    }
    else
    {
      ports.title.subscribe(function(v) { document.title = v; });
    }
  }
  if ('redirect' in ports)
  {
    ports.redirect.subscribe(function(v) {
      if (v.length > 0)
      {
        window.location = v;
      }
    });
  }
}


// returns a boolean representing whether the node is alive or not.
function trimDeadNodes(node)
{
  if (node.isOutput)
  {
    return true;
  }

  var liveKids = [];
  for (var i = node.kids.length; i--; )
  {
    var kid = node.kids[i];
    if (trimDeadNodes(kid))
    {
      liveKids.push(kid);
    }
  }
  node.kids = liveKids;

  return liveKids.length > 0;
}


////  RENDERING  ////

// customRenderer needs to be an instantiated Elm module with a function called "view".
// ie. somewhere, SomeModule.make(elm) should have been called.
function initGraphics(elm, Module, fieldName, customRenderer)
{
  // Enables rendering of any value, not just Element.
  function makeElement(scene) {
    var view;
    if (typeof customRenderer !== 'undefined')
    {
      view = customRenderer.view;
    }
    else
    {
      var Element = Elm.Graphics.Element.make(elm);
      view = Element.show;
    }
    return view(scene);
  }

  // The default behaviour is to render "main".
  if (typeof fieldName == 'undefined')
  {
    fieldName = 'main';
  }
  var signalGraph = Module[fieldName];

  // make sure the signal graph is actually a signal & extract the visual model
  if ((typeof signalGraph !== 'object') || !('notify' in signalGraph))
  {
    signalGraph = Elm.Signal.make(elm).constant(signalGraph);
  }
  var initialScene = signalGraph.value;

  // The value being rendered is not actually an element - make it one.
  if (!initialScene.props && !initialScene.tagName && !initialScene.element) {
    initialScene = makeElement(initialScene);
  }

  // Figure out what the render functions should be
  var render;
  var update;
  if (initialScene.props)
  {
    var Element = Elm.Native.Graphics.Element.make(elm);
    render = Element.render;
    update = Element.updateAndReplace;
  }
  else
  {
    var VirtualDom = Elm.Native.VirtualDom.make(elm);
    render = VirtualDom.render;
    update = VirtualDom.updateAndReplace;
  }

  // Add the initialScene to the DOM
  var container = elm.node;
  var node = render(initialScene);
  while (container.firstChild)
  {
    container.removeChild(container.firstChild);
  }
  container.appendChild(node);

  var _requestAnimationFrame =
    typeof requestAnimationFrame !== 'undefined'
      ? requestAnimationFrame
      : function(cb) { setTimeout(cb, 1000/60); }
      ;

  // domUpdate is called whenever the main Signal changes.
  //
  // domUpdate and drawCallback implement a small state machine in order
  // to schedule only 1 draw per animation frame. This enforces that
  // once draw has been called, it will not be called again until the
  // next frame.
  //
  // drawCallback is scheduled whenever
  // 1. The state transitions from PENDING_REQUEST to EXTRA_REQUEST, or
  // 2. The state transitions from NO_REQUEST to PENDING_REQUEST
  //
  // Invariants:
  // 1. In the NO_REQUEST state, there is never a scheduled drawCallback.
  // 2. In the PENDING_REQUEST and EXTRA_REQUEST states, there is always exactly 1
  //    scheduled drawCallback.
  var NO_REQUEST = 0;
  var PENDING_REQUEST = 1;
  var EXTRA_REQUEST = 2;
  var state = NO_REQUEST;
  var savedScene = initialScene;
  var scheduledScene = initialScene;

  function domUpdate(newScene)
  {
    scheduledScene = makeElement(newScene);

    switch (state)
    {
      case NO_REQUEST:
        _requestAnimationFrame(drawCallback);
        state = PENDING_REQUEST;
        return;
      case PENDING_REQUEST:
        state = PENDING_REQUEST;
        return;
      case EXTRA_REQUEST:
        state = PENDING_REQUEST;
        return;
    }
  }

  function drawCallback()
  {
    switch (state)
    {
      case NO_REQUEST:
        // This state should not be possible. How can there be no
        // request, yet somehow we are actively fulfilling a
        // request?
        throw new Error(
          "Unexpected draw callback.\n" +
          "Please report this to <https://github.com/elm-lang/core/issues>."
        );

      case PENDING_REQUEST:
        // At this point, we do not *know* that another frame is
        // needed, but we make an extra request to rAF just in
        // case. It's possible to drop a frame if rAF is called
        // too late, so we just do it preemptively.
        _requestAnimationFrame(drawCallback);
        state = EXTRA_REQUEST;

        // There's also stuff we definitely need to draw.
        draw();
        return;

      case EXTRA_REQUEST:
        // Turns out the extra request was not needed, so we will
        // stop calling rAF. No reason to call it all the time if
        // no one needs it.
        state = NO_REQUEST;
        return;
    }
  }

  function draw()
  {
    update(elm.node.firstChild, savedScene, scheduledScene);
    if (elm.Native.Window)
    {
      elm.Native.Window.values.resizeIfNeeded();
    }
    savedScene = scheduledScene;
  }

  var renderer = Elm.Native.Signal.make(elm).output(fieldName, domUpdate, signalGraph);

  // must check for resize after 'renderer' is created so
  // that changes show up.
  if (elm.Native.Window)
  {
    elm.Native.Window.values.resizeIfNeeded();
  }

  return renderer;
}

//// HOT SWAPPING ////

// Returns boolean indicating if the swap was successful.
// Requires that the two signal graphs have exactly the same
// structure.
function hotSwap(from, to)
{
  function similar(nodeOld,nodeNew)
  {
    if (nodeOld.id !== nodeNew.id)
    {
      return false;
    }
    if (nodeOld.isOutput)
    {
      return nodeNew.isOutput;
    }
    return nodeOld.kids.length === nodeNew.kids.length;
  }
  function swap(nodeOld,nodeNew)
  {
    nodeNew.value = nodeOld.value;
    return true;
  }
  var canSwap = depthFirstTraversals(similar, from.inputs, to.inputs);
  if (canSwap)
  {
    depthFirstTraversals(swap, from.inputs, to.inputs);
  }
  from.node.parentNode.replaceChild(to.node, from.node);

  return canSwap;
}

// Returns false if the node operation f ever fails.
function depthFirstTraversals(f, queueOld, queueNew)
{
  if (queueOld.length !== queueNew.length)
  {
    return false;
  }
  queueOld = queueOld.slice(0);
  queueNew = queueNew.slice(0);

  var seen = [];
  while (queueOld.length > 0 && queueNew.length > 0)
  {
    var nodeOld = queueOld.pop();
    var nodeNew = queueNew.pop();
    if (seen.indexOf(nodeOld.id) < 0)
    {
      if (!f(nodeOld, nodeNew))
      {
        return false;
      }
      queueOld = queueOld.concat(nodeOld.kids || []);
      queueNew = queueNew.concat(nodeNew.kids || []);
      seen.push(nodeOld.id);
    }
  }
  return true;
}

module.exports = {
  Display: Display,
  makeRuntime: makeRuntime,
  // init: function(elm, module, fieldName, customRenderer) {
  //   return initWithRuntime(elm, module, undefined /* moduleToReplace */, fieldName, customRenderer);
  // }
  run: function(container, module, args, moduleToReplace, fieldName, customRenderer) {
    return init(Display.COMPONENT, container, module, args, moduleToReplace, fieldName, customRenderer);
  }
}