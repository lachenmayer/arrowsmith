{Display, makeRuntime, run} = require './runtime.js'

get = (parent, nameList) ->
  child = parent
  for nameSegment in nameList
    child = child[nameSegment]
    if not child?
      console.error "evaluate: undefined name #{nameList} in object: ", parent
  child

appendToDefinition = (defName, valueNode) ->
  def = document.querySelector ".defname-#{defName}"

  # Remove an older value if it exists.
  def.querySelector('.definition-value')?.remove()

  container = document.createElement 'div'
  container.className = 'definition-value'
  container.appendChild valueNode
  def.appendChild container

makeView = (resultNode, viewName) ->
  viewRuntime = makeRuntime Display.COMPONENT, resultNode, {}
  ViewModule = get Elm, viewName
  ViewModule.make viewRuntime

cachedModule = undefined

evaluate = (done) -> ([moduleName, names]) ->
  console.log "evaluate", names
  if names.length is 0
    console.log "nothing to evaluate"
    return
  executionFrame = document.getElementById 'elm-script'
  unless executionFrame?
    console.log 'evaluate: execution frame does not exist'
    return # TODO error handling...

  modul = get executionFrame.contentWindow.Elm, moduleName

  for [name, viewName] in names
    console.log name, viewName
    resultNode = document.createElement 'div'
    view = makeView resultNode, viewName

    # try to hotswap with the old view - won't work if not an element
    # try
    #   cachedModule = run resultNode, modul, {}, cachedModule, name, view
    # catch e
    #   console.log e
    cachedModule = run resultNode, modul, {}, undefined, name, view

    appendToDefinition name, resultNode
    result = [moduleName, name, viewName]
    console.log "evaluate: #{result[1]}: #{result[2]}"
    done result

evaluateMain = (moduleName) ->
  executionFrame = document.getElementById 'elm-script'
  unless executionFrame?
    console.log 'evaluate: execution frame does not exist'
    return # TODO error handling...

  resultFrame = document.createElement 'iframe'
  resultFrame.className = 'module-result'
  resultFrame.style = "width: 100%;"

  moduleDiv = document.querySelector ".module-#{moduleName.join "-"}"
  moduleDiv.querySelector("iframe")?.remove()
  moduleDiv.appendChild resultFrame

  script = document.createElement 'script'
  script.text = executionFrame.contentDocument.querySelector('script').text
  resultFrame.contentDocument.body.appendChild script

  modul = get resultFrame.contentWindow.Elm, moduleName
  resultFrame.contentWindow.Elm.fullscreen modul, {}

  resultFrame.height = resultFrame.contentDocument.body.scrollHeight

  console.log "evaluateMain done"

module.exports =
  {evaluate, evaluateMain}