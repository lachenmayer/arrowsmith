freshContext = ->
  inputs: []
  addListener: ->
  node: {}

# TODO ghetto duck-typing
isElement = (x) ->
  x.element? and x.props?

appendToDefinition = (defName, valueNode) ->
  def = document.querySelector ".defname-#{defName}"

  # Remove an older value if it exists.
  def.querySelector('.definition-value')?.remove()

  container = document.createElement 'div'
  container.className = 'definition-value'
  container.appendChild valueNode
  def.appendChild container

module.exports = (done) -> ([moduleName, name]) ->
  executionFrame = document.getElementById 'elm-script'
  unless executionFrame?
    console.log 'evaluate: execution frame does not exist'
    return # TODO error handling...

  context = freshContext()
  toString = executionFrame.contentWindow.Elm.Native.Show.make(context).toString
  show = executionFrame.contentWindow.Elm.Graphics.Element.make(context).show
  render = executionFrame.contentWindow.Elm.Native.Graphics.Element.make(context).render
  view = (x) -> if isElement x then x else show x

  modul = executionFrame.contentWindow.Elm
  for nameSegment in moduleName
    modul = modul[nameSegment]
    # TODO error handling...
    if not modul?
      console.log "evaluate: undefined name #{moduleName}"
  modul = modul.make context

  renderedNode = render view modul[name]
  appendToDefinition name, renderedNode

  result = [moduleName, name, toString modul[name]]
  console.log "evaluate: #{result[1]}: #{result[2]}"
  done result
