freshContext = ->
  inputs: []
  addListener: ->
  node: {}

module.exports = (done) -> ([moduleName, name]) ->
  executionFrame = document.getElementById 'elm-script'
  unless executionFrame?
    console.log 'evaluate: execution frame does not exist'
    return # TODO error handling...

  context = freshContext()

  # TODO this is where "show" logic is going to go
  show = executionFrame.contentWindow.Elm.Native.Show.make(context).toString
  
  modul = executionFrame.contentWindow.Elm
  for nameSegment in moduleName
    modul = modul[nameSegment]
    # TODO error handling...
    if not modul?
      console.log "evaluate: undefined name #{moduleName}"
  modul = modul.make context
  
  result = [moduleName, name, show modul[name]]
  console.log "evaluate: #{result[1]}: #{result[2]}"
  done result
