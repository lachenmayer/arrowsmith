ready = require 'domready'
{attachToEnvironment} = require './environment.coffee'

# ready ->
#   Elm.fullscreen Elm.Arrowsmith.ImportView, {}

ready ->
  initialModule = document.querySelector('.initial-module')
  unless initialModule?
    console.log 'expected an initial module, but none found'
    return

  elmFile = JSON.parse initialModule.innerHTML
  initialModule.parentNode.removeChild initialModule
  attachToEnvironment elmFile.compiledCode

  ports =
    initialModule: elmFile.modul # : Module
    editedValue: ["", ""] # : (Name, Value)
    finishEvaluating: [[], "", [""]] # : (ModuleName, Name, ModuleName)
    compiledModules: elmFile.modul # : Module
    compileErrors: "" # : ElmError(~ String)
  editor = Elm.fullscreen Elm.Arrowsmith.Main, ports

  {editDefinition} = require('./edit.coffee')
  editor.ports.editDefinition.subscribe editDefinition(editor.ports.editedValue.send, editor.ports.compiledModules.send, editor.ports.compileErrors.send)

  {evaluate, evaluateMain} = require('./evaluate.coffee')
  editor.ports.evaluate.subscribe evaluate editor.ports.finishEvaluating.send
  editor.ports.evaluateMain.subscribe evaluateMain
