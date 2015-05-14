ready = require 'domready'
{attachToEnvironment} = require './update.coffee'

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
    evaluatedValue: [[], "", ""] # : (ModuleName, Name, Value)
    compiledModules: elmFile.modul # : Module
    compileErrors: "" # : ElmError(~ String)
  editor = Elm.fullscreen Elm.Arrowsmith.Main, ports

  edit = require('./edit.coffee')(editor.ports.editedValue.send, editor.ports.compiledModules.send, editor.ports.compileErrors.send)
  editor.ports.stopEditing.subscribe edit

  {evaluate, evaluateMain} = require('./evaluate.coffee')

  editor.ports.evaluate.subscribe evaluate editor.ports.evaluatedValue.send
  editor.ports.evaluateMain.subscribe evaluateMain
