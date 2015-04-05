ready = require 'domready'
{attachToEnvironment} = require './utils.coffee'

ready ->
  initialModule = document.querySelector('.initial-module')
  unless initialModule?
    console.log 'expected an initial module, but none found'
    return

  elmFile = JSON.parse initialModule.innerHTML
  initialModule.parentNode.removeChild initialModule

  console.log elmFile.compiledCode.length

  attachToEnvironment elmFile.compiledCode

  ports =
    initialModule: elmFile.modul # : Module
    moduleUpdates: [elmFile.modul, ["Ok", ""]] # : (Module, Result Error ElmCode)
    editedValue: ["", ""] # : (Name, Value)
    evaluatedValue: [[], "", ""] # : (ModuleName, Name, Value)
    compileResponse: ["Ok", ""] # : Result ElmError ElmCode (~ish)
  editor = Elm.fullscreen Elm.Arrowsmith.Main, ports

  # editor.ports.compileModule.subscribe require('./compile.coffee')(editor.ports.compileResponse.send)
  editor.ports.stopEditing.subscribe require('./edit.coffee')(editor.ports.editedValue.send)
  editor.ports.evaluate.subscribe require('./evaluate.coffee')(editor.ports.evaluatedValue.send)