ready = require 'domready'

ready ->
  initialProgram = document.querySelector('.initial-program')
  unless initialProgram?
    console.log 'expected an initial program, but none found'
    return

  program = JSON.parse initialProgram.innerHTML
  initialProgram.parentNode.removeChild initialProgram

  console.log program

  ports =
    initialProgram: program
    editedValue: ["", ""] # : (Name, Value)
    evaluatedValue: [[], "", ""] # : (ModuleName, Name, Value)
    compileResponse: ["Ok", ""] # : Result ElmError ElmCode (~ish)
  editor = Elm.fullscreen Elm.Arrowsmith.Main, ports

  editor.ports.compileProgram.subscribe require('./compile.coffee')(editor.ports.compileResponse.send)
  editor.ports.stopEditing.subscribe require('./edit.coffee')(editor.ports.editedValue.send)
  editor.ports.evaluate.subscribe require('./evaluate.coffee')(editor.ports.evaluatedValue.send)