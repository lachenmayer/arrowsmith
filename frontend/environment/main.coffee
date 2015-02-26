ports =
  editorValue: ["", ""] # : (Name, Value)
  evaluatedValue: [[], "", ""] # : (ModuleName, Name, Value)
  compileResponse: ["Ok", ""] # : Result ElmError ElmCode (~ish)

editor = Elm.fullscreen Elm.Arrowsmith.Main, ports
editor.ports.compileProgram.subscribe require('./compile.coffee')(editor.ports.compileResponse.send)
editor.ports.stopEditing.subscribe require('./edit.coffee')(editor.ports.editorValue.send)
editor.ports.evaluate.subscribe require('./evaluate.coffee')(editor.ports.evaluatedValue.send)