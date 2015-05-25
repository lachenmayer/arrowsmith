page = require 'page'
ready = require 'domready'
{getProject, getModule} = require './get.coffee'
{attachToEnvironment} = require './environment.coffee'

container = document.createElement 'div'

clear = (elem) ->
  while elem.firstChild
    elem.removeChild elem.firstChild

initEditor = (elmFile) ->
  attachToEnvironment elmFile.compiledCode
  clear container

  ports =
    initialModule: elmFile.modul # : Module
    editedValue: ["", ""] # : (Name, Value)
    finishEvaluating: [[], "", [""]] # : (ModuleName, Name, ModuleName)
    compiledModules: elmFile.modul # : Module
    compileErrors: "" # : ElmError(~ String)
  editor = Elm.embed Elm.Arrowsmith.Editor, container, ports

  {editDefinition} = require('./edit.coffee')
  editor.ports.editDefinition.subscribe editDefinition(editor.ports.editedValue.send, editor.ports.compiledModules.send, editor.ports.compileErrors.send)

  {evaluate, evaluateMain} = require('./evaluate.coffee')
  editor.ports.evaluate.subscribe evaluate editor.ports.finishEvaluating.send
  editor.ports.evaluateMain.subscribe evaluateMain

initProject = ({elmFiles, projectRepo}) ->
  clear container
  ports =
    repo: projectRepo
    files: Object.keys elmFiles
    expanded: true
  Elm.embed Elm.Arrowsmith.Project, container, ports

error = (errorMessage) ->
  # server error
  if errorMessage.entity?
    errorMessage = errorMessage.entity.error
  errorDiv = document.createElement 'div'
  errorDiv.textContent = errorMessage
  document.body.appendChild errorDiv

projectRoute = ({user, project}) ->
  getProject user, project
    .then initProject
    .catch error

moduleRoute = ({user, project, module}) ->
  getModule user, project, module
    .then initEditor
    .catch error

ready ->
  document.body.appendChild container
  page '/:user/:project', ({params}) -> projectRoute params
  page '/:user/:project/:module', ({params}) -> moduleRoute params
  page()
