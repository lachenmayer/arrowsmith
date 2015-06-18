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
    initialElmFile: elmFile # : ElmFile
    finishEvaluating: [[], "", [""]] # : (ModuleName, Name, ModuleName)
    compiledElmFiles: elmFile # : Signal ElmFile
  editor = Elm.embed Elm.Arrowsmith.Editor, container, ports

  {editDefinition, editText} = require './edit.coffee'
  editor.ports.editDefinition.subscribe editDefinition editor.ports.compiledElmFiles.send
  editor.ports.editText.subscribe editText editor.ports.compiledElmFiles.send

  {evaluate, evaluateMain, stopEvaluating} = require './evaluate.coffee'
  editor.ports.evaluate.subscribe evaluate editor.ports.finishEvaluating.send
  editor.ports.evaluateMain.subscribe evaluateMain
  editor.ports.stopEvaluating.subscribe stopEvaluating

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
    .catch error
    .then initProject

moduleRoute = ({user, project, module}) ->
  getModule user, project, module
    .catch error
    .then initEditor

ready ->
  document.body.appendChild container
  page '/:user/:project', ({params}) -> projectRoute params
  page '/:user/:project/:module', ({params}) -> moduleRoute params
  page()
