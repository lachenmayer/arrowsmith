{attachToEnvironment} = require './update.coffee'
Promise = require 'bluebird'

newDefinitionName = '__arrowsmithNewDefinition__'

updateClient = require 'rest'
  .wrap require('rest/interceptor/mime')
  .wrap require('rest/interceptor/errorCode')
  .wrap require('rest/interceptor/defaultRequest'),
    method: 'POST'
    path: "#{document.location.pathname}/edit"

updateRequest = (update) ->
  updateClient entity: update

sendUpdateRequest = (editAction) ->
  requestBody = JSON.stringify editAction
  request = updateRequest requestBody
  request.then (response) ->
    [editStatus, compileResponse] = response.entity
    # TODO this shouldn't be necessary (use HTTP codes to signal error).
    console.assert editStatus == 'EditSuccess',
      'edit.coffee: Expected edit success message with successful response.'
    compileResponse

handleCompileResponse = ({compiledModules, compileErrors}) ->
  ([compileStatus, fileOrError]) ->
    switch compileStatus
      when 'CompileSuccess'
        attachToEnvironment fileOrError.compiledCode
        compiledModules fileOrError.modul
      when 'CompileFailure'
        compileErrors fileOrError

update = (action, ports) ->
  request = sendUpdateRequest action
  request.then handleCompileResponse ports
  request.catch (response) ->
    console.error 'edit.coffee: edit error'
    console.log response

addDefinition = (ports) ->
  editField = document.querySelector '.new-definition'
  code = editField.querySelector('.definition-code').value

  update ['AddDefinition', [newDefinitionName, null, code]], ports

changeDefinition = (name, ports) ->
  editField = document.querySelector ".defname-#{name}"
  code = editField.querySelector('.definition-code').value
  ports.editedValue [name, code]

  update ['ChangeDefinition', [name, code]], ports

module.exports = (editedValue, compiledModules, compileErrors) -> (name) ->
  ports = {editedValue, compiledModules, compileErrors}
  if name is newDefinitionName
    addDefinition ports
  else
    changeDefinition name, ports