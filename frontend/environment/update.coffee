# Update the environment, usually based on newly compiled code.
Promise = require 'bluebird'
{attachToEnvironment} = require './environment.coffee'

updateClient = require 'rest'
  .wrap require('rest/interceptor/mime')
  .wrap require('rest/interceptor/errorCode')
  .wrap require('rest/interceptor/defaultRequest'),
    method: 'POST'
    path: "/api/github#{document.location.pathname}" # TODO properly

updateRequest = (update) ->
  updateClient entity: update

sendUpdateRequest = (editAction) ->
  requestBody = JSON.stringify editAction
  request = updateRequest requestBody
  request.then (response) ->
    [editStatus, compileResponse] = response.entity
    # TODO this shouldn't be necessary (use HTTP codes to signal error).
    console.assert editStatus == 'EditSuccess',
      "edit.coffee: Expected edit success message with successful response. Instead got: #{response.entity}"
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

module.exports =
  {update}
