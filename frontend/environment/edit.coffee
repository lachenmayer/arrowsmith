{attachToEnvironment} = require './update.coffee'

updateClient = require 'rest'
  .wrap require('rest/interceptor/mime')
  .wrap require('rest/interceptor/errorCode')
  .wrap require('rest/interceptor/defaultRequest'),
    method: 'POST'
    path: "#{document.location.pathname}/edit"

updateRequest = (update) ->
  updateClient entity: update

module.exports = (editedValue, compiledModules, compileErrors) -> (name) ->
  # Send the latest value of an edit field back to Elm.
  # Currently trivial, but hopefully should be able to plug in
  # some fancier editors with this mechanism.
  editField = document.querySelector ".defname-#{name}"
  code = editField.querySelector('.definition-code').value
  editedValue [name, code]

  # Send an update to the backend.
  editAction = JSON.stringify ["ChangeDefinition", [name, code]]
  request = updateRequest editAction
  request.then (response) ->
    [editStatus, compileResponse] = response.entity
    # TODO this shouldn't be necessary (use HTTP codes to signal error).
    console.assert editStatus == "EditSuccess",
      "edit.coffee: Expected edit success message with successful response."
    [compileStatus, fileOrError] = compileResponse
    console.log compileResponse
    switch compileStatus
      when "CompileSuccess"
        attachToEnvironment fileOrError.compiledCode
        compiledModules fileOrError.modul
      when "CompileFailure"
        compileErrors fileOrError
  request.catch (response) ->
    console.error "edit.coffee: edit error"
    console.log response