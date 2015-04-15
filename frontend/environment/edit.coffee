updateClient = require 'rest'
  .wrap require('rest/interceptor/mime')
  .wrap require('rest/interceptor/errorCode')
  .wrap require('rest/interceptor/defaultRequest'),
    method: 'POST'
    path: "#{document.location.pathname}/edit"

updateRequest = (update) ->
  updateClient entity: update

module.exports = (editedValue, moduleUpdates) -> (name) ->
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
    console.log "YES"
    console.log response
  request.catch (response) ->
    console.log "NO"
    console.log response