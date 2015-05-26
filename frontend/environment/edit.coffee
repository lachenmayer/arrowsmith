{update} = require './update.coffee'

newDefinitionName = '__arrowsmithNewDefinition__'

addDefinition = (editedValue, compiledElmFiles) ->
  editField = document.querySelector '.new-definition'
  code = editField.querySelector('.definition-code').value
  update ['AddDefinition', [newDefinitionName, null, code]], compiledElmFiles

changeDefinition = (name, editedValue, compiledElmFiles) ->
  editField = document.querySelector ".defname-#{name}"
  code = editField.querySelector('.definition-code').value
  # TODO: is this necessary?
  # editedValue [name, code]
  update ['ChangeDefinition', [name, code]], compiledElmFiles

editDefinition = (editedValue, compiledElmFiles) -> (name) ->
  if name is newDefinitionName
    addDefinition editedValue, compiledElmFiles
  else
    changeDefinition name, editedValue, compiledElmFiles

editText = (compiledElmFiles) -> ->
  code = document.querySelector('.module-code').value
  update ['ReplaceText', code], compiledElmFiles

editImport = (editedValue, compiledElmFiles) -> (import_) ->
  console.log import_

module.exports =
  {editDefinition, editText}
