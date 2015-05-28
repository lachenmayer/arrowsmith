{update} = require './update.coffee'

newDefinitionName = '__arrowsmithNewDefinition__'

addDefinition = (compiledElmFiles) ->
  editField = document.querySelector '.new-definition'
  code = editField.querySelector('.definition-code').value
  update ['AddDefinition', [newDefinitionName, null, code]], compiledElmFiles

changeDefinition = (name, compiledElmFiles) ->
  editField = document.querySelector ".defname-#{name}"
  code = editField.querySelector('.definition-code').value
  update ['ChangeDefinition', [name, code]], compiledElmFiles

editDefinition = (compiledElmFiles) -> (name) ->
  if name is newDefinitionName
    addDefinition compiledElmFiles
  else
    changeDefinition name, compiledElmFiles

editText = (compiledElmFiles) -> ->
  code = document.querySelector('.module-code').value
  update ['ReplaceText', code], compiledElmFiles

editImport = (editedValue, compiledElmFiles) -> (import_) ->
  console.log import_

module.exports =
  {editDefinition, editText}
