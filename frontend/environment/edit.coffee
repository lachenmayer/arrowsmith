{update} = require './update.coffee'

newDefinitionName = '__arrowsmithNewDefinition__'

addDefinition = (ports) ->
  editField = document.querySelector '.new-definition'
  code = editField.querySelector('.definition-code').value
  update ['AddDefinition', [newDefinitionName, null, code]], ports

changeDefinition = (name, ports) ->
  editField = document.querySelector ".defname-#{name}"
  code = editField.querySelector('.definition-code').value
  ports.editedValue [name, code]
  update ['ChangeDefinition', [name, code]], ports

editDefinition = (editedValue, compiledModules, compileErrors) -> (name) ->
  ports = {editedValue, compiledModules, compileErrors}
  if name is newDefinitionName
    addDefinition ports
  else
    changeDefinition name, ports

editImport = (editedValue, compiledModules, compileErrors) -> (import_) ->
  console.log import_

module.exports =
  {editDefinition}
