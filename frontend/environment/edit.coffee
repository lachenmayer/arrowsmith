# Send the latest value of an edit field back to Elm.
# Currently trivial, but hopefully should be able to plug in some fancier editors with this mechanism.
module.exports = (done) -> ->
  editField = document.querySelector '.editing'
  name = editField.querySelector('.definition-name').innerText
  code = editField.querySelector('.definition-code').innerText
  console.log "edit: [#{name}, #{code}]"
  done [name, code]
