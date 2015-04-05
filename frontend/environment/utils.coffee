attachToEnvironment = (compiledCode) ->
  document.getElementById('elm-script')?.remove()
  executionFrame = document.createElement 'iframe'
  executionFrame.id = 'elm-script'
  executionFrame.style.display = 'none'
  document.body.appendChild executionFrame

  script = document.createElement 'script'
  script.text = compiledCode

  executionFrame.contentDocument.body.appendChild script

module.exports =
  attachToEnvironment: attachToEnvironment
