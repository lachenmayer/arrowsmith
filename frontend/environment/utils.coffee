attachToEnvironment = (compiledCode) ->
  document.getElementById('elm-script')?.remove()
  executionFrame = document.createElement 'iframe'
  executionFrame.id = 'elm-script'
  executionFrame.style.display = 'none'
  document.body.appendChild executionFrame

  script = document.createElement 'script'

  executionFrame.contentDocument.body.appendChild script

  maxTextLength = 10000
  for i in [0..Math.floor(compiledCode.length / maxTextLength)]
    chunk = compiledCode.substr (i * maxTextLength), maxTextLength
    chunkNode = document.createTextNode chunk
    script.appendChild chunkNode


module.exports =
  attachToEnvironment: attachToEnvironment
