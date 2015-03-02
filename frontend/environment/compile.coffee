compileClient = require 'rest'
  .wrap require('rest/interceptor/mime')
  .wrap require('rest/interceptor/errorCode')
  .wrap require('rest/interceptor/defaultRequest'),
    method: 'POST'
    path: "#{document.location.pathname}/compile"

compileRequest = (elmCode) ->
  compileClient entity: elmCode

attachToEnvironment = (elmCode) ->
  document.getElementById('elm-script')?.remove()
  executionFrame = document.createElement 'iframe'
  executionFrame.id = 'elm-script'
  executionFrame.style.display = 'none'
  document.body.appendChild executionFrame

  script = document.createElement 'script'
  script.text = elmCode
  executionFrame.contentDocument.body.appendChild script

module.exports = (done) -> (elmCode) ->
  console.log 'compiling'
  console.log elmCode
  request = compileRequest elmCode
  request.then (response) ->
    attachToEnvironment response.entity.code
    done ["Ok", ""] # TODO fill with something useful?
  request.catch (response) ->
    console.log "compile: HTTP error #{response.status.code}"
    console.log response
    done ["Err", response.entity.error]