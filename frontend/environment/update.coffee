updateClient = require 'rest'
  .wrap require('rest/interceptor/mime')
  .wrap require('rest/interceptor/errorCode')
  .wrap require('rest/interceptor/defaultRequest'),
    method: 'POST'
    path: "#{document.location.pathname}/update"

updateRequest = (update) ->
  updateClient entity: update

module.exports = (done) -> (update) ->
  console.log 'updating'
  console.log update
  request = updateRequest update
  request.then (response) ->
    attachToEnvironment response.entity.code
    done ["Ok", ""] # TODO fill with something useful?
  request.catch (response) ->
    console.log "update: HTTP error #{response.status.code}"
    console.log response
    done ["Err", response.entity.error]
