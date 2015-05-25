get = require 'rest'
  .wrap require('rest/interceptor/mime')
  .wrap require('rest/interceptor/errorCode')
  .wrap require('rest/interceptor/defaultRequest'),
    method: 'GET'

getEntity = (url) ->
  get path: url
    .then ({entity}) -> entity

getProject = (user, project) ->
  getEntity "/api/github/#{user}/#{project}"

getModule = (user, project, module) ->
  getEntity "/api/github/#{user}/#{project}/#{module}"

module.exports =
  {getProject, getModule}
