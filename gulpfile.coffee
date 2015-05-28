gulp = require 'gulp'
run = require 'gulp-run'
stylus = require 'gulp-stylus'
nib = require 'nib'
webpack = require 'gulp-webpack'

editorDir = 'frontend/editor'
environmentDir = 'frontend/environment'
stylesDir = 'frontend/styles'
out = 'frontend/bin'

gulp.task 'editor', ->
  gulp.src('')
    .pipe run "./.cabal-sandbox/bin/elm-make --yes #{editorDir}/Arrowsmith/Editor.elm #{editorDir}/Arrowsmith/Project.elm --output #{out}/editor.js"

gulp.task 'environment', ->
  gulp.src "#{environmentDir}/boot.js"
    .pipe webpack
      module:
        loaders: [
          test: /\.coffee$/
          loader: 'coffee'
        ]
      output:
        filename: 'env.js'
    .pipe gulp.dest out

gulp.task 'styles', ->
  gulp.src "#{stylesDir}/*.styl"
    .pipe stylus
      'include css': true
      use: nib()
    .pipe gulp.dest out

gulp.task 'default', ['editor', 'environment', 'styles'] , ->
  gulp.watch "#{editorDir}/**/*.elm", ['editor']
  gulp.watch "#{environmentDir}/*.coffee", ['environment']
  gulp.watch "#{environmentDir}/*.js", ['environment']
  gulp.watch "#{stylesDir}/*.styl", ['styles']
