gulp = require 'gulp'
coffee = require 'gulp-coffee'
run = require 'gulp-run'
stylus = require 'gulp-stylus'
nib = require 'nib'
webpack = require 'gulp-webpack'

editorDir = 'frontend/editor'
environmentDir = 'frontend/environment'
stylesDir = 'frontend/styles'
out = 'frontend/bin'

gulp.task 'editor', ->
  gulp.src ''
    .pipe run "elm-make #{editorDir}/Arrowsmith/Main.elm --output #{out}/editor.js"

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
      use: nib()
    .pipe gulp.dest out

gulp.task 'default', ['editor', 'environment', 'styles'] , ->
  gulp.watch "#{editorDir}/**/*.elm", ['editor']
  gulp.watch "#{environmentDir}/*.coffee", ['environment']
  gulp.watch "#{stylesDir}/*.styl", ['styles']
 