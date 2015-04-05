/*
Copyright (c) 2011 Cimaron Shanahan

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
var del = require('del'),
	gulp = require('gulp'),
	browserify = require('gulp-browserify'),
	concat = require('gulp-concat'),
	include = require('gulp-include'),
	jison = require('gulp-jison'),
	jisonlex = require('gulp-jison-lex'),
	jshint = require('gulp-jshint'),
	rename = require('gulp-rename'),
	uglify = require('gulp-uglify')
	;

gulp.task('clean', function(cb) {
    del(['build/*'], cb);
});

/**
 * Component Tasks
 */
gulp.task('jison-lex', function() {
	return gulp.src('parser/*.jisonlex')
		.pipe(jisonlex({
			outFile : 'lexer.js'
		}))
		//.pipe(rename('lexer.js'))
		.pipe(gulp.dest('build'))
});

gulp.task('jison', function() {
	return gulp.src('parser/*.jison')
		.pipe(jison({ moduleType: 'commonjs' }))
		//.pipe(rename('grammar.js'))
		.pipe(gulp.dest('build'))
});

gulp.task('_parser', ['jison', 'jison-lex'], function() {
	return gulp.src([
		'preprocessor/preprocessor.js',
		'preprocessor/comments.js',
		'preprocessor/directives.js',
		'parser/type.js',
		'parser/symbol.js',
		'parser/ast.js',
		'ir/builtin.js',
		'build/grammar.js',
		'build/lexer.js',
		'parser/parser.js',
		'extensions/**/*.js'
		])
		.pipe(concat('parser.js'))
		.pipe(gulp.dest('build'))
});

gulp.task('generator', function() {
	return gulp.src([
		'ir/generator.js',	
		'ir/ir.js',
		'ir/instruction.js',
		'output/program.js',
		'output/**/*.js'
		])
		.pipe(concat('ir.js'))
		.pipe(gulp.dest('build'))
});

/**
 * Build
 */
gulp.task('glsl-all', ['_parser', 'generator'], function() {
	return gulp.src([
		'glsl.js',
		'library/util.js',
		'event.js',
		'state.js',
		'build/parser.js',
		'build/ir.js'
		])
		.pipe(concat('glsl.part.js'))
		.pipe(gulp.dest('build'))
});


gulp.task('glsl-parser', ['_parser'], function() {
	return gulp.src([
		'glsl.js',
		'library/util.js',
		'event.js',
		'state.js',
		'build/parser.js'
		])
		.pipe(concat('glsl.part.js'))
		.pipe(gulp.dest('build'))
});

gulp.task('errors', ['glsl-all'], function() {
	return gulp.src([
		'build/glsl.js'
	])
	.pipe(jshint())
	;
});

/**
 * Final processing
 */
gulp.task('default', ['clean', 'glsl-all'], function() {

	return gulp.src([
		'index.js'
		])
		.pipe(include())
		/*.pipe(uglify({
			mangle : false,
			output : {
				beautify : true
			},
			compress : false,
			preserveComments : function(node, comment) {
				return !comment.value.match(/Copyright/);
			}
		}))*/
		/*.pipe(uglify({
			mangle : true,
			output : {
				beautify : false
			},
			compress : true
		}))*/
		.pipe(rename('glsl.js'))
		.pipe(gulp.dest('build'))
		;
});

gulp.task('parser', ['clean', 'glsl-parser'], function() {
	return gulp.src([
		'index.js'
		])
		.pipe(include())
		.pipe(rename('glsl.parser.js'))
		.pipe(gulp.dest('build'))
		;
});

/**
 * Watch
 */
gulp.task('watch', function() {
    gulp.watch([
        '*.js',
        'library/*.js',
        'ir/*.js',
        'output/**/*.js',
        'parser/*.js',
        'parser/*.jison',
        'preprocessor/*.js',
		'extensions/**/*.js'
    ], ['default']);
});

gulp.task('watch-parser', function() {
    gulp.watch([
        '*.js',
        'library/*.js',
        'parser/*.js',
        'parser/*.jison',
        'preprocessor/*.js',
		'extensions/**/*.js'
    ], ['parser']);
});


