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

/**
 * GLSL Parser Class
 */
function GlslParser() {
	
	//Jison Global
	this.jison = parser;
	this.jison.lexer = lexer;
}

var proto = GlslParser.prototype;

/**
 * Parse Program
 */
proto.parse = function(state) {
	var result;

	this.jison.yy =  {
		test : 1,
		state : state
	};

	try {
		this.jison.parse(state.getTranslationUnit());
	} catch(e) {
		state.addError(e.message, e.lineNumber, e.columnNumber);
		return false;
	}

	return true;
};

glsl.parser = new GlslParser();



/**
 * External Parse
 *
 * @param   string   src        Source code
 * @param   object   options    Compilation options
 *
 * @return  object
 */
glsl.parse = function(src, options) {
	var state,
		result,
		irs
		;

	state = new GlslState(options);
	state.setSource(src);
	this.fire('init', [state]);

	//Preprocess
	result = this.preprocessor.process(state, options);
	this.fire('preprocess', [state]);

	//Parse into AST
	if (result) {
		result = this.parser.parse(state);
		this.fire('parse', [state]);
	}

	if (result) {
		state.status = true;	
		this.fire('complete', [state]);
	} else {
		this.fire('fail', [state]);
	}

	return state;
};

