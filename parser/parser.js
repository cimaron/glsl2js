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

function GlslState(options) {
	var i;

	this.options = {
		target : 0,
		language_version : 100,
	};

	for (i in options) {
		this.options[i] = options[i];	
	}

	this.symbols = new SymbolTable();
	symbol_table_init(this.symbols, options.target);

	this.status = false;
	this.translation_unit = "";
	this.ast = [];
	this.ir = null;

	this.errors = [];
}

proto = GlslState.prototype = {};

/**
 * Get identifier type
 *
 * @param   object   state   GLSL state
 * @param   string   name    Identifier name
 * Add error to state
 *
 * @return  string
 * @param   string   msg      Message
 * @param   int      line     Message
 * @param   int      column   Message
 */
proto.classify_identifier = function(name) {
	if (this.symbols.get_variable(name) || this.symbols.get_function(name)) {
		return 'IDENTIFIER';
	} else if (this.symbols.get_type(name)) {
		return 'TYPE_IDENTIFIER';
	} else {
		return 'NEW_IDENTIFIER';
	}
};

proto.setSource = function(src) {
	this.src = src;
};

proto.getSource = function() {
	return this.src;
};

proto.setTranslationUnit = function(tu) {
	this.translation_unit = tu;	
};

proto.getTranslationUnit = function() {
	return this.translation_unit;
};

proto.addAstNode = function(node) {
	this.ast.push(node);
};

proto.getAst = function() {
	return this.ast;
};

proto.setIR = function(ir) {
	this.ir = ir;
};

proto.getIR = function() {
	return this.ir;
};

proto.getStatus = function() {
	return this.status;
};


/**
 * Add error to state
 *
 * @param   string   msg      Message
 * @param   int      line     Message
 * @param   int      column   Message
 */
proto.addError = function(msg, line, column) {
	var err;

	err = util.format("%s at line %s, column %s", msg, line, column);

	this.errors.push(err);
};

/**
 * Get compile errors
 *
 * @return  mixed
 */
proto.getErrors = function() {
	return this.errors;
}


/**
 * Jison parser compatibility
 */
glsl.parse = function(state) {
	var result;

	parser.yy =  {
		test : 1,
		state : state
	};

	try {
		parser.parse(state.getTranslationUnit());
	} catch(e) {
		state.addError(e.message, e.lineNumber, e.columnNumber);
		return false;
	}

	return true;
};
	
