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

(function() {
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

var glsl = {
	
	state : null,
	
	error : [],

	compile : function(source, options) {
		var processed,
		    state,
		    irs
		    ;

		//Preprocess
		processed = this.preprocessor.process(source);
		if (!processed) {
			this.error = this.preprocessor.error;
			return false;
		}

		state = this.parse(processed, options);
		if (state.error) {
			this.error = state.info_log;
			return false;
		}

		this.state = state;
		irs = this.generate(this.state);
		if (!irs) {
			this.error = this.state.info_log;	
		}

		return irs;
	},

	getLastError : function() {
		return this.error;
	},

	/**
	 * Compilation targets
	 */
	target : {
		fragment : 0,
		vertex : 1
	}
};


// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

/**
 * Select node.js util functions
 */

var util = {};

(function(exports) {


var formatRegExp = /%[sdj%]/g;
exports.format = function(f) {
  if (!isString(f)) {
    var objects = [];
    for (var i = 0; i < arguments.length; i++) {
      objects.push(inspect(arguments[i]));
    }
    return objects.join(' ');
  }

  var i = 1;
  var args = arguments;
  var len = args.length;
  var str = String(f).replace(formatRegExp, function(x) {
    if (x === '%%') return '%';
    if (i >= len) return x;
    switch (x) {
      case '%s': return String(args[i++]);
      case '%d': return Number(args[i++]);
      case '%j':
        try {
          return JSON.stringify(args[i++]);
        } catch (_) {
          return '[Circular]';
        }
      default:
        return x;
    }
  });
  for (var x = args[i]; i < len; x = args[++i]) {
    //if (isNull(x) || !isObject(x)) {
      str += ' ' + x;
    //} else {
    //  str += ' ' + inspect(x);
    //}
  }
  return str;
};

function isString(arg) {
  return typeof arg === 'string';
}
exports.isString = isString;

/**
 * Inherit the prototype methods from one constructor into another.
 *
 * The Function.prototype.inherits from lang.js rewritten as a standalone
 * function (not on Function.prototype). NOTE: If this file is to be loaded
 * during bootstrapping this function needs to be rewritten using some native
 * functions as prototype setup using normal JavaScript does not work as
 * expected during bootstrapping (see mirror.js in r114903).
 *
 * @param {function} ctor Constructor function which needs to inherit the
 * prototype.
 * @param {function} superCtor Constructor function to inherit prototype from.
 */
exports.inherits = function(ctor, superCtor) {
  ctor.super_ = superCtor;
  ctor.prototype = Object.create(superCtor.prototype, {
    constructor: {
      value: ctor,
      enumerable: false,
      writable: true,
      configurable: true
    }
  });
};

}(util));


/*
Copyright (c) 2014 Cimaron Shanahan

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
 * Preprocessor Class
 */
function Preprocessor() {
	this.error = [];
}

Preprocessor.modules = {};

var proto = Preprocessor.prototype;

proto.process = function(src) {
	var m;

	this.error = null;

	for (m in Preprocessor.modules) {
		
		try {
			result = Preprocessor.modules[m].process(src);
		} catch (e) {
			this.error.push(util.format("compiler error: %s at line %s, column %s", e.message, e.lineNumber, e.columnNumber));
			return false;
		}

		if (!result) {
			this.error.push(Preprocessor.modules[m].error);
			return false;
		}
	}

	return result;
};

glsl.preprocessor = new Preprocessor();


/*
Copyright (c) 2014 Cimaron Shanahan

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

Preprocessor.modules.comments = {

	error : null,
	
	process : function(src) {
		var i,
		    chr,
			la,
			out = "",
			line = 1,
		    in_single = 0,
			in_multi = 0
			;
		
		for (i = 0; i < src.length; i++) {

			chr = src.substr(i, 1);
			la = src.substr(i + 1, 1);
			
			//Enter single line comment
			if (chr == '/' && la == '/' && !in_single && !in_multi) {
				in_single = line;
				i++;
				continue;
			}
			
			//Exit single line comment
			if (chr == "\n" && in_single) {
				in_single = 0;
			}
			
			//Enter multi line comment
			if (chr == '/' && la == '*' && !in_multi && !in_single) {
				in_multi = line;
				i++;
				continue;
			}
			
			//Exit multi line comment
			if (chr == '*' && la == '/' && in_multi) {

				//Treat single line multi-comment as space
				if (in_multi == line) {
					out += " ";
				}

				in_multi = 0;
				i++;				
				continue;
			}

			//Newlines are preserved
			if ((!in_multi && !in_single) || chr == "\n") {
				out += 	chr;
				line++;
			}
		}
		
		return out;
	}

};


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
CONNECTION WITH THE SOFTWARE OR THE USE		 OR OTHER DEALINGS IN THE SOFTWARE.
*/

function Type(name, size, slots, base) {
	this.name = name;
	this.size = size;
	this.slots = slots;
	this.base = base;
}

var types = {
	_void : new Type("void", 1, 1),
	bool : new Type("bool", 1, 1),
	int : new Type("int", 1, 1),
	float : new Type("float", 1, 1),
	vec2 : new Type("vec2", 2, 1, 'float'),
	vec3 : new Type("vec3", 3, 1, 'float'),
	vec4 : new Type("vec4", 4, 1, 'float'),
	bvec2 : new Type("bvec2", 2, 1, 'bool'),
	bvec3 : new Type("bvec3", 3, 1, 'bool'),
	bvec4 : new Type("bvec4", 4, 1, 'bool'),
	ivec2 : new Type("ivec2", 2, 1, 'int'),
	ivec3 : new Type("ivec3", 3, 1, 'int'),
	ivec4 : new Type("ivec4", 4, 1, 'int'),
	mat2 : new Type("mat2", 4, 2, 'float'),
	mat3 : new Type("mat3", 9, 3, 'float'),
	mat4 : new Type("mat4", 16, 4, 'float'),
	sampler2D : new Type("sampler2D", 1, 1),
	samplerCube : new Type("samplerCube", 1, 1)
};


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
 * SymbolTableEntry constructor
 */
function SymbolTableEntry(name, typedef) {
	this.name = name;
	this.typedef = typedef;
	this.type = null;
	this.definition = null;
	this.depth = null;
	this.qualifier = null;
	this.out = name;
	this.constant = null;		
}

SymbolTableEntry.typedef = {
	variable : 0,
	func : 1,
	type : 2
};


/**
 * symbol_table constructor
 */
function SymbolTable() {
	this.table = {};
	this.depth = 0;
}

SymbolTable.prototype = {};
var proto = SymbolTable.prototype;

/**
 * 
 */
proto.push_scope = function() {
	this.depth++;
};

/**
 * 
 */
proto.pop_scope = function() {
	var n, t;
	
	for (n in this.table) {
		
		if (this.table.hasOwnProperty(n)) {
			t = this.table[n];
			
			while (t[0] && t[0].depth === this.depth) {
				t.splice(0, 1);	
			}
			
			if (t.length === 0) {
				delete this.table[n];	
			}
		}
	}

	this.depth--;
};

/**
 * 
 */
proto.name_declared_this_scope = function(name) {
	
	var e = this.get_entry(name);
	
	return e && e.depth === this.depth;
};

/**
 * 
 */
proto.add_variable = function(name, type) {
	
	var entry = new SymbolTableEntry(name, SymbolTableEntry.typedef.variable);
	entry.type = type;
	
	return this._add_entry(entry);
};

/**
 * 
 */
proto.add_type = function(name, t) {
	
	var entry = new SymbolTableEntry(name, SymbolTableEntry.typedef.type);
	entry.definition = t;
	
	return this._add_entry(entry);
};

/**
 * 
 */
proto.add_function = function(name, type, def) {

	var entry;

	//don't readd the exact same function definition
	entry = this.get_function(name, type, def);
	if (entry) {
		return entry;
	}

	entry = new SymbolTableEntry(name, SymbolTableEntry.typedef.func);
	entry.type = type;

	if (def) {
		entry.definition = def;
	}

	return this._add_entry(entry);
};

/**
 * 
 */
proto.get_variable = function(name) {
	
	var entry = this.get_entry(name, SymbolTableEntry.typedef.variable);

	return entry;
};

/**
 * 
 */
proto.get_type = function(name) {

	var entry = this.get_entry(name, SymbolTableEntry.typedef.type);

	return entry;
};

/**
 * 
 */
proto.get_function = function(name, type, def) {
	
	var entry = this.get_entry(name, SymbolTableEntry.typedef.func, def);
	
	return entry;
};

/**
 * @protected
 */
proto._match_definition = function(def, entry) {

	var i;
	
	if (!def) {
		return true;	
	}
	
	if (def.length !== entry.length) {
		return false;	
	}
	
	for (i = 0; i < def.length; i++) {
		if (def[i] !== entry[i]) {
			return false;
		}
	}
	
	return true;
};

/**
 * @protected
 */
proto._add_entry = function(entry) {

	if (!this.table[entry.name]) {
		this.table[entry.name] = [];	
	}
	
	this.table[entry.name].splice(0, 0, entry);
	entry.depth = this.depth;
	
	return entry;
};

/**
 * @protected
 */
proto.get_entry = function(name, typedef, def) {

	var t, i, entry;
	
	t = this.table[name] || [];
	for (i = 0; i < t.length; i++) {
		entry = t[i];
		if (entry.typedef === typedef && (typedef !== SymbolTableEntry.typedef.func || this._match_definition(def, entry.definition))) {
			return entry;
		}
	}
	
	return null;
};


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
 * Base class of all abstract syntax tree nodes
 */
function AstNode() {

	//public:
	this.location = {
		first_line : 0,
		first_column : 0,
		last_line : 0,
		last_column : 0
	};
}

AstNode.prototype = {};
var proto = AstNode.prototype;

//public:
proto.getLocation = function() {
	return this.location;
};

proto.setLocation = function(loc) {
	this.location.first_line = loc.first_line;
	this.location.first_column = loc.first_column;
	this.location.last_line = loc.last_line;
	this.location.last_column = loc.last_column;
};

proto.toString = function() {
	return this.constructor.name;
};

proto.ir = function() {debugger;};


//inverse of operators
var ast_operators = [
	"=",
	"POS",
	"NEG",
	"+",
	"-",
	"*",
	"/",
	"%",
	"<<",
	">>",
	"<",
	">",
	"<=",
	">=",
	"==",
	"!=",
	"&",
	"^",
	"|",
	"~",
	"&&",
	"^^",
	"||",
	"!",		
	"*=",
	"/=",
	"%=",
	"+=",
	"-=",
	"<<=",
	">>=",
	"&=",
	"^=",
	"|=",
	"?:",
	"++x",
	"--x",
	"x++",
	"x--",
	".",
	"[]",
	"()",
	"ident",
	"float",
	"int",
	"bool"
];

var ast_precision = {
	none : 0,
	high : 1,
	medium : 2,
	low : 3
};



/**
 * AST Type Specifier Class
 */
function AstTypeSpecifier(specifier) {
	AstNode.apply(this);
	this.type_specifier = null;
	this.type_name = null;
	this.structure = null;
	this.is_array = 0;
	this.array_size = null;	
	this.precision = 2;
	this.is_precision_statement = null;

	if (AstTypeSpecifier[typeof specifier]) {
		AstTypeSpecifier[typeof specifier].call(this, specifier);
	}
}

proto = AstTypeSpecifier.prototype;
util.inherits(AstTypeSpecifier, AstNode);

//overloaded constructors
AstTypeSpecifier.number = function(specifier) {
	this.type_specifier = specifier;
	this.precision = ast_precision.none;
	this.is_precision_statement = false;
	this.type_name = types[specifier].name;
};

AstTypeSpecifier.string = function(name) {
	this.type_specifier = types[name];
	this.type_name = name;
	this.is_array = false;
	this.precision = ast_precision.none;
	this.is_precision_statement = false;
};

AstTypeSpecifier.object = function(s) {
	this.type_specifier = types.struct;
	this.type_name = s.name;
	this.structure = s;
	this.is_array = false;
	this.precision = ast_precision.none;
	this.is_precision_statement = false;			
};

/**
 * toString
 *
 * @return  string
 */
proto.toString = function() {
	return util.format("%s %s",
					this.type_specifier === types.struct ? this.structure : this.type_name,
					this.is_array ? util.format("[ %s] ", this.array_size || "") : ""
					);
};


/**
 * AST Function Class
 */
function AstFunction() {
	AstNode.apply(this);

	this.return_type = null;
	this.identifier = null;
	this.parameters = [];
	this.is_definition = false;
	this.signature = null;	
}

proto = AstFunction.prototype;
util.inherits(AstFunction, AstNode);

/**
 * toString
 *
 * @return  string
 */
AstFunction.toString = function() {
	return util.format("%s %s(%s)", this.return_type, this.identifier, this.parameters);			
};

/**
 * Representation of any sort of expression.
 */
function AstExpression(oper, ex0, ex1, ex2) {
	AstNode.apply(this);

	this.oper = oper;
	this.subexpressions = [null, null, null];
	this.primary_expression = {};
	this.expressions = [];

	if (ast_operators.indexOf(this.oper) === -1) {
		this.oper = 'ident';
		this.primary_expression.identifier = oper;
	} else {
		this.subexpressions[0] = ex0;
		this.subexpressions[1] = ex1;
		this.subexpressions[2] = ex2;
	}
}

util.inherits(AstExpression, AstNode);
proto = AstExpression.prototype;

//public:

/**
 * Makes number a float representation
 *
 * @param   string   The string representation of a number
 *
 * @return  string
 */
proto.makeFloat = function(n) {
	n += (n.toString().indexOf('.') === -1) ? ".0" : "";
	return n;
};


proto.toString = function() {
	switch (this.oper) {
		case '=':
		case '*=':
		case '/=':
		case '%=':
		case '+=':
		case '-=':
		case '<<=':
		case '>>=':
		case '&=':
		case '^=':
		case '|=':
			return util.format("(%s %s %s)", this.subexpressions[0], this.oper, this.subexpressions[1]);

		case '.':
			return util.format("(%s. %s)", this.subexpressions[0], this.primary_expression.identifier);

		case '+':
		case '-':
		case '~':
		case '++_':
		case '--_':
			return util.format("(%s %s)", this.oper, this.subexpressions[0]);
		
		case '_++':
		case '_--':
			return util.format("(%s %s)", this.subexpressions[0], this.oper);

		case '?:':
			return util.format("(%s ? %s : %s)", this.subexpressions[0], this.subexpressions[1], this.subexpressions[2]);				

		case '[]':
			return util.format("(%s [ %s ])", this.subexpressions[0], this.subexpressions[1]);				

		case '()':
			return util.format("(%s ( %s ))", this.subexpressions[0], this.expressions.join(", "));

		case 'ident':
			return util.format("%s", this.primary_expression.identifier);
		
		case 'int':
			return util.format("%s", this.primary_expression.int_constant);
		
		case 'float':
			return util.format("%s", this.primary_expression.float_constant);
		
		case 'bool':
			return util.format("%s", this.primary_expression.bool_constant ? 'true' : 'false');

		case ',':
			return util.format("(%s)", this.expressions.join(", "));
	}
};


var AstTypeQualifier = function() {
	//large union
	this.flags = {};
	this.location = null;
};

AstTypeQualifier.flags = {
	invariant : 1,
	constant : 2,
	attribute : 4,
	varying : 8,
	'in' : 16,
	out : 32,
	centroid : 64,
	uniform : 128,
	smooth : 256,
	flat : 512,
	noperspective : 1024,
	origin_upper_left : 2048,
	pixel_center_integer : 4096,
	explicit_location : 8192
};


/**
 * AST Fully Specified Type Class
 */
function AstFullySpecifiedType() {
	AstNode.apply(this);
	
	this.qualifier = null;
	this.specifier = null;
}

proto = AstFullySpecifiedType.prototype;
util.inherits(AstFullySpecifiedType, AstNode);

/**
 * 
 */
proto.has_qualifiers = function() {
	return this.qualifier.flags.i !== 0;
};

/**
 * toString
 *
 * @return  string
 */
proto.toString = function() {
	return util.format("... %s", this.specifier);
};


/**
 * AST Declaration Class
 */
function AstDeclaration(identifier, is_array, array_size, initializer) {
	AstNode.apply(this);

	this.identifier = identifier;
	this.is_array = is_array;
	this.array_size = array_size;
	this.initializer = initializer;
}

proto = AstDeclaration.prototype;
util.inherits(AstDeclaration, AstNode);

/**
 * toString
 *
 * @return  string
 */
proto.toString = function() {
	return util.format("%s %s %s", this.identifier, "...", this.initializer ? util.format("= %s", this.initializer) : "");
};


/**
 * AST Declarator List Class
 */
function AstDeclaratorList(type) {
	AstNode.apply(this);

	this.type = type;
	this.declarations = [];
	this.invariant = 0;
}

proto = AstDeclaratorList.prototype;
util.inherits(AstDeclaratorList, AstNode);

/**
 * toString
 *
 * @return  string
 */
proto.toString = function() {
	return util.format("%s %s;\n", this.type || "invariant ", this.declarations.join(""));
};


/**
 * AST Parameter Declarator Class
 */
function AstParameterDeclarator() {
	AstNode.apply(this);
	this.type = null;
	this.identifier = null;
	this.is_array = false;
	this.array_size = 0;
	this.formal_parameter = null;
	this.is_void = null;
}

proto = AstParameterDeclarator.prototype;
util.inherits(AstParameterDeclarator, AstNode);

/**
 * toString
 *
 * @return  string
 */
proto.toString = function() {
	return util.format("%s%s %s", this.type, this.identifier || "", this.is_array ? util.format("[%s]", this.array_size) : "");
};


/**
 * AST Expression Statement Class
 */
function AstExpressionStatement(ex) {
	AstNode.apply(this);

	this.expression = ex;
}

proto = AstExpressionStatement.prototype;
util.inherits(AstExpressionStatement, AstNode);

/**
 * toString
 *
 * @return  string
 */
proto.toString = function() {
	return util.format("%s;\n ", this.expression || "");
};


/**
 * AST Compound Statement Class
 */
function AstCompoundStatement(new_scope, statements) {
	AstNode.apply(this);
	this.new_scope = new_scope;
	if (statements) {
		this.statements = statements;
	} else {
		this.statements = [];
	}
}
 
proto = AstCompoundStatement.prototype;
util.inherits(AstCompoundStatement, AstNode);

/**
 * toString
 *
 * @return  string
 */
proto.toString = function() {
	return util.format("{\n%s}\n", this.statements.join(""));
};


/**
 * AST Function Definition Class
 */
function AstFunctionDefinition() {
	AstNode.apply(this);

	this.proto_type = null;
	this.body = null;
}

proto = AstFunctionDefinition.prototype;
util.inherits(AstFunctionDefinition, AstNode);

/**
 * toString
 *
 * @return  string
 */
proto.toString = function() {
	return util.format("%s%s", this.proto_type, this.body);
};

/**
 * AST Function Definition Class
 */
function AstExpressionBin(oper, ex0, ex1) {
	AstExpression.apply(this, [oper, ex0, ex1]);
}

proto = AstExpressionBin.prototype;
util.inherits(AstExpressionBin, AstExpression);

/**
 * toString
 *
 * @return  string
 */
proto.toString = function() {
	return util.format("(%s %s %s)", this.subexpressions[0], this.oper, this.subexpressions[1]);
};


/**
 * AST Function Expression Class
 */
function AstFunctionExpression(arg) {
	AstExpression.apply(this);
	this.cons = false;

	if (arg.constructor.name === 'AstExpression') {
		this.cons = false;
		AstExpression.call(this, '()', arg);
	} else if (arg.constructor.name === 'AstTypeSpecifier') {
		this.cons = true;
		AstExpression.call(this, '()', arg);
	}

}

proto = AstFunctionExpression.prototype;
util.inherits(AstFunctionExpression, AstExpression);

proto.is_constructor = function() {
	return this.cons;
};


/**
 * AST Selection Statement Class
 */
function AstSelectionStatement(condition, then_statement, else_statement) {
	AstNode.apply(this);
	this.condition = condition;
	this.then_statement = then_statement;
	this.else_statement = else_statement;
}

proto = AstSelectionStatement.prototype;
util.inherits(AstSelectionStatement, AstNode);

/**
 * toString
 *
 * @return  string
 */
proto.toString = function() {
	return util.format("if ( %s) %s %s", this.condition, this.then_statement, this.else_statement ? util.format("else %s", this.else_statement) : "");
};


/**
 * AST Struct Specifier Class
 */
function AstStructSpecifier(identifier, declarator_list) {
	AstNode.apply(this);
	this.name = null;
	this.declarations = [];

	if (identifier === null) {
		identifier = glsl.util.format("#anon_struct%d", AstStructSpecifier.anon_count);
		AstStructSpecifier.anon_count++;
	}
	this.name = identifier;
	this.declarations = declarator_list.declarations;
}

AstStructSpecifier.anon_count = 1;

proto = AstStructSpecifier.prototype;
util.inherits(AstStructSpecifier, AstNode);



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
CONNECTION WITH THE SOFTWARE OR THE USE		 OR OTHER DEALINGS IN THE SOFTWARE.
*/

var builtin = {
	
	vars : {
	
		vertex : [
			{
				position : 0,
				type : 'vec4',
				name : 'gl_Position',
				out : 'result@0'
			},
			{
				position : 1,
				type : 'float',
				name : 'gl_PointSize',
				out : 'result@1'
			}
		],

		fragment : [
			{
				position : 0,
				type : 'vec4',
				name : 'gl_FragColor',
				out : 'result@0'
			}
		]
	},
	
	/**
	 * List of instructions for operators
	 * 
	 * Denoted by operator, then by definition of param types to output type
	 */
	oper : {
		"!" : {
			"bool:bool" : [
				"SEQ %1.x %2.x 0.0"
				]
			},
		"+" : {
			"float,float:float" : ["ADD %1.x %2.x %3.x"],
			"vec2,vec2:vec2" : ["ADD %1.xy %2.xy %3.xy"],
			"vec3,vec3:vec3" : ["ADD %1.xyz %2.xyz %3.xyz"],
			"vec4,vec4:vec4" : ["ADD %1 %2 %3"]
			},
		"<" : {
			"float,float:float" : ["SLT %1.x %2.x %3.x"]
			},
		"*" : {
			"float,float:float" : ["MUL %1.x %2.x %3.x"],
			"float,vec3:vec3" : ["MUL %1.xyz %2.x %3.xyz"],
			"float,vec4:vec4" : ["MUL %1 %2.x %3"],
			"vec3,float:vec3" : ["MUL %1.xyz %2.xyz %3.x"],
			"vec3,vec3:vec3" : ["MUL %1.xyz %2.xyz %3.xyz"],
			"vec4,vec4:vec4" : ["MUL %1 %2 %3"],
			"mat3,vec3:vec3" : [
				"MUL %1.xyz %2.xyz %3.x",
				"MAD %1.xyz %2@1.xyz %3.y %1",
				"MAD %1.xyz %2@2.xyz %3.z %1"
				],
			"mat4,vec4:vec4" : [
				"MUL %1 %2 %3.x",
				"MAD %1 %2@1 %3.y %1",
				"MAD %1 %2@2 %3.z %1",
				"MAD %1 %2@3 %3.w %1"
				],
			"mat4,mat4:mat4" : [
				"MUL %1 %2 %3.x",
				"MAD %1 %2@1 %3.y %1",
				"MAD %1 %2@2 %3.z %1",
				"MAD %1 %2@3 %3.w %1",
				"MUL %1@1 %2 %3@1.x",
				"MAD %1@1 %2@1 %3@1.y %1@1",
				"MAD %1@1 %2@2 %3@1.z %1@1",
				"MAD %1@1 %2@3 %3@1.w %1@1",
				"MUL %1@2 %2 %3@2.x",
				"MAD %1@2 %2@1 %3@2.y %1@2",
				"MAD %1@2 %2@2 %3@2.z %1@2",
				"MAD %1@2 %2@3 %3@2.w %1@2",
				"MUL %1@3 %2 %3@3.x",
				"MAD %1@3 %2@1 %3@3.y %1@3",
				"MAD %1@3 %2@2 %3@3.z %1@3",
				"MAD %1@3 %2@3 %3@3.w %1@3"
				]
			},
		"-" : {
			"vec3,vec3:vec3" : ["SUB %1.xyz %2.xyz %3.xyz"],
			"vec4,vec4:vec4" : ["SUB %1 %2 %3"]
		}
	},

	/**
	 * List of instructions for built in functions
	 * 
	 * Denoted by function name, then by definition of param types to output type
	 */
	func : {
		"dot": {
			"vec3,vec3:float" : ["DP3 %1.x %2.xyz %3.xyz"],
			"vec4,vec4:float" : ["DP4 %1.x %2 %3"]
			},			
        "max": {
			"float,float:float" : ["MAX %1.x %2.x %3.x"]
			},
        "normalize": {
			"vec3:vec3" : [
				"DP3 %1.x %2 %2",
				"RSQ %1.x %1.x",
				"MUL %1.xyz %2.xyz %1.x"
				],
			"vec4:vec4" : [
				"DP4 %1.x %2 %2",
				"RSQ %1.x %1.x",
				"MUL %1 %2 %1.x"
				]
			},
		"pow": {
			"float,float:float" : ["POW %1.x %2.x %3.x"]
			},
        "reflect": {
			"vec3,vec3:vec3" : [
				"DP3 %1.x %3 %2",
				"MUL %1.xyz %3 %1.x",
				"MAD %1.xyz -%1 2.0 %2"
				]
			},
		"texture2D": {
			"sampler2D,vec2:vec4" : ["TEX %1 %3 %2 \"2D\""]
		}
	}
};

function _builtinParseType(str) {
	var parts, ret;

	parts = str.split(":");
	parts[0] = parts[0].split(",");
	
	ret = {
		src : parts[0],
		dest : parts[1]
	};
	
	return ret;
}


function symbol_table_init(state) {
	var i, j, vars, v, entry, types, name;

	vars = (state.options.target === glsl.target.vertex) ? builtin.vars.vertex : builtin.vars.fragment;

	for (i = 0; i < vars.length; i++) {
		v = vars[i];
		entry = state.symbols.add_variable(v.name, v.type);
		entry.position = v.position;
		entry.out = v.out;
	}

	vars = builtin.func;

	for (name in vars) {
		v = vars[name];
		for (j in v) {
			types = _builtinParseType(j);	
			entry = state.symbols.add_function(name, types.dest, types.src);
			entry.code = v[j]
		}
	}
}


/* parser generated by jison 0.4.15 */
/*
  Returns a Parser object of the following structure:

  Parser: {
    yy: {}
  }

  Parser.prototype: {
    yy: {},
    trace: function(),
    symbols_: {associative list: name ==> number},
    terminals_: {associative list: number ==> name},
    productions_: [...],
    performAction: function anonymous(yytext, yyleng, yylineno, yy, yystate, $$, _$),
    table: [...],
    defaultActions: {...},
    parseError: function(str, hash),
    parse: function(input),

    lexer: {
        EOF: 1,
        parseError: function(str, hash),
        setInput: function(input),
        input: function(),
        unput: function(str),
        more: function(),
        less: function(n),
        pastInput: function(),
        upcomingInput: function(),
        showPosition: function(),
        test_match: function(regex_match_array, rule_index),
        next: function(),
        lex: function(),
        begin: function(condition),
        popState: function(),
        _currentRules: function(),
        topState: function(),
        pushState: function(condition),

        options: {
            ranges: boolean           (optional: true ==> token location info will include a .range[] member)
            flex: boolean             (optional: true ==> flex-like lexing behaviour where the rules are tested exhaustively to find the longest match)
            backtrack_lexer: boolean  (optional: true ==> lexer regexes are tested in order and for each matching regex the action code is invoked; the lexer terminates the scan when a token is returned by the action code)
        },

        performAction: function(yy, yy_, $avoiding_name_collisions, YY_START),
        rules: [...],
        conditions: {associative list: name ==> set},
    }
  }


  token location info (@$, _$, etc.): {
    first_line: n,
    last_line: n,
    first_column: n,
    last_column: n,
    range: [start_number, end_number]       (where the numbers are indexes into the input string, regular zero-based)
  }


  the parseError function receives a 'hash' object with these members for lexer and parser errors: {
    text:        (matched text)
    token:       (the produced terminal token, if any)
    line:        (yylineno)
  }
  while parser (grammar) errors will also provide these members, i.e. parser errors deliver a superset of attributes: {
    loc:         (yylloc)
    expected:    (string describing the set of expected tokens)
    recoverable: (boolean: TRUE when the parser has a error recovery rule available for this particular error)
  }
*/
var parser = (function(){
var o=function(k,v,o,l){for(o=o||{},l=k.length;l--;o[k[l]]=v);return o},$V0=[13,14,15,16,17,21,22,47,108,120,121,125,128,132,133,134,135,137,138,139,140,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169],$V1=[1,18],$V2=[1,19],$V3=[1,20],$V4=[1,21],$V5=[1,22],$V6=[1,53],$V7=[1,54],$V8=[1,17],$V9=[1,44],$Va=[1,45],$Vb=[1,28],$Vc=[1,47],$Vd=[1,48],$Ve=[1,49],$Vf=[1,50],$Vg=[1,40],$Vh=[1,41],$Vi=[1,42],$Vj=[1,43],$Vk=[1,46],$Vl=[1,55],$Vm=[1,56],$Vn=[1,57],$Vo=[1,58],$Vp=[1,59],$Vq=[1,60],$Vr=[1,61],$Vs=[1,62],$Vt=[1,63],$Vu=[1,64],$Vv=[1,65],$Vw=[1,66],$Vx=[1,67],$Vy=[1,68],$Vz=[1,69],$VA=[1,70],$VB=[1,71],$VC=[1,72],$VD=[1,73],$VE=[1,74],$VF=[1,75],$VG=[1,76],$VH=[1,37],$VI=[1,38],$VJ=[1,39],$VK=[1,77],$VL=[5,13,14,15,16,17,21,47,108,120,121,125,128,132,133,134,135,137,138,139,140,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169],$VM=[1,82],$VN=[1,83],$VO=[1,84],$VP=[1,86],$VQ=[1,87],$VR=[49,106],$VS=[21,47,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169],$VT=[2,121],$VU=[1,101],$VV=[1,102],$VW=[1,103],$VX=[1,100],$VY=[2,159],$VZ=[21,25,26,49,106],$V_=[2,141],$V$=[21,25,26,30,32,49,106],$V01=[21,47,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,169],$V11=[21,47,120,121,135,137,138,139,140,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169],$V21=[21,25,26,30,32,34,49,106],$V31=[2,177],$V41=[2,12],$V51=[11,23,30,32,34,36,38,39,40,49,57,58,62,63,64,67,68,70,71,72,73,75,76,78,80,82,84,86,88,90,92,93,94,95,96,97,98,99,100,101,102,106,170],$V61=[5,10,13,14,15,16,17,21,25,26,28,29,30,39,40,47,51,57,58,59,60,106,108,120,121,125,128,132,133,134,135,137,138,139,140,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,172,189,191,193,194,195,196,197,198,202,203,204,205],$V71=[1,168],$V81=[1,169],$V91=[1,170],$Va1=[1,171],$Vb1=[1,155],$Vc1=[1,156],$Vd1=[1,181],$Ve1=[1,162],$Vf1=[1,163],$Vg1=[1,164],$Vh1=[1,165],$Vi1=[1,136],$Vj1=[1,127],$Vk1=[1,138],$Vl1=[1,139],$Vm1=[1,140],$Vn1=[1,141],$Vo1=[1,142],$Vp1=[1,143],$Vq1=[1,144],$Vr1=[1,145],$Vs1=[1,146],$Vt1=[1,147],$Vu1=[1,148],$Vv1=[32,49],$Vw1=[10,21,25,26,28,29,30,39,40,47,51,57,58,59,60,106,108,120,121,125,128,132,133,134,135,137,138,139,140,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,172,189,193,194,195,196,197,198,202,203,204,205],$Vx1=[10,21,25,26,28,29,30,39,40,47,51,57,58,59,60,106,108,120,121,125,128,132,133,134,135,137,138,139,140,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,172,189,191,193,194,195,196,197,198,202,203,204,205],$Vy1=[1,215],$Vz1=[23,32,36,49,106],$VA1=[23,32,36,49,57,58,62,63,64,67,68,70,71,72,73,75,76,78,80,82,84,86,88,90,106],$VB1=[2,58],$VC1=[23,32,36,49,57,58,62,63,64,67,68,70,71,72,73,75,76,78,80,82,84,86,88,90,92,93,94,95,96,97,98,99,100,101,102,106],$VD1=[1,249],$VE1=[23,32,36,49,88,90,106],$VF1=[1,250],$VG1=[23,32,34,36,38,39,40,49,57,58,62,63,64,67,68,70,71,72,73,75,76,78,80,82,84,86,88,90,92,93,94,95,96,97,98,99,100,101,102,106],$VH1=[10,21,25,26,28,29,30,39,40,47,51,57,58,59,60,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169],$VI1=[23,32,36,49,86,88,90,106],$VJ1=[1,251],$VK1=[23,32,36,49,84,86,88,90,106],$VL1=[1,254],$VM1=[23,32,36,49,82,84,86,88,90,106],$VN1=[1,255],$VO1=[23,32,36,49,80,82,84,86,88,90,106],$VP1=[1,259],$VQ1=[23,32,36,49,78,80,82,84,86,88,90,106],$VR1=[1,262],$VS1=[1,263],$VT1=[10,21,25,26,28,29,30,32,39,40,47,51,57,58,59,60,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169],$VU1=[23,32,36,49,75,76,78,80,82,84,86,88,90,106],$VV1=[1,264],$VW1=[1,265],$VX1=[1,266],$VY1=[1,267],$VZ1=[23,32,36,49,70,71,72,73,75,76,78,80,82,84,86,88,90,106],$V_1=[1,268],$V$1=[1,269],$V02=[23,32,36,49,67,68,70,71,72,73,75,76,78,80,82,84,86,88,90,106],$V12=[1,270],$V22=[1,271],$V32=[23,32,36,49,57,58,67,68,70,71,72,73,75,76,78,80,82,84,86,88,90,106],$V42=[1,272],$V52=[1,273],$V62=[1,274],$V72=[21,47,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,172],$V82=[1,304],$V92=[30,34],$Va2=[32,106],$Vb2=[10,21,25,26,28,29,30,39,40,47,51,57,58,59,60,106,120,121,125,128,132,133,134,135,137,138,139,140,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169];
var parser = {trace: function trace() { },
yy: {},
symbols_: {"error":2,"glsl-start":3,"translation_unit":4,"EOF":5,"version_statement":6,"extension_statement_list":7,"external_declaration_list":8,"VERSION":9,"INTCONSTANT":10,"EOL":11,"pragma_statement":12,"PRAGMA_DEBUG_ON":13,"PRAGMA_DEBUG_OFF":14,"PRAGMA_OPTIMIZE_ON":15,"PRAGMA_OPTIMIZE_OFF":16,"PRAGMA_INVARIANT_ALL":17,"extension_statement":18,"any_identifier":19,"variable_identifier":20,"TYPE_IDENTIFIER":21,"EXTENSION":22,":":23,"external_declaration":24,"IDENTIFIER":25,"NEW_IDENTIFIER":26,"primary_expression":27,"FLOATCONSTANT":28,"BOOLCONSTANT":29,"(":30,"expression":31,")":32,"postfix_expression":33,"[":34,"integer_expression":35,"]":36,"function_call":37,".":38,"++":39,"--":40,"function_call_or_method":41,"function_call_generic":42,"method_call_generic":43,"function_call_header_with_parameters":44,"function_call_header_no_parameters":45,"function_call_header":46,"VOID":47,"assignment_expression":48,",":49,"type_specifier":50,"FIELD_SELECTION":51,"method_call_header_with_parameters":52,"method_call_header_no_parameters":53,"method_call_header":54,"unary_expression":55,"unary_operator":56,"+":57,"-":58,"!":59,"~":60,"multiplicative_expression":61,"*":62,"/":63,"%":64,"additive_expression":65,"shift_expression":66,"<<":67,">>":68,"relational_expression":69,"<":70,">":71,"<=":72,">=":73,"equality_expression":74,"==":75,"!=":76,"and_expression":77,"&":78,"exclusive_or_expression":79,"^":80,"inclusive_or_expression":81,"|":82,"logical_and_expression":83,"&&":84,"logical_xor_expression":85,"^^":86,"logical_or_expression":87,"||":88,"conditional_expression":89,"?":90,"assignment_operator":91,"=":92,"*=":93,"/=":94,"%=":95,"+=":96,"-=":97,"<<=":98,">>=":99,"&=":100,"^=":101,"|=":102,"constant_expression":103,"declaration":104,"function_prototype":105,";":106,"init_declarator_list":107,"PRECISION":108,"precision_qualifier":109,"type_specifier_no_prec":110,"function_declarator":111,"function_header":112,"function_header_with_parameters":113,"parameter_declaration":114,"fully_specified_type":115,"parameter_declarator":116,"parameter_type_qualifier":117,"parameter_qualifier":118,"parameter_type_specifier":119,"IN":120,"OUT":121,"INOUT":122,"single_declaration":123,"initializer":124,"INVARIANT":125,"type_qualifier":126,"layout_qualifier":127,"LAYOUT":128,"layout_qualifier_id_list":129,"layout_qualifier_id":130,"interpolation_qualifier":131,"SMOOTH":132,"FLAT":133,"NOPERSPECTIVE":134,"CONST":135,"storage_qualifier":136,"ATTRIBUTE":137,"VARYING":138,"CENTROID":139,"UNIFORM":140,"type_specifier_nonarray":141,"basic_type_specifier_nonarray":142,"struct_specifier":143,"FLOAT":144,"DOUBLE":145,"INT":146,"BOOL":147,"VEC2":148,"VEC3":149,"VEC4":150,"BVEC2":151,"BVEC3":152,"BVEC4":153,"IVEC2":154,"IVEC3":155,"IVEC4":156,"MAT2X2":157,"MAT3X3":158,"MAT4X4":159,"SAMPLER1D":160,"SAMPLER2D":161,"SAMPLER3D":162,"SAMPLERCUBE":163,"SAMPLER1DSHADOW":164,"SAMPLER2DSHADOW":165,"HIGHP":166,"MEDIUMP":167,"LOWP":168,"STRUCT":169,"{":170,"struct_declaration_list":171,"}":172,"struct_declaration":173,"struct_declarator_list":174,"struct_declarator":175,"declaration_statement":176,"statement":177,"compound_statement":178,"simple_statement":179,"expression_statement":180,"selection_statement":181,"switch_statement":182,"case_label":183,"iteration_statement":184,"jump_statement":185,"statement_list":186,"statement_no_new_scope":187,"compound_statement_no_new_scope":188,"IF":189,"selection_rest_statement":190,"ELSE":191,"condition":192,"SWITCH":193,"CASE":194,"DEFAULT":195,"WHILE":196,"DO":197,"FOR":198,"for_init_statement":199,"for_rest_statement":200,"conditionopt":201,"CONTINUE":202,"BREAK":203,"RETURN":204,"DISCARD":205,"function_definition":206,"$accept":0,"$end":1},
terminals_: {2:"error",5:"EOF",9:"VERSION",10:"INTCONSTANT",11:"EOL",13:"PRAGMA_DEBUG_ON",14:"PRAGMA_DEBUG_OFF",15:"PRAGMA_OPTIMIZE_ON",16:"PRAGMA_OPTIMIZE_OFF",17:"PRAGMA_INVARIANT_ALL",21:"TYPE_IDENTIFIER",22:"EXTENSION",23:":",25:"IDENTIFIER",26:"NEW_IDENTIFIER",28:"FLOATCONSTANT",29:"BOOLCONSTANT",30:"(",32:")",34:"[",36:"]",38:".",39:"++",40:"--",47:"VOID",49:",",51:"FIELD_SELECTION",57:"+",58:"-",59:"!",60:"~",62:"*",63:"/",64:"%",67:"<<",68:">>",70:"<",71:">",72:"<=",73:">=",75:"==",76:"!=",78:"&",80:"^",82:"|",84:"&&",86:"^^",88:"||",90:"?",92:"=",93:"*=",94:"/=",95:"%=",96:"+=",97:"-=",98:"<<=",99:">>=",100:"&=",101:"^=",102:"|=",106:";",108:"PRECISION",120:"IN",121:"OUT",122:"INOUT",125:"INVARIANT",128:"LAYOUT",132:"SMOOTH",133:"FLAT",134:"NOPERSPECTIVE",135:"CONST",137:"ATTRIBUTE",138:"VARYING",139:"CENTROID",140:"UNIFORM",144:"FLOAT",145:"DOUBLE",146:"INT",147:"BOOL",148:"VEC2",149:"VEC3",150:"VEC4",151:"BVEC2",152:"BVEC3",153:"BVEC4",154:"IVEC2",155:"IVEC3",156:"IVEC4",157:"MAT2X2",158:"MAT3X3",159:"MAT4X4",160:"SAMPLER1D",161:"SAMPLER2D",162:"SAMPLER3D",163:"SAMPLERCUBE",164:"SAMPLER1DSHADOW",165:"SAMPLER2DSHADOW",166:"HIGHP",167:"MEDIUMP",168:"LOWP",169:"STRUCT",170:"{",172:"}",189:"IF",191:"ELSE",193:"SWITCH",194:"CASE",195:"DEFAULT",196:"WHILE",197:"DO",198:"FOR",202:"CONTINUE",203:"BREAK",204:"RETURN",205:"DISCARD"},
productions_: [0,[3,2],[4,3],[6,0],[6,3],[12,2],[12,2],[12,2],[12,2],[12,2],[7,0],[7,2],[19,1],[19,1],[18,5],[8,1],[8,2],[20,1],[20,1],[27,1],[27,1],[27,1],[27,1],[27,3],[33,1],[33,4],[33,1],[33,3],[33,2],[33,2],[35,1],[37,1],[41,1],[41,3],[42,2],[42,2],[45,2],[45,1],[44,2],[44,3],[46,2],[46,2],[46,1],[43,2],[43,2],[53,2],[53,1],[52,2],[52,3],[54,2],[55,1],[55,2],[55,2],[55,2],[56,1],[56,1],[56,1],[56,1],[61,1],[61,3],[61,3],[61,3],[65,1],[65,3],[65,3],[66,1],[66,3],[66,3],[69,1],[69,3],[69,3],[69,3],[69,3],[74,1],[74,3],[74,3],[77,1],[77,3],[79,1],[79,3],[81,1],[81,3],[83,1],[83,3],[85,1],[85,3],[87,1],[87,3],[89,1],[89,5],[48,1],[48,3],[91,1],[91,1],[91,1],[91,1],[91,1],[91,1],[91,1],[91,1],[91,1],[91,1],[91,1],[31,1],[31,3],[103,1],[104,2],[104,2],[104,4],[105,2],[111,1],[111,1],[113,2],[113,3],[112,3],[116,2],[116,5],[114,3],[114,2],[114,3],[114,2],[118,0],[118,1],[118,1],[118,1],[119,1],[107,1],[107,3],[107,5],[107,6],[107,7],[107,8],[107,5],[123,1],[123,2],[123,4],[123,5],[123,6],[123,7],[123,4],[123,2],[115,1],[115,2],[127,4],[129,1],[129,3],[130,1],[130,3],[131,1],[131,1],[131,1],[117,1],[126,1],[126,1],[126,2],[126,1],[126,2],[126,2],[126,3],[126,1],[136,1],[136,1],[136,1],[136,2],[136,1],[136,1],[136,2],[136,2],[136,1],[50,1],[50,2],[110,1],[110,3],[110,4],[141,1],[141,1],[141,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[142,1],[109,1],[109,1],[109,1],[143,5],[143,4],[171,1],[171,2],[173,3],[174,1],[174,3],[175,1],[175,4],[124,1],[176,1],[177,1],[177,1],[179,1],[179,1],[179,1],[179,1],[179,1],[179,1],[179,1],[178,2],[178,3],[187,1],[187,1],[188,2],[188,3],[186,1],[186,2],[180,1],[180,2],[181,5],[190,3],[190,1],[192,1],[192,4],[182,5],[183,3],[183,2],[184,5],[184,7],[184,6],[199,1],[199,1],[201,1],[201,0],[200,2],[200,3],[185,2],[185,2],[185,2],[185,3],[185,2],[24,1],[24,1],[24,1],[206,2]],
performAction: function anonymous(yytext, yyleng, yylineno, yy, yystate /* action[1] */, $$ /* vstack */, _$ /* lstack */) {
/* this == yyval */

var $0 = $$.length - 1;
switch (yystate) {
case 1:
 return $$[$0-1]; 
break;
case 15: case 16:

			if ($$[$0] !== null) {
				yy.state.translation_unit.push($$[$0]);
			}
		
break;
case 19:

				this.$ = new AstExpression('ident');
				this.$.setLocation(_$[$0]);
				this.$.primary_expression.identifier = $$[$0]; 
break;
case 20:

				this.$ = new AstExpression('int');
				this.$.setLocation(_$[$0]);
				this.$.primary_expression.int_constant = $$[$0]; 
break;
case 21:

				this.$ = new AstExpression('float');
				this.$.setLocation(_$[$0]);
				this.$.primary_expression.float_constant = $$[$0]; 
break;
case 22:

				this.$ = new AstExpression('bool');
				this.$.setLocation(_$[$0]);
				this.$.primary_expression.bool_constant = $$[$0]; 
break;
case 27:

				this.$ = new AstExpression('.', $$[$0-2]);
				this.$.setLocation(_$[$0-2]);				
				this.$.primary_expression.identifier = $$[$0]; 
break;
case 38:

				this.$ = $$[$0-1];
				this.$.setLocation(_$[$0-1]);
				this.$.expressions.push($$[$0]); 
break;
case 39:

				this.$ = $$[$0-2];
				this.$.setLocation(_$[$0-2]);
				this.$.expressions.push($$[$0]); 
break;
case 40:

				this.$ = new AstFunctionExpression($$[$0-1]);
				this.$.setLocation(_$[$0-1]);
			
break;
case 41:

				var callee = new AstExpression($$[$0-1]);
				this.$ = new AstFunctionExpression(callee);
				this.$.setLocation(_$[$0-1]); 
break;
case 51: case 52: case 53:

				this.$ = new AstExpression($$[$0-1], $$[$0]);
				this.$.setLocation(_$[$0-1]);
			
break;
case 59: case 60: case 61: case 63: case 64: case 66: case 67: case 69: case 70: case 71: case 72: case 74: case 75: case 77: case 79: case 81: case 83: case 85: case 87:

				this.$ = new AstExpressionBin($$[$0-1], $$[$0-2], $$[$0]);
				this.$.setLocation(_$[$0-2]);
			
break;
case 89:

				this.$ = new AstExpression($$[$0-3], $$[$0-4], $$[$0-2], $$[$0]);
				this.$.setLocation(_$[$0-4]);
			
break;
case 91:

				this.$ = new AstExpression($$[$0-1], $$[$0-2], $$[$0]);
				this.$.setLocation(_$[$0-2]);
			
break;
case 103:

				this.$ = $$[$0];
			
break;
case 104:

				if ($$[$0-2].oper !== $$[$0-1]) {
					this.$ = new AstExpression($$[$0-1]);
					this.$.setLocation(_$[$0-2]);
					this.$.expressions.push($$[$0-2]);
				} else {
					this.$ = $$[$0-2];
				}
				this.$.expressions.push($$[$0]);
			
break;
case 106:

				yy.state.symbols.pop_scope();
				this.$ = $$[$0-1];
			
break;
case 107:

				this.$ = $$[$0-1];
			
break;
case 108:

				$$[$0-1].precision = $$[$0-2];
				$$[$0-1].is_precision_statement = true;
				this.$ = $$[$0-1]; 
break;
case 112:

			  	this.$ = $$[$0-1];
				this.$.parameters.push($$[$0]);
			
break;
case 114:

				this.$ = new AstFunction();
				this.$.setLocation(_$[$0-2]);
				this.$.return_type = $$[$0-2];
				this.$.identifier = $$[$0-1];
				yy.state.symbols.add_function($$[$0-1]);
				yy.state.symbols.push_scope();
			
break;
case 120:

				this.$ = new AstParameterDeclarator();
				this.$.setLocation(_$[$0-1]);
				this.$.type = new AstFullySpecifiedType();
				this.$.type.qualifier = $$[$0-1];
				this.$.type.specifier = $$[$0];
			
break;
case 133:

				if ($$[$0].specifier.type_specifier !== types.struct) {
					yy.state.addError("empty declaration list", _$[$0].first_line, _$[$0].first_column);
					return 0;
				}

				this.$ = new AstDeclaratorList($$[$0]);
				this.$.setLocation(_$[$0]); 
break;
case 134:

				var decl = new AstDeclaration($$[$0], false);
				this.$ = new AstDeclaratorList($$[$0-1]);
				this.$.setLocation(_$[$0-1]);
				this.$.declarations.push(decl); 
break;
case 135:

				var decl = new AstDeclaration($$[$0-2], true);
				this.$ = new AstDeclaratorList($$[$0-3]);
				this.$.setLocation(_$[$0-3]);
				this.$.declarations.push(decl); 
break;
case 136:

				var decl = new AstDeclaration($$[$0-3], true, $$[$0-1]);
				this.$ = new AstDeclaratorList($$[$0-4]);
				this.$.setLocation(_$[$0-4]);
				this.$.declarations.push(decl); 
break;
case 137:

				var decl = new AstDeclaration($$[$0-4], true, null, $$[$0]);
				this.$ = new AstDeclaratorList($$[$0-5]);
				this.$.setLocation(_$[$0-5]);
				this.$.declarations.push(decl); 
break;
case 138:

				var decl = new AstDeclaration($$[$0-5], true, $$[$0-3], $$[$0]);
				this.$ = new AstDeclaratorList($$[$0-6]);
				this.$.setLocation(_$[$0-6]);
				this.$.declarations.push(decl); 
break;
case 139:

				var decl = new AstDeclaration($$[$0-2], false, null, $$[$0]);
				this.$ = new AstDeclaratorList($$[$0-3]);
				this.$.setLocation(_$[$0-3]);
				this.$.declarations.push(decl); 
break;
case 141:

				this.$ = new AstFullySpecifiedType();
				this.$.setLocation(_$[$0]);
				this.$.specifier = $$[$0]; 
break;
case 142:

				this.$ = new AstFullySpecifiedType();
				this.$.setLocation(_$[$0-1]);
				this.$.qualifier = $$[$0-1];
				this.$.specifier = $$[$0]; 
break;
case 143:

				this.$ = $$[$0-1]; 
break;
case 161:

				/*jslint bitwise: true */
				this.$ = new AstTypeQualifier();
				this.$.flags |= AstTypeQualifier.flags.attribute;
			
break;
case 162:

				this.$ = new AstTypeQualifier();
				this.$.flags |= AstTypeQualifier.flags.varying;
			
break;
case 163:

				this.$ = new AstTypeQualifier();
				this.$.flags |= AstTypeQualifier.flags.centroid;
				this.$.flags |= AstTypeQualifier.flags.varying;
			
break;
case 164:

				this.$ = new AstTypeQualifier();
				this.$.flags |= AstTypeQualifier.flags['in'];
			
break;
case 165:

				this.$ = new AstTypeQualifier();
				this.$.flags |= AstTypeQualifier.flags.out;
			
break;
case 166:

				this.$ = new AstTypeQualifier();
				this.$.flags |= AstTypeQualifier.flags.centroid;
				this.$.flags |= AstTypeQualifier.flags['in'];
			
break;
case 167:

				this.$ = new AstTypeQualifier();
				this.$.flags |= AstTypeQualifier.flags.centroid;
				this.$.flags |= AstTypeQualifier.flags.out;
			
break;
case 168:

				this.$ = new AstTypeQualifier();
				this.$.flags |= AstTypeQualifier.flags.uniform;
			
break;
case 169:

				this.$ = $$[$0];  
			
break;
case 170:

				this.$ = $$[$0];
				this.$.precision = $$[$0-1];
			
break;
case 174: case 175: case 176:

		  	this.$ = new AstTypeSpecifier($$[$0]);
			this.$.setLocation(_$[$0]);
		
break;
case 200:

				this.$ = ast_precision.high; 
break;
case 201:

				this.$ = ast_precision.medium; 
break;
case 202:

				this.$ = ast_precision.low; 
break;
case 203:

				this.$ = new AstStructSpecifier($$[$0-3], $$[$0-1]);
				this.$.setLocation(_$[$0-4]);
				yy.state.symbols.add_type($$[$0-3], types._void);
			
break;
case 205:

				this.$ = [$$[$0]];
			
break;
case 206:

				this.$ = $$[$0-1];
				this.$.push($$[$0]);
			
break;
case 207:

				var type = new AstFullySpecifiedType();
				type.setLocation(_$[$0-2]);
				type.specifier = $$[$0-2];
				
				this.$ = new AstDeclaratorList(type);
				this.$.setLocation(_$[$0-2]);
				this.$.declarations = $$[$0-1]; 
break;
case 210:

				this.$ = new AstDeclaration($$[$0], false);
				this.$.setLocation(_$[$0]);
				yy.state.symbols.add_variable($$[$0]);
			
break;
case 219: case 220: case 257:

				this.$ = null; 
break;
case 223:

				this.$ = new AstCompoundStatement(true);
				this.$.setLocation(_$[$0-1]); 
break;
case 224:

			  	yy.state.symbols.push_scope();
				this.$ = new AstCompoundStatement(true, $$[$0-1]);
				this.$.setLocation(_$[$0-2]);
				yy.state.symbols.pop_scope(); 
break;
case 228:

				this.$ = new AstCompoundStatement(false, $$[$0-1]);
				this.$.setLocation(_$[$0-2]); 
break;
case 229:

				if ($$[$0] === null) {
					yy.state.addError("<nil> statement", _$[$0].first_line, _$[$0].first_column);
				} else {
					this.$ = [$$[$0]];
				}
			
break;
case 230:

				if ($$[$0] === null) {
					yy.state.addError("<nil> statement", _$[$0-1].first_line, _$[$0-1].first_column);
				}
				this.$ = $$[$0-1];
				this.$.push($$[$0]);
			
break;
case 232:

				this.$ = new AstExpressionStatement($$[$0-1]);
				this.$.setLocation(_$[$0-1]); 
break;
case 233:

				this.$ = new AstSelectionStatement($$[$0-2], $$[$0].then_statement, $$[$0].else_statement);
				this.$.setLocation(_$[$0-4]); 
break;
case 234:
 debugger;
		  		this.$ = {};
				this.$.then_statement = $$[$0-2];
				this.$.else_statement = $$[$0]; 
break;
case 235:

				this.$.then_statement = $$[$01]; 
break;
case 255: case 256:

				this.$ = $$[$0]; 
break;
case 258:

				this.$ = new AstFunctionDefinition();
				this.$.setLocation(_$[$0-1]);
				this.$.proto_type = $$[$0-1];
				this.$.body = $$[$0];
				yy.state.symbols.pop_scope(); 
break;
}
},
table: [o($V0,[2,3],{3:1,4:2,6:3,9:[1,4]}),{1:[3]},{5:[1,5]},o($V0,[2,10],{7:6}),{10:[1,7]},{1:[2,1]},{8:8,12:14,13:$V1,14:$V2,15:$V3,16:$V4,17:$V5,18:9,21:$V6,22:[1,11],24:10,47:$V7,50:29,104:13,105:15,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,206:12},{11:[1,78]},{5:[2,2],12:14,13:$V1,14:$V2,15:$V3,16:$V4,17:$V5,21:$V6,24:79,47:$V7,50:29,104:13,105:15,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,206:12},o($V0,[2,11]),o($VL,[2,15]),{19:80,20:81,21:$VM,25:$VN,26:$VO},o($VL,[2,255]),o($VL,[2,256]),o($VL,[2,257]),{106:$VP,170:$VQ,188:85},{49:[1,89],106:[1,88]},{109:90,166:$VH,167:$VI,168:$VJ},{11:[1,91]},{11:[1,92]},{11:[1,93]},{11:[1,94]},{11:[1,95]},{32:[1,96]},o($VR,[2,126]),o($VS,$VT,{114:97,117:98,118:99,32:[2,110],120:$VU,121:$VV,122:$VW,135:$VX}),{32:[2,111],49:[1,104]},o($VR,[2,133],{19:105,20:106,21:$VM,25:$VN,26:$VO}),o($VS,$VY,{20:107,136:108,131:109,25:$VN,26:$VO,120:$V9,121:$Va,132:$Vd,133:$Ve,134:$Vf,135:$Vg,137:$Vh,138:$Vi,139:$Vj,140:$Vk}),o($VZ,$V_),{21:$V6,47:$V7,50:110,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($V$,[2,169]),{21:$V6,47:$V7,110:111,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,169:$VK},o($VS,[2,152]),o($VS,[2,153],{136:112,120:$V9,121:$Va,135:$Vg,137:$Vh,138:$Vi,139:$Vj,140:$Vk}),o($VS,[2,155],{136:113,120:$V9,121:$Va,135:$Vg,137:$Vh,138:$Vi,139:$Vj,140:$Vk}),o($V$,[2,171],{34:[1,114]}),o($V01,[2,200]),o($V01,[2,201]),o($V01,[2,202]),o($VS,[2,160]),o($VS,[2,161]),o($VS,[2,162]),{120:[1,116],121:[1,117],138:[1,115]},o($VS,[2,164]),o($VS,[2,165]),o($VS,[2,168]),{30:[1,118]},o($V11,[2,148]),o($V11,[2,149]),o($V11,[2,150]),o($V21,[2,174]),o($V21,[2,175]),o($V21,[2,176]),o($V21,$V31),o($V21,[2,178]),o($V21,[2,179]),o($V21,[2,180]),o($V21,[2,181]),o($V21,[2,182]),o($V21,[2,183]),o($V21,[2,184]),o($V21,[2,185]),o($V21,[2,186]),o($V21,[2,187]),o($V21,[2,188]),o($V21,[2,189]),o($V21,[2,190]),o($V21,[2,191]),o($V21,[2,192]),o($V21,[2,193]),o($V21,[2,194]),o($V21,[2,195]),o($V21,[2,196]),o($V21,[2,197]),o($V21,[2,198]),o($V21,[2,199]),{19:119,20:81,21:$VM,25:$VN,26:$VO,170:[1,120]},o($V0,[2,4]),o($VL,[2,16]),{23:[1,121]},o([11,23,32,34,49,92,106,170],$V41),o([11,23,32,34,36,38,39,40,49,57,58,62,63,64,67,68,70,71,72,73,75,76,78,80,82,84,86,88,90,92,93,94,95,96,97,98,99,100,101,102,106,170],[2,13]),o($V51,[2,17]),o($V51,[2,18]),o($VL,[2,258]),o($V61,[2,106]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:137,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:158,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,104:135,105:149,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$Vj1,172:[1,122],176:128,177:124,178:125,179:126,180:129,181:130,182:131,183:132,184:133,185:134,186:123,189:$Vk1,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1},o($V61,[2,107]),{19:186,20:81,21:$VM,25:$VN,26:$VO},{21:$V6,47:$V7,110:187,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,169:$VK},o($VL,[2,5]),o($VL,[2,6]),o($VL,[2,7]),o($VL,[2,8]),o($VL,[2,9]),o([106,170],[2,109]),o($Vv1,[2,112]),o($VS,$VT,{118:188,120:$VU,121:$VV,122:$VW}),{21:$V6,47:$V7,50:191,109:32,110:31,116:189,119:190,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o([21,47,120,121,122,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169],[2,151]),o($VS,[2,122]),o($VS,[2,123]),o($VS,[2,124]),o($VS,$VT,{117:98,118:99,114:192,120:$VU,121:$VV,122:$VW,135:$VX}),o($VR,[2,134],{34:[1,193],92:[1,194]}),o([34,49,92,106],$V41,{30:[1,195]}),o($VR,[2,140]),o($VS,[2,157]),{120:$V9,121:$Va,135:$Vg,136:196,137:$Vh,138:$Vi,139:$Vj,140:$Vk},o($VZ,[2,142]),o($V$,[2,170]),o($VS,[2,154]),o($VS,[2,156]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,36:[1,197],37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:199,103:198,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VS,[2,163]),o($VS,[2,166]),o($VS,[2,167]),{19:204,20:81,21:$VM,25:$VN,26:$VO,129:202,130:203},{170:[1,205]},{21:$V6,47:$V7,50:208,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,171:206,173:207},{19:209,20:81,21:$VM,25:$VN,26:$VO},o($V61,[2,227]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:137,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:158,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,104:135,105:149,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$Vj1,172:[1,210],176:128,177:211,178:125,179:126,180:129,181:130,182:131,183:132,184:133,185:134,189:$Vk1,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1},o($Vw1,[2,229]),o($Vx1,[2,214]),o($Vx1,[2,215]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:137,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:158,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,104:135,105:149,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$Vj1,172:[1,212],176:128,177:124,178:125,179:126,180:129,181:130,182:131,183:132,184:133,185:134,186:213,189:$Vk1,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1},o($Vx1,[2,216]),o($Vx1,[2,217]),o($Vx1,[2,218]),o($Vx1,[2,219]),o($Vx1,[2,220]),o($Vx1,[2,221]),o($Vx1,[2,222]),o($Vx1,[2,213]),o($Vx1,[2,231]),{49:$Vy1,106:[1,214]},{30:[1,216]},{30:[1,217]},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:218,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{23:[1,219]},{30:[1,220]},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:137,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:158,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,104:135,105:149,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$Vj1,176:128,177:221,178:125,179:126,180:129,181:130,182:131,183:132,184:133,185:134,189:$Vk1,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1},{30:[1,222]},{106:[1,223]},{106:[1,224]},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:226,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,106:[1,225],109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{106:[1,227]},{106:$VP},o($Vz1,[2,103]),o($Vz1,[2,90]),o($VA1,$VB1,{91:228,92:[1,229],93:[1,230],94:[1,231],95:[1,232],96:[1,233],97:[1,234],98:[1,235],99:[1,236],100:[1,237],101:[1,238],102:[1,239]}),o($Vz1,[2,88],{88:[1,241],90:[1,240]}),o($VC1,[2,50],{34:[1,242],38:[1,243],39:[1,244],40:[1,245]}),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:246,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:247,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:248,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VZ,$V_,{30:$VD1}),o($VE1,[2,86],{86:$VF1}),o($VG1,[2,24]),o($VG1,[2,26]),o($VH1,[2,54]),o($VH1,[2,55]),o($VH1,[2,56]),o($VH1,[2,57]),o($VI1,[2,84],{84:$VJ1}),o($VG1,[2,19],{30:[1,252]}),o($VG1,[2,20]),o($VG1,[2,21]),o($VG1,[2,22]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:253,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VG1,[2,31]),o($VK1,[2,82],{82:$VL1}),o($VG1,[2,32]),o($VM1,[2,80],{80:$VN1}),{32:[1,256],49:[1,257]},{32:[1,258]},o($VO1,[2,78],{78:$VP1}),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,32:[2,37],33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:[1,261],48:260,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VQ1,[2,76],{75:$VR1,76:$VS1}),o($VT1,[2,42]),o($VU1,[2,73],{70:$VV1,71:$VW1,72:$VX1,73:$VY1}),o($VZ1,[2,68],{67:$V_1,68:$V$1}),o($V02,[2,65],{57:$V12,58:$V22}),o($V32,[2,62],{62:$V42,63:$V52,64:$V62}),o($VR,[2,127],{34:[1,275],92:[1,276]}),{106:[1,277]},{21:$V6,47:$V7,50:191,109:32,110:31,116:278,119:279,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($Vv1,[2,118]),o($Vv1,[2,120]),o($Vv1,[2,125],{20:81,19:280,21:$VM,25:$VN,26:$VO}),o($Vv1,[2,113]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,36:[1,281],37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:199,103:282,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:284,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,124:283,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o([21,32,47,120,121,122,135,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169],[2,114]),o($VS,[2,158]),o($V$,[2,172]),{36:[1,285]},{36:[2,105]},o($VA1,$VB1),{30:$VD1},{32:[1,286],49:[1,287]},o($Vv1,[2,144]),o($Vv1,[2,146],{92:[1,288]}),{21:$V6,47:$V7,50:208,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,171:289,173:207},{21:$V6,47:$V7,50:208,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,172:[1,290],173:291},o($V72,[2,205]),{19:294,20:81,21:$VM,25:$VN,26:$VO,174:292,175:293},{11:[1,295]},o($V61,[2,228]),o($Vw1,[2,230]),o($Vx1,[2,223]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:137,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:158,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,104:135,105:149,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$Vj1,172:[1,296],176:128,177:211,178:125,179:126,180:129,181:130,182:131,183:132,184:133,185:134,189:$Vk1,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1},o($Vx1,[2,232]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:297,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:298,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:299,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{23:[1,300],49:$Vy1},o($Vx1,[2,240]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:302,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:158,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,115:303,120:$V9,121:$Va,125:$V82,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,192:301},{196:[1,305]},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:137,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:158,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,104:135,105:149,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,176:308,180:307,199:306},o($Vx1,[2,250]),o($Vx1,[2,251]),o($Vx1,[2,252]),{49:$Vy1,106:[1,309]},o($Vx1,[2,254]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:310,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VH1,[2,92]),o($VH1,[2,93]),o($VH1,[2,94]),o($VH1,[2,95]),o($VH1,[2,96]),o($VH1,[2,97]),o($VH1,[2,98]),o($VH1,[2,99]),o($VH1,[2,100]),o($VH1,[2,101]),o($VH1,[2,102]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:311,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:312,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:314,33:154,35:313,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{19:315,20:317,21:$VM,25:$VN,26:$VO,43:316,52:318,53:319,54:320},o($VG1,[2,28]),o($VG1,[2,29]),o($VC1,[2,51]),o($VC1,[2,52]),o($VC1,[2,53]),o($VT1,[2,40]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:321,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:322,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VT1,[2,41]),{32:[1,323],49:$Vy1},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:324,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:325,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VG1,[2,34]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:326,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VG1,[2,35]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:327,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($Vv1,[2,38]),o($V92,$V31,{32:[2,36]}),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:328,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:329,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:330,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:331,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:332,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:333,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:334,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:335,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:336,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:337,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:338,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:339,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:340,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,36:[1,341],37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:199,103:342,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:284,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,124:343,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($V61,[2,108]),o($Vv1,[2,117]),o($Vv1,[2,119]),o($Vv1,[2,115],{34:[1,344]}),o($VR,[2,135],{92:[1,345]}),{36:[1,346]},o($VR,[2,139]),o([32,49,106],[2,212]),o($V$,[2,173]),o($V11,[2,143]),{19:204,20:81,21:$VM,25:$VN,26:$VO,130:347},{10:[1,348]},{21:$V6,47:$V7,50:208,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,172:[1,349],173:291},o($V21,[2,204]),o($V72,[2,206]),{49:[1,351],106:[1,350]},o($VR,[2,208]),o($VR,[2,210],{34:[1,352]}),o($V0,[2,14]),o($Vx1,[2,224]),o($Vz1,[2,104]),{32:[1,353],49:$Vy1},{32:[1,354],49:$Vy1},o($Vx1,[2,239]),{32:[1,355]},o($Va2,[2,236],{49:$Vy1}),{19:356,20:81,21:$VM,25:$VN,26:$VO},o($VS,$VY,{136:108,131:109,120:$V9,121:$Va,132:$Vd,133:$Ve,134:$Vf,135:$Vg,137:$Vh,138:$Vi,139:$Vj,140:$Vk}),{30:[1,357]},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:302,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:158,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,106:[2,247],109:32,110:31,115:303,120:$V9,121:$Va,125:$V82,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,192:360,200:358,201:359},o($Vb2,[2,244]),o($Vb2,[2,245]),o($Vx1,[2,253]),o($Vz1,[2,91]),{23:[1,361],49:$Vy1},o($VE1,[2,87],{86:$VF1}),{36:[1,362]},{36:[2,30],49:$Vy1},o($VG1,[2,27]),o($VG1,[2,33]),o($VG1,$V41,{30:[1,363]}),{32:[1,364],49:[1,365]},{32:[1,366]},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,32:[2,46],33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:[1,368],48:367,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VI1,[2,85],{84:$VJ1}),o($VK1,[2,83],{82:$VL1}),o($VG1,[2,23]),o($VM1,[2,81],{80:$VN1}),o($VO1,[2,79],{78:$VP1}),o($Vv1,[2,39]),o($VQ1,[2,77],{75:$VR1,76:$VS1}),o($VU1,[2,74],{70:$VV1,71:$VW1,72:$VX1,73:$VY1}),o($VU1,[2,75],{70:$VV1,71:$VW1,72:$VX1,73:$VY1}),o($VZ1,[2,69],{67:$V_1,68:$V$1}),o($VZ1,[2,70],{67:$V_1,68:$V$1}),o($VZ1,[2,71],{67:$V_1,68:$V$1}),o($VZ1,[2,72],{67:$V_1,68:$V$1}),o($V02,[2,66],{57:$V12,58:$V22}),o($V02,[2,67],{57:$V12,58:$V22}),o($V32,[2,63],{62:$V42,63:$V52,64:$V62}),o($V32,[2,64],{62:$V42,63:$V52,64:$V62}),o($VA1,[2,59]),o($VA1,[2,60]),o($VA1,[2,61]),o($VR,[2,128],{92:[1,369]}),{36:[1,370]},o($VR,[2,132]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:199,103:371,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:284,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,124:372,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VR,[2,136],{92:[1,373]}),o($Vv1,[2,145]),o($Vv1,[2,147]),o($V21,[2,203]),o($V72,[2,207]),{19:294,20:81,21:$VM,25:$VN,26:$VO,175:374},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,50:201,51:$Vd1,55:200,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:199,103:375,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:137,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:158,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,104:135,105:149,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$Vj1,176:128,177:377,178:125,179:126,180:129,181:130,182:131,183:132,184:133,185:134,189:$Vk1,190:376,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1},{170:$Vj1,178:378},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:137,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:158,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,104:135,105:149,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$VQ,176:128,179:381,180:129,181:130,182:131,183:132,184:133,185:134,187:379,188:380,189:$Vk1,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1},{92:[1,382]},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:383,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{32:[1,384]},{106:[1,385]},{106:[2,246]},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:386,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VG1,[2,25]),o($VT1,[2,49]),o($VG1,[2,43]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:387,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VG1,[2,44]),o($Vv1,[2,47]),o($V92,$V31,{32:[2,45]}),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:284,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,124:388,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VR,[2,129],{92:[1,389]}),{36:[1,390]},o($VR,[2,137]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:284,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,124:391,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($VR,[2,209]),{36:[1,392]},o($Vx1,[2,233]),o($Vw1,[2,235],{191:[1,393]}),o($Vx1,[2,238]),o($Vx1,[2,241]),o($Vx1,[2,225]),o($Vx1,[2,226]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:284,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,124:394,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},{32:[1,395],49:$Vy1},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:137,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:158,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,104:135,105:149,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$VQ,176:128,179:381,180:129,181:130,182:131,183:132,184:133,185:134,187:396,188:380,189:$Vk1,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1},{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:397,32:[2,248],33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($Vz1,[2,89]),o($Vv1,[2,48]),o($VR,[2,130]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:284,50:201,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,109:32,110:31,124:398,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK},o($Vv1,[2,116]),o($VR,[2,138]),o($VR,[2,211]),{10:$V71,20:167,21:$V6,25:$VN,26:$VO,27:160,28:$V81,29:$V91,30:$Va1,31:137,33:154,37:161,39:$Vb1,40:$Vc1,41:172,42:174,44:176,45:177,46:179,47:$V7,48:150,50:158,51:$Vd1,55:152,56:157,57:$Ve1,58:$Vf1,59:$Vg1,60:$Vh1,61:185,65:184,66:183,69:182,74:180,77:178,79:175,81:173,83:166,85:159,87:153,89:151,104:135,105:149,106:$Vi1,107:16,108:$V8,109:32,110:31,111:23,112:25,113:26,115:27,120:$V9,121:$Va,123:24,125:$Vb,126:30,127:34,128:$Vc,131:35,132:$Vd,133:$Ve,134:$Vf,135:$Vg,136:33,137:$Vh,138:$Vi,139:$Vj,140:$Vk,141:36,142:51,143:52,144:$Vl,145:$Vm,146:$Vn,147:$Vo,148:$Vp,149:$Vq,150:$Vr,151:$Vs,152:$Vt,153:$Vu,154:$Vv,155:$Vw,156:$Vx,157:$Vy,158:$Vz,159:$VA,160:$VB,161:$VC,162:$VD,163:$VE,164:$VF,165:$VG,166:$VH,167:$VI,168:$VJ,169:$VK,170:$Vj1,176:128,177:399,178:125,179:126,180:129,181:130,182:131,183:132,184:133,185:134,189:$Vk1,193:$Vl1,194:$Vm1,195:$Vn1,196:$Vo1,197:$Vp1,198:$Vq1,202:$Vr1,203:$Vs1,204:$Vt1,205:$Vu1},o($Va2,[2,237]),{106:[1,400]},o($Vx1,[2,243]),{32:[2,249],49:$Vy1},o($VR,[2,131]),o($Vx1,[2,234]),o($Vx1,[2,242])],
defaultActions: {5:[2,1],199:[2,105],360:[2,246]},
parseError: function parseError(str, hash) {
    if (hash.recoverable) {
        this.trace(str);
    } else {
        throw new Error(str);
    }
},
parse: function parse(input) {
    var self = this, stack = [0], tstack = [], vstack = [null], lstack = [], table = this.table, yytext = '', yylineno = 0, yyleng = 0, recovering = 0, TERROR = 2, EOF = 1;
    var args = lstack.slice.call(arguments, 1);
    var lexer = Object.create(this.lexer);
    var sharedState = { yy: {} };
    for (var k in this.yy) {
        if (Object.prototype.hasOwnProperty.call(this.yy, k)) {
            sharedState.yy[k] = this.yy[k];
        }
    }
    lexer.setInput(input, sharedState.yy);
    sharedState.yy.lexer = lexer;
    sharedState.yy.parser = this;
    if (typeof lexer.yylloc == 'undefined') {
        lexer.yylloc = {};
    }
    var yyloc = lexer.yylloc;
    lstack.push(yyloc);
    var ranges = lexer.options && lexer.options.ranges;
    if (typeof sharedState.yy.parseError === 'function') {
        this.parseError = sharedState.yy.parseError;
    } else {
        this.parseError = Object.getPrototypeOf(this).parseError;
    }
    function popStack(n) {
        stack.length = stack.length - 2 * n;
        vstack.length = vstack.length - n;
        lstack.length = lstack.length - n;
    }
    _token_stack:
        function lex() {
            var token;
            token = lexer.lex() || EOF;
            if (typeof token !== 'number') {
                token = self.symbols_[token] || token;
            }
            return token;
        }
    var symbol, preErrorSymbol, state, action, a, r, yyval = {}, p, len, newState, expected;
    while (true) {
        state = stack[stack.length - 1];
        if (this.defaultActions[state]) {
            action = this.defaultActions[state];
        } else {
            if (symbol === null || typeof symbol == 'undefined') {
                symbol = lex();
            }
            action = table[state] && table[state][symbol];
        }
                    if (typeof action === 'undefined' || !action.length || !action[0]) {
                var errStr = '';
                expected = [];
                for (p in table[state]) {
                    if (this.terminals_[p] && p > TERROR) {
                        expected.push('\'' + this.terminals_[p] + '\'');
                    }
                }
                if (lexer.showPosition) {
                    errStr = 'Parse error on line ' + (yylineno + 1) + ':\n' + lexer.showPosition() + '\nExpecting ' + expected.join(', ') + ', got \'' + (this.terminals_[symbol] || symbol) + '\'';
                } else {
                    errStr = 'Parse error on line ' + (yylineno + 1) + ': Unexpected ' + (symbol == EOF ? 'end of input' : '\'' + (this.terminals_[symbol] || symbol) + '\'');
                }
                this.parseError(errStr, {
                    text: lexer.match,
                    token: this.terminals_[symbol] || symbol,
                    line: lexer.yylineno,
                    loc: yyloc,
                    expected: expected
                });
            }
        if (action[0] instanceof Array && action.length > 1) {
            throw new Error('Parse Error: multiple actions possible at state: ' + state + ', token: ' + symbol);
        }
        switch (action[0]) {
        case 1:
            stack.push(symbol);
            vstack.push(lexer.yytext);
            lstack.push(lexer.yylloc);
            stack.push(action[1]);
            symbol = null;
            if (!preErrorSymbol) {
                yyleng = lexer.yyleng;
                yytext = lexer.yytext;
                yylineno = lexer.yylineno;
                yyloc = lexer.yylloc;
                if (recovering > 0) {
                    recovering--;
                }
            } else {
                symbol = preErrorSymbol;
                preErrorSymbol = null;
            }
            break;
        case 2:
            len = this.productions_[action[1]][1];
            yyval.$ = vstack[vstack.length - len];
            yyval._$ = {
                first_line: lstack[lstack.length - (len || 1)].first_line,
                last_line: lstack[lstack.length - 1].last_line,
                first_column: lstack[lstack.length - (len || 1)].first_column,
                last_column: lstack[lstack.length - 1].last_column
            };
            if (ranges) {
                yyval._$.range = [
                    lstack[lstack.length - (len || 1)].range[0],
                    lstack[lstack.length - 1].range[1]
                ];
            }
            r = this.performAction.apply(yyval, [
                yytext,
                yyleng,
                yylineno,
                sharedState.yy,
                action[1],
                vstack,
                lstack
            ].concat(args));
            if (typeof r !== 'undefined') {
                return r;
            }
            if (len) {
                stack = stack.slice(0, -1 * len * 2);
                vstack = vstack.slice(0, -1 * len);
                lstack = lstack.slice(0, -1 * len);
            }
            stack.push(this.productions_[action[1]][0]);
            vstack.push(yyval.$);
            lstack.push(yyval._$);
            newState = table[stack[stack.length - 2]][stack[stack.length - 1]];
            stack.push(newState);
            break;
        case 3:
            return true;
        }
    }
    return true;
}};


function Parser () {
  this.yy = {};
}
Parser.prototype = parser;parser.Parser = Parser;
return new Parser;
})();


if (typeof require !== 'undefined' && typeof exports !== 'undefined') {
exports.parser = parser;
exports.Parser = parser.Parser;
exports.parse = function () { return parser.parse.apply(parser, arguments); };
exports.main = function commonjsMain(args) {
    if (!args[1]) {
        console.log('Usage: '+args[0]+' FILE');
        process.exit(1);
    }
    var source = require('fs').readFileSync(require('path').normalize(args[1]), "utf8");
    return exports.parser.parse(source);
};
if (typeof module !== 'undefined' && require.main === module) {
  exports.main(process.argv.slice(1));
}
}
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

function glsl_state(options) {
	var i;
	
	this.options = {
		target : 0,
		language_version : 100,
	};
	
	for (i in options) {
		this.options[i] = options[i];	
	}

	this.symbols = new SymbolTable();
	this.translation_unit = [];

	this.info_log = [];
	this.error = false;
}

proto = glsl_state.prototype = {};

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

	this.error = true;
	this.info_log.push(err);
};


/**
 * Jison parser compatibility
 */
glsl.parse = function(src, options) {
	var result, state;

	state = new glsl_state(options);

	symbol_table_init(state);

	parser.yy =  {
		test : 1,
		state : state
	};

	try {
		result = parser.parse(src);
	} catch(e) {
		state.addError(e.message, e.lineNumber, e.columnNumber);
	}

	return state;
};
	

/*
Copyright (c) 2014 Cimaron Shanahan

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

/* generated by jison-lex 0.3.4 */
var lexer = (function(){
var lexer = ({

EOF:1,

parseError:function parseError(str, hash) {
        if (this.yy.parser) {
            this.yy.parser.parseError(str, hash);
        } else {
            throw new Error(str);
        }
    },

// resets the lexer, sets new input
setInput:function (input, yy) {
        this.yy = yy || this.yy || {};
        this._input = input;
        this._more = this._backtrack = this.done = false;
        this.yylineno = this.yyleng = 0;
        this.yytext = this.matched = this.match = '';
        this.conditionStack = ['INITIAL'];
        this.yylloc = {
            first_line: 1,
            first_column: 0,
            last_line: 1,
            last_column: 0
        };
        if (this.options.ranges) {
            this.yylloc.range = [0,0];
        }
        this.offset = 0;
        return this;
    },

// consumes and returns one char from the input
input:function () {
        var ch = this._input[0];
        this.yytext += ch;
        this.yyleng++;
        this.offset++;
        this.match += ch;
        this.matched += ch;
        var lines = ch.match(/(?:\r\n?|\n).*/g);
        if (lines) {
            this.yylineno++;
            this.yylloc.last_line++;
        } else {
            this.yylloc.last_column++;
        }
        if (this.options.ranges) {
            this.yylloc.range[1]++;
        }

        this._input = this._input.slice(1);
        return ch;
    },

// unshifts one char (or a string) into the input
unput:function (ch) {
        var len = ch.length;
        var lines = ch.split(/(?:\r\n?|\n)/g);

        this._input = ch + this._input;
        this.yytext = this.yytext.substr(0, this.yytext.length - len);
        //this.yyleng -= len;
        this.offset -= len;
        var oldLines = this.match.split(/(?:\r\n?|\n)/g);
        this.match = this.match.substr(0, this.match.length - 1);
        this.matched = this.matched.substr(0, this.matched.length - 1);

        if (lines.length - 1) {
            this.yylineno -= lines.length - 1;
        }
        var r = this.yylloc.range;

        this.yylloc = {
            first_line: this.yylloc.first_line,
            last_line: this.yylineno + 1,
            first_column: this.yylloc.first_column,
            last_column: lines ?
                (lines.length === oldLines.length ? this.yylloc.first_column : 0)
                 + oldLines[oldLines.length - lines.length].length - lines[0].length :
              this.yylloc.first_column - len
        };

        if (this.options.ranges) {
            this.yylloc.range = [r[0], r[0] + this.yyleng - len];
        }
        this.yyleng = this.yytext.length;
        return this;
    },

// When called from action, caches matched text and appends it on next action
more:function () {
        this._more = true;
        return this;
    },

// When called from action, signals the lexer that this rule fails to match the input, so the next matching rule (regex) should be tested instead.
reject:function () {
        if (this.options.backtrack_lexer) {
            this._backtrack = true;
        } else {
            return this.parseError('Lexical error on line ' + (this.yylineno + 1) + '. You can only invoke reject() in the lexer when the lexer is of the backtracking persuasion (options.backtrack_lexer = true).\n' + this.showPosition(), {
                text: "",
                token: null,
                line: this.yylineno
            });

        }
        return this;
    },

// retain first n characters of the match
less:function (n) {
        this.unput(this.match.slice(n));
    },

// displays already matched input, i.e. for error messages
pastInput:function () {
        var past = this.matched.substr(0, this.matched.length - this.match.length);
        return (past.length > 20 ? '...':'') + past.substr(-20).replace(/\n/g, "");
    },

// displays upcoming input, i.e. for error messages
upcomingInput:function () {
        var next = this.match;
        if (next.length < 20) {
            next += this._input.substr(0, 20-next.length);
        }
        return (next.substr(0,20) + (next.length > 20 ? '...' : '')).replace(/\n/g, "");
    },

// displays the character position where the lexing error occurred, i.e. for error messages
showPosition:function () {
        var pre = this.pastInput();
        var c = new Array(pre.length + 1).join("-");
        return pre + this.upcomingInput() + "\n" + c + "^";
    },

// test the lexed token: return FALSE when not a match, otherwise return token
test_match:function (match, indexed_rule) {
        var token,
            lines,
            backup;

        if (this.options.backtrack_lexer) {
            // save context
            backup = {
                yylineno: this.yylineno,
                yylloc: {
                    first_line: this.yylloc.first_line,
                    last_line: this.last_line,
                    first_column: this.yylloc.first_column,
                    last_column: this.yylloc.last_column
                },
                yytext: this.yytext,
                match: this.match,
                matches: this.matches,
                matched: this.matched,
                yyleng: this.yyleng,
                offset: this.offset,
                _more: this._more,
                _input: this._input,
                yy: this.yy,
                conditionStack: this.conditionStack.slice(0),
                done: this.done
            };
            if (this.options.ranges) {
                backup.yylloc.range = this.yylloc.range.slice(0);
            }
        }

        lines = match[0].match(/(?:\r\n?|\n).*/g);
        if (lines) {
            this.yylineno += lines.length;
        }
        this.yylloc = {
            first_line: this.yylloc.last_line,
            last_line: this.yylineno + 1,
            first_column: this.yylloc.last_column,
            last_column: lines ?
                         lines[lines.length - 1].length - lines[lines.length - 1].match(/\r?\n?/)[0].length :
                         this.yylloc.last_column + match[0].length
        };
        this.yytext += match[0];
        this.match += match[0];
        this.matches = match;
        this.yyleng = this.yytext.length;
        if (this.options.ranges) {
            this.yylloc.range = [this.offset, this.offset += this.yyleng];
        }
        this._more = false;
        this._backtrack = false;
        this._input = this._input.slice(match[0].length);
        this.matched += match[0];
        token = this.performAction.call(this, this.yy, this, indexed_rule, this.conditionStack[this.conditionStack.length - 1]);
        if (this.done && this._input) {
            this.done = false;
        }
        if (token) {
            return token;
        } else if (this._backtrack) {
            // recover context
            for (var k in backup) {
                this[k] = backup[k];
            }
            return false; // rule action called reject() implying the next rule should be tested instead.
        }
        return false;
    },

// return next match in input
next:function () {
        if (this.done) {
            return this.EOF;
        }
        if (!this._input) {
            this.done = true;
        }

        var token,
            match,
            tempMatch,
            index;
        if (!this._more) {
            this.yytext = '';
            this.match = '';
        }
        var rules = this._currentRules();
        for (var i = 0; i < rules.length; i++) {
            tempMatch = this._input.match(this.rules[rules[i]]);
            if (tempMatch && (!match || tempMatch[0].length > match[0].length)) {
                match = tempMatch;
                index = i;
                if (this.options.backtrack_lexer) {
                    token = this.test_match(tempMatch, rules[i]);
                    if (token !== false) {
                        return token;
                    } else if (this._backtrack) {
                        match = false;
                        continue; // rule action called reject() implying a rule MISmatch.
                    } else {
                        // else: this is a lexer rule which consumes input without producing a token (e.g. whitespace)
                        return false;
                    }
                } else if (!this.options.flex) {
                    break;
                }
            }
        }
        if (match) {
            token = this.test_match(match, rules[index]);
            if (token !== false) {
                return token;
            }
            // else: this is a lexer rule which consumes input without producing a token (e.g. whitespace)
            return false;
        }
        if (this._input === "") {
            return this.EOF;
        } else {
            return this.parseError('Lexical error on line ' + (this.yylineno + 1) + '. Unrecognized text.\n' + this.showPosition(), {
                text: "",
                token: null,
                line: this.yylineno
            });
        }
    },

// return next match that has a token
lex:function lex() {
        var r = this.next();
        if (r) {
            return r;
        } else {
            return this.lex();
        }
    },

// activates a new lexer condition state (pushes the new lexer condition state onto the condition stack)
begin:function begin(condition) {
        this.conditionStack.push(condition);
    },

// pop the previously active lexer condition state off the condition stack
popState:function popState() {
        var n = this.conditionStack.length - 1;
        if (n > 0) {
            return this.conditionStack.pop();
        } else {
            return this.conditionStack[0];
        }
    },

// produce the lexer rule set which is active for the currently active lexer condition state
_currentRules:function _currentRules() {
        if (this.conditionStack.length && this.conditionStack[this.conditionStack.length - 1]) {
            return this.conditions[this.conditionStack[this.conditionStack.length - 1]].rules;
        } else {
            return this.conditions["INITIAL"].rules;
        }
    },

// return the currently active lexer condition state; when an index argument is provided it produces the N-th previous condition state, if available
topState:function topState(n) {
        n = this.conditionStack.length - 1 - Math.abs(n || 0);
        if (n >= 0) {
            return this.conditionStack[n];
        } else {
            return "INITIAL";
        }
    },

// alias for begin(condition)
pushState:function pushState(condition) {
        this.begin(condition);
    },

// return the number of states currently on the stack
stateStackSize:function stateStackSize() {
        return this.conditionStack.length;
    },
options: {},
performAction: function anonymous(yy,yy_,$avoiding_name_collisions,YY_START) {
var YYSTATE=YY_START;
switch($avoiding_name_collisions) {
case 0:;
break;
case 1:
break;
case 2: this.begin('PP'); return 9; 
break;
case 3: this.begin('PP'); return 22; 
break;
case 4:
	
	/* Eat characters until the first digit is
	 * encountered
	 */
	
	var ptr = 0;
	while (yy_.yytext.slice(0, 1) < '0' || yy_.yytext.slice(0, 1) > '9') {
		ptr++;
	}

	/* Subtract one from the line number because
	 * yy_.yylineno is zero-based instead of
	 * one-based.
	 */
	yy_.yylineno = parseInt(yy_.yytext.slice(0, 1), 10) - 1;
	yy_.yylloc.source = parseInt(yy_.yytext.slice(0), 10);

break;
case 5:
				   /* Eat characters until the first digit is
				    * encountered
				    */
					var ptr = 0;
					while (yy_.yytext.slice(0, 1) < '0' || yy_.yytext.slice(0, 1) > '9')
						ptr++;

				   /* Subtract one from the line number because
				    * yy_.yylineno is zero-based instead of
				    * one-based.
				    */
				   yy_.yylineno = parseInt(yy_.yytext.slice(0, 1), 10) - 1;
				
break;
case 6:
				  this.begin('PP');
				  return 13;
				
break;
case 7:
				  this.begin('PP');
				  return 14;
				
break;
case 8:
				  this.begin('PP');
				  return 15;
				
break;
case 9:
				  this.begin('PP');
				  return 16;
				
break;
case 10:
				  this.begin('PP');
				  return 17;
				
break;
case 11: this.begin('PRAGMA'); 
break;
case 12: this.begin('INITIAL'); yy_.yylineno++; yycolumn = 0; 
break;
case 13: 
break;
case 14: 
break;
case 15: 
break;
case 16:return ":";
break;
case 17:
				   yylval.identifier = strdup(yy_.yytext);
				   return 25;
				
break;
case 18:
				    yylval.n = parseInt(yy_.yytext);
				    return 10;
				
break;
case 19: this.begin('INITIAL'); yy_.yylineno++; yycolumn = 0; return 11; 
break;
case 20: /*yy_.yylineno++; yycolumn = 0;*/ 
break;
case 21:return 137;
break;
case 22:return 135;
break;
case 23:return 148;
break;
case 24:return 144;
break;
case 25:return 146;
break;
case 26:return 204;
break;
case 27:return 203;
break;
case 28:return 198;
break;
case 29:return 197;
break;
case 30:return 192;
break;
case 31:return 199;
break;
case 32:return 190;
break;
case 33:return 206;
break;
case 34:return 205;
break;
case 35:return 152;
break;
case 36:return 153;
break;
case 37:return 154;
break;
case 38:return 155;
break;
case 39:return 156;
break;
case 40:return 157;
break;
case 41:return 149;
break;
case 42:return 150;
break;
case 43:return 151;
break;
case 44:return 158;
break;
case 45:return 159;
break;
case 46:return 160;
break;
case 47:return 120;
break;
case 48:return 121;
break;
case 49:return 122;
break;
case 50:return 140;
break;
case 51:return 138;
break;
case 52:return 125;
break;
case 53:return 133;
break;
case 54:return 132;
break;
case 55:return 161;
break;
case 56:return 162;
break;
case 57:return 163;
break;
case 58:return 164;
break;
case 59:return 165;
break;
case 60:return 166;
break;
case 61:return 170;
break;
case 62:return 47;
break;
case 63:/*copy manually*/
break;
case 64:return 39;
break;
case 65:return 40;
break;
case 66:return 72;
break;
case 67:return 73;
break;
case 68:return 75;
break;
case 69:return 76;
break;
case 70:return 84;
break;
case 71:return 88;
break;
case 72:return 86;
break;
case 73:return 67;
break;
case 74:return 68;
break;
case 75:return 93;
break;
case 76:return 94;
break;
case 77:return 96;
break;
case 78:return 95;
break;
case 79:return 98;
break;
case 80:return 99;
break;
case 81:return 100;
break;
case 82:return 101;
break;
case 83:return 102;
break;
case 84:return 97;
break;
case 85:
			    this.yylval = parseFloat(yy_.yytext);
			    return 28;
			
break;
case 86:
				this.yylval = parseFloat(yy_.yytext);
				return 28;
			
break;
case 87:
			    this.yylval = parseFloat(yy_.yytext);
			    return 28;
			
break;
case 88:
			    this.yylval = parseFloat(yy_.yytext);
			    return 28;
			
break;
case 89:
			    this.yylval = parseFloat(yy_.yytext);
			    return 28;
			
break;
case 90:
			    this.yylval = parseInt(yy_.yytext + 2, 16);
			    return 'INTCONSTANT';
			
break;
case 91:
			    this.yylval = parseInt(yy_.yytext, 8);
			    return 'INTCONSTANT';
			
break;
case 92:
				this.yylval = parseInt(yy_.yytext);
				return 'INTCONSTANT';
			
break;
case 93:
			    this.yylval = 1;
			    return 29;
			
break;
case 94:
			    this.yylval = 0;
			    return 29;
			
break;
case 95:return 'ASM'
break;
case 96:return 'CLASS'
break;
case 97:return 'UNION'
break;
case 98:return 'ENUM'
break;
case 99:return 'TYPEDEF'
break;
case 100:return 'TEMPLATE'
break;
case 101:return 'THIS'
break;
case 102:return 'PACKED'
break;
case 103:return 'GOTO'
break;
case 104:return 194
break;
case 105:return 196
break;
case 106:return 'INLINE'
break;
case 107:return 'NOINLINE'
break;
case 108:return 'VOLATILE'
break;
case 109:return 'PUBLIC'
break;
case 110:return 'STATIC'
break;
case 111:return 'EXTERN'
break;
case 112:return 'EXTERNAL'
break;
case 113:return 'INTERFACE'
break;
case 114:return 'LONG'
break;
case 115:return 'SHORT'
break;
case 116:return 145
break;
case 117:return 'HALF'
break;
case 118:return 'FIXED'
break;
case 119:return 'UNSIGNED'
break;
case 120:return 'INPUT'
break;
case 121:return 'OUTPUT'
break;
case 122:return 'HVEC2'
break;
case 123:return 'HVEC3'
break;
case 124:return 'HVEC4'
break;
case 125:return 'DVEC2'
break;
case 126:return 'DVEC3'
break;
case 127:return 'DVEC4'
break;
case 128:return 'FVEC2'
break;
case 129:return 'FVEC3'
break;
case 130:return 'FVEC4'
break;
case 131:return 'SAMPLER2DRECT';
break;
case 132:return 'SAMPLER3DRECT';
break;
case 133:return 'SAMPLER2DRECTSHADOW';
break;
case 134:return 'SIZEOF';
break;
case 135:return 'CAST';
break;
case 136:return 'NAMESPACE';
break;
case 137:return 'USING';
break;
case 138:return 169;
break;
case 139:return 168;
break;
case 140:return 167;
break;
case 141:return 108;
break;
case 142:
	yy.yylval = yy_.yytext;
	return yy.state.classify_identifier(yy.state, yy_.yytext);

break;
case 143:return yy_.yytext;
break;
case 144:return 5;
break;
}
},
rules: [/^(?:[ \r\t]+)/,/^(?:[ \t]*#[ \t]*$)/,/^(?:[ \t]*#[ \t]*version\b)/,/^(?:[ \t]*#[ \t]*extension\b)/,/^(?:(^([ \t]*)([ \t]*))line([ \t]+)((([1-9][0-9]*)|([xX][0-9a-fA-F]+)|([0-7]*)))([ \t]+)((([1-9][0-9]*)|([xX][0-9a-fA-F]+)|([0-7]*)))([ \t]*)$)/,/^(?:(^([ \t]*)([ \t]*))line([ \t]+)((([1-9][0-9]*)|([xX][0-9a-fA-F]+)|([0-7]*)))([ \t]*)$)/,/^(?:([ \t]*)#([ \t]*)pragma([ \t]+)debug([ \t]*)\(([ \t]*)on([ \t]*)\))/,/^(?:([ \t]*)#([ \t]*)pragma([ \t]+)debug([ \t]*)\(([ \t]*)off([ \t]*)\))/,/^(?:([ \t]*)#([ \t]*)pragma([ \t]+)optimize([ \t]*)\(([ \t]*)on([ \t]*)\))/,/^(?:([ \t]*)#([ \t]*)pragma([ \t]+)optimize([ \t]*)\(([ \t]*)off([ \t]*)\))/,/^(?:([ \t]*)#([ \t]*)pragma([ \t]+)STDGL([ \t]+)invariant([ \t]*)\(([ \t]*)all([ \t]*)\))/,/^(?:([ \t]*)#([ \t]*)pragma([ \t]+))/,/^(?:[\n])/,/^(?:.)/,/^(?:\/\/[^\n]*)/,/^(?:[ \t\r]*)/,/^(?::)/,/^(?:[_a-zA-Z][_a-zA-Z0-9]*)/,/^(?:[1-9][0-9]*)/,/^(?:[\n])/,/^(?:[\n])/,/^(?:attribute\b)/,/^(?:const\b)/,/^(?:bool\b)/,/^(?:float\b)/,/^(?:int\b)/,/^(?:break\b)/,/^(?:continue\b)/,/^(?:do\b)/,/^(?:while\b)/,/^(?:else\b)/,/^(?:for\b)/,/^(?:if\b)/,/^(?:discard\b)/,/^(?:return\b)/,/^(?:bvec2\b)/,/^(?:bvec3\b)/,/^(?:bvec4\b)/,/^(?:ivec2\b)/,/^(?:ivec3\b)/,/^(?:ivec4\b)/,/^(?:vec2\b)/,/^(?:vec3\b)/,/^(?:vec4\b)/,/^(?:mat2\b)/,/^(?:mat3\b)/,/^(?:mat4\b)/,/^(?:in\b)/,/^(?:out\b)/,/^(?:inout\b)/,/^(?:uniform\b)/,/^(?:varying\b)/,/^(?:invariant\b)/,/^(?:flat\b)/,/^(?:smooth\b)/,/^(?:sampler1D\b)/,/^(?:sampler2D\b)/,/^(?:sampler3D\b)/,/^(?:samplerCube\b)/,/^(?:sampler1DShadow\b)/,/^(?:sampler2DShadow\b)/,/^(?:struct\b)/,/^(?:void\b)/,/^(?:layout\b)/,/^(?:\+\+)/,/^(?:--)/,/^(?:<=)/,/^(?:>=)/,/^(?:==)/,/^(?:!=)/,/^(?:&&)/,/^(?:\|\|)/,/^(?:\^\^)/,/^(?:<<)/,/^(?:>>)/,/^(?:\*=)/,/^(?:\/=)/,/^(?:\+=)/,/^(?:%=)/,/^(?:<<=)/,/^(?:>>=)/,/^(?:&=)/,/^(?:\^=)/,/^(?:\|=)/,/^(?:-=)/,/^(?:[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?[fF]?)/,/^(?:\.[0-9]+([eE][+-]?[0-9]+)?[fF]?)/,/^(?:[0-9]+\.([eE][+-]?[0-9]+)?[fF]?)/,/^(?:[0-9]+[eE][+-]?[0-9]+[fF]?)/,/^(?:[0-9]+[fF])/,/^(?:0[xX][0-9a-fA-F]+)/,/^(?:0[0-7]*)/,/^(?:[1-9][0-9]*)/,/^(?:true\b)/,/^(?:false\b)/,/^(?:asm\b)/,/^(?:class\b)/,/^(?:union\b)/,/^(?:enum\b)/,/^(?:typedef\b)/,/^(?:template\b)/,/^(?:this\b)/,/^(?:packed\b)/,/^(?:goto\b)/,/^(?:switch\b)/,/^(?:default\b)/,/^(?:inline\b)/,/^(?:noinline\b)/,/^(?:volatile\b)/,/^(?:public\b)/,/^(?:static\b)/,/^(?:extern\b)/,/^(?:external\b)/,/^(?:interface\b)/,/^(?:long\b)/,/^(?:short\b)/,/^(?:double\b)/,/^(?:half\b)/,/^(?:fixed\b)/,/^(?:unsigned\b)/,/^(?:input\b)/,/^(?:output\b)/,/^(?:hvec2\b)/,/^(?:hvec3\b)/,/^(?:hvec4\b)/,/^(?:dvec2\b)/,/^(?:dvec3\b)/,/^(?:dvec4\b)/,/^(?:fvec2\b)/,/^(?:fvec3\b)/,/^(?:fvec4\b)/,/^(?:sampler2DRect\b)/,/^(?:sampler3DRect\b)/,/^(?:sampler2DRectShadow\b)/,/^(?:sizeof\b)/,/^(?:cast\b)/,/^(?:namespace\b)/,/^(?:using\b)/,/^(?:lowp\b)/,/^(?:mediump\b)/,/^(?:highp\b)/,/^(?:precision\b)/,/^(?:[_a-zA-Z][_a-zA-Z0-9]*)/,/^(?:.)/,/^(?:$)/],
conditions: {"PRAGMA":{"rules":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144],"inclusive":true},"PP":{"rules":[0,1,2,3,4,5,6,7,8,9,10,11,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144],"inclusive":true},"INITIAL":{"rules":[0,1,2,3,4,5,6,7,8,9,10,11,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144],"inclusive":true}}
});
return lexer;
})();

parser.lexer = lexer;


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
CONNECTION WITH THE SOFTWARE OR THE USE		 OR OTHER DEALINGS IN THE SOFTWARE.
*/


/**
 * Constructs a program's object code from an ast and symbol table
 *
 * @param   string     The error message
 * @param   AstNode    The error AstNode
 *
 * @return  string
 */
glsl.generate = function(state) {

	var irs = new Ir(state.options.target);

	try {
		for (var i = 0; i < state.translation_unit.length; i++) {
			state.translation_unit[i].ir(state, irs);
		}

	} catch (e) {
		if (!e.ir) {
			e.message = "compiler error: " + e.message;
		}
		state.addError(e.message, e.lineNumber, e.columnNumber);
	}

	if (state.error) {
		return false;	
	}

	return irs;
};

/**
 * Constructs an error message
 *
 * @param   string     The error message
 * @param   AstNode    The error AstNode
 *
 * @return  string
 */
function ir_error(message, node) {
	var e = new IrError();

	if (node && node.location) {
		e.lineNumber = node.location.first_line;
		e.columnNumber = node.location.first_column;
		e.message = message;
	}

	throw e;
}

/**
 * Default IR
 */
AstNode.prototype.irx = function(state, irs) {
	ir_error(util.format("Can't generate ir for %s", this.typeOf()), this);
};

/**
 * Constructs a type specifier code block
 *
 * @param   object   state    parser state
 */
AstTypeSpecifier.prototype.ir = function(state, irs) {

	if (this.is_precision_statement) {
		return;
	}

//	ir_error("Cannot generate type specifier", this);
};


/**
 * Constructs a declaration list
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstDeclaratorList.prototype.ir = function(state, irs) {
	var type, qualifier, i, decl, name, entry, constant, assign, lhs;

	type = this.type;
	if (type.qualifier) {
		qualifier = type.qualifier.flags;
	}

	for (i = 0; i < this.declarations.length; i++) {

		decl = this.declarations[i];
		name = decl.identifier;

		//add symbol table entry
		entry = state.symbols.add_variable(name);
		entry.type = type.specifier.type_name;
		entry.qualifier = qualifier;

		if (entry.qualifier & AstTypeQualifier.flags.uniform) {
			entry.out = irs.getUniform(entry);
		} else if (entry.qualifier & AstTypeQualifier.flags.attribute) {
			entry.out = irs.getAttribute(entry);
		} else if (entry.qualifier & AstTypeQualifier.flags.varying) {
			entry.out = irs.getVarying(entry);
		} else {
			entry.out = irs.getTemp();			
		}

		constant = (qualifier & AstTypeQualifier.flags.constant);

		if (decl.initializer) {

			//@todo: generate constants at compile time (this may be able to be taken care of in the generator)
			if (constant) {
				//entry.constant = decl.initializer.Dest;
			} else {
				lhs = new AstExpression('ident');
				lhs.primary_expression.identifier = name;
				assign = new AstExpression('=', lhs, decl.initializer);
				assign.ir(state, irs);
			}

		} else {
			if (constant) {
				ir_error("Declaring const without initialier", decl);
			}
		}
	}
};


/**
 * Constructs a function definition block
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstFunctionDefinition.prototype.ir = function(state, irs) {

	if (this.is_definition) {
		//enter definition into symbol table?
		return;
	}

	//handle function proto
	this.proto_type.ir(state, irs);

	//handle function body
	this.body.ir(state, irs);

	irs.push(new IrInstruction('RET'));
};


/**
 * Constructs a function header code block
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstFunction.prototype.ir = function(state, irs) {
	var i, name, param, entry;

	//generate
	name = this.identifier;
	entry = state.symbols.get_function(name);

	//generate param list
	for (i = 0; i < this.parameters.length; i++) {
		param = this.parameters[i];
		if (param.is_void || !param.identifier) {
			break;
		}
	}
};


/**
 * Constructs a compound statement code block
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstCompoundStatement.prototype.ir = function(state, irs) {
	var i;

	state.symbols.push_scope();

	for (i = 0; i < this.statements.length; i++) {
		this.statements[i].ir(state, irs);
	}

	state.symbols.pop_scope();
};


/**
 * Constructs an expression statement code block
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstExpressionStatement.prototype.ir = function(state, irs) {
	this.expression.ir(state, irs);
};




/**
 * Constructs an expression code block
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstExpression.prototype.ir = function(state, irs) {
	var i;

	//simple (variable, or value)
	for (i in this.primary_expression) {
		return this.ir_simple(state, irs);
	}

	//operator
	if (this.oper) {
		return this.ir_op(state, irs);
	}

	//cast
	if (this.constructor.name ==  'AstTypeSpecifier') {
		this.Type = this.type_specifier;
		return;
	}

	ir_error("Could not translate unknown expression type", e);
};



/**
 * Constructs an operator expression code block
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstExpression.prototype.ir_op = function(state, irs) {
	var se, temp, ops;

	if (se = this.subexpressions) {
		se[0] ? se[0].ir(state, irs) : null;
		se[1] ? se[1].ir(state, irs) : null;
		se[2] ? se[2].ir(state, irs) : null;
	}
	
	switch (this.oper) {

		//case '+=':
		case '=':
			this.ir_assign(state, irs);
			break;

		//case 'POS':
		case 'NEG':
			if (se[0].Dest[0] != '-') {
				e.Dest = "-" + se[0].Dest;	
			} else {
				e.Dest = se[0].Dest.substring(1);	
			}
			e.Type = se[0].Type;
			break;

		//binary expression
		case '+':
		case '-':
		case '*':
		case '/':
		case '%':
		case '<<':
		case '>>':
		case '<':
		case '>':
		case '<=':
		case '>=':
		case '==':
		case '!-':
		case '&':
		case '^':
		case '|':
		case '~':
		case '&&':
		case '^^':
		case '||':
			this.ir_generate(state, irs, 2);
			break;

		case '!':
			this.ir_generate(state, irs, 1);
			break;

		/*
		case '*=':
		case '/=':
		case '%=':
		case '+=':
		case '-=':
		case '<<=':
		case '>>=':
		case '&=':
		case '^=':
		case '|=':
			break;
		case '?:':
			break;
		case '++_':
		case '--_':
			break;
		case '_++':
		case '_--':
			break;
			*/
		//case '.': break;
		case '[]':
			break;
		/*
		case 'VAR':
		case 'int':
		case 'float':
		case 'bool':
			ir_expression_simple(e, se);
			break;
		*/
		default:
			ir_error(util.format("Could not translate unknown expression %s (%s)", this, this.oper), this);
	}
};


/**
 * Constructs an assignment expression
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstExpression.prototype.ir_assign = function(state, irs, local) {
	var cond, ir, temp, size, slots, swz, i, entry, lhs, rhs;

	lhs = this.subexpressions[0];
	rhs = this.subexpressions[1];

	if (this.oper == '+=') {
		rhs.oper = '+';
		//ir_expression_generate(rhs, [lhs, rhs], 2);
	}

	/*
	if (conditional.length > 0) {
		cond = conditional[conditional.length - 1];	
	}
	*/

	if (lhs.Type != rhs.Type) {
		ir_error(util.format("Could not assign value of type %s to %s", rhs.Type, lhs.Type), this);
	}
	this.Type = lhs.Type;

	if (lhs.Entry && lhs.Entry.constant) {
		ir_error(util.format("Cannot assign value to constant %s", lhs.Dest), this);	
	}

	irs.push(new IrComment(util.format("(%s = %s) => %s %s", lhs.Dest, rhs.Dest, lhs.Type, lhs.Dest), this.location));

	size = types[this.Type].size;
	slots = types[this.Type].slots;

	//get the swizzle for each slot
	swz = Ir.swizzles[0].substring(0, 4 - (((slots * 4) - size) / slots));

	//all components are used up in all slots
	if (swz == Ir.swizzles[0]) {
		swz = "";
	}

	for (i = 0; i < slots; i++) {
		/*
		if (cond && !local) {
			ir = new IR('CMP', se[0].Dest, "-" + cond, se[1].Dest, se[0].Dest);
			ir.addOffset(i);
			ir.setSwizzle(swz);
			irs.push(ir);

		} else {
		*/
			ir = new IrInstruction('MOV', lhs.Dest, rhs.Dest);
			ir.addOffset(i);
			ir.setSwizzle(swz);
			irs.push(ir);
		/*
		}
		*/
	}
};


/**
 * Constructs a simple expression code block
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstExpression.prototype.ir_simple = function(state, irs) {
	var name, entry, t;

	if (this.oper == '.') {
		this.ir_field(state, irs);
		return;
	}

	//identifier
	if (name = this.primary_expression.identifier) {

		//lookup identifier in symbol table
		entry = state.symbols.get_variable(name) || state.symbols.get_function(name);

		if (!entry || !entry.type) {
			ir_error(util.format("%s is undefined", name), this);
		}

		this.Type = entry.type;
		this.Entry = entry;

		if (entry.constant) {
			this.Dest = entry.constant;
		} else {
			this.Dest = entry.out;
		}

		return;
	}

	//float constant
	if ('float_constant' in this.primary_expression) {
		this.Type = 'float';
		this.Dest = this.makeFloat(this.primary_expression.float_constant);
		return;
	}

	//int constant
	if ('int_constant' in this.primary_expression) {
		this.Type = 'int';
		this.Dest = this.makeFloat(e.primary_expression.int_constant);
		return;
	}

	ir_error("Cannot translate unknown simple expression type", e);
};

/**
 * Constructs the code for an expression
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstExpression.prototype.ir_generate = function(state, irs, len) {
	var table, se, types, dest, i, j, def, match, comment;

	if (!(table = builtin.oper[this.oper])) {
		ir_error(util.format("Could not generate operation %s", this.oper), this);
	}

	this.Dest = irs.getTemp();

	types = [];
	dest = [this.Dest];
	se = this.subexpressions;

	for (i = 0; i < len; i++) {
		types.push(se[i].Type);
		dest.push(se[i].Dest);
	}

	def = new RegExp(types.join(",") + "\:(.*)");
	for (j in table) {
		if (match = j.match(def)) {
			this.Type = match[1];
			break;
		}
	}

	if (!match) {
		ir_error(util.format("Could not apply operation %s to %s", this.oper, types.join(", ")), this);
	}

	if (len <= 4) {
		//this.Dest += util.format(".%s", swizzles[0].substring(0, glsl.type.size[this.Type]));
	}

	if (len == 1) {
		comment = util.format("(%s %s) => %s %s", this.oper, se[0].Dest, this.Type, this.Dest);
	} else if (len == 2) {
		comment = util.format("(%s %s %s) => %s %s", se[0].Dest, this.oper, se[1].Dest, this.Type, this.Dest);
	} else if (len == 3) {
		comment = util.format("(%s ? %s : %s) => %s %s", se[0].Dest, se[1].Dest, se[2].Dest, this.Type, this.Dest);
	}

	irs.push(new IrComment(comment, this.location));

	irs.build(table[j], dest);
};


/**
 * Constructs a function expression
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstFunctionExpression.prototype.ir = function(state, irs) {
	var i, func, se, def, dest, entry;

	//Prepare the dest
	this.subexpressions[0].ir(state, irs);

	if (this.cons) {
		return this.ir_constructor(state, irs);
	}

	func = this.subexpressions[0].Dest;
	def = [];
	dest = [];

	for (i = 0; i < this.expressions.length; i++) {

		se = this.expressions[i];
		se.ir(state, irs);

		def.push(se.Type);
		dest.push(se.Dest);
	}

	entry = state.symbols.get_function(func, null, def);
	if (!entry) {
		ir_error(util.format("Function %s(%s) is not defined", func, def_names.join(",")), this);
	}

	this.Type = entry.type;
	this.Dest = irs.getTemp();
	
	irs.push(new IrComment(util.format("%s(%s) => %s %s", entry.name, dest.join(", "), this.Type, this.Dest), this.location));

	dest.unshift(this.Dest);

	irs.build(entry.code, dest);
};


/**
 * Constructs a type constructor
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstFunctionExpression.prototype.ir_constructor = function(state, irs) {
	var type, dest_i, si, sei, ses, d, s, expr, comment, comment_text;

	type = this.subexpressions[0].type_specifier;

	si = 0;
	sei = 0;

	this.Type = type.name;
	this.Dest = irs.getTemp();

	comment_text = [];
	comment = new IrComment("", this.location);
	irs.push(comment);

	for (dest_i = 0; dest_i < type.size; dest_i++) {

		expr = this.expressions[sei];

		//build next subexpression
		if (si == 0) {

			if (!expr) {
				ir_error("Not enough parameters to constructor", e);				
			}

			expr.ir(state, irs);
			ses = types[expr.Type].size;

			comment_text.push(expr.Dest);
		}

		//need to add support for > vec4

		//compute destination
		d = util.format("%s.%s", this.Dest, Ir.swizzles[0][dest_i]);

		//compute source
		s = new IrOperand(expr.Dest);

		//expression was to just get the identifier, so add the appropriate swizzle,
		//else, either a number, or the correct swizzle already been set

		if (!s.swizzle) {
			s.swizzle = Ir.swizzles[0][si];			
		}

		irs.push(new IrInstruction('MOV', d, s.toString()));

		//used up all components in current expression, move on to the next one
		si++;
		if (si >= ses) {
			si = 0;
			sei++;
		}

	}

	comment.comment = util.format("%s(%s) => %s %s", this.Type, comment_text.join(", "), this.Type, this.Dest);
};


/**
 * Constructs a field selection code block
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstExpression.prototype.ir_field = function(state, irs) {
	var field, swz, base, se;

	//pick swizzle set
	field = this.primary_expression.identifier;

	se = this.subexpressions[0];
	se.ir(state, irs);

	if (Ir.isSwizzle(field)) {

		base = types[se.Type].base;
		if (field.length > 1) {
			if (base == 'int') {
				base = 'ivec' + field.length;	
			}
			if (base == 'bool') {
				base = 'bvec' + field.length;	
			}
			if (base == 'float') {
				base = 'vec' + field.length;	
			}
		}

		this.Type = base;

		if (field.length > 4 || !this.Type) {
			ir_error(util.format("Invalid field selection %s.%s", se, field), this);
		}

		this.Dest = util.format("%s.%s", se.Dest, Ir.normalizeSwizzle(field));
	}
}


/**
 * Constructs a selection statement
 *
 * @param   ast_node    Statement
 */
AstSelectionStatement.prototype.ir = function(state, irs) {
	var ir, cond;

	this.condition.ir(state, irs);
	//@todo: add a check that condition is bool type?

	irs.push(new IrComment(util.format("if %s then", this.condition.Dest), this.location));

	//set a flag based on the result
	ir = new IrInstruction('IF', this.condition.Dest);
	irs.push(ir);

	this.then_statement.ir(state, irs);

	if (this.else_statement) {

		irs.push(new IrInstruction('ELSE'));

		this.else_statement.ir(state, irs);
	}

	irs.push(new IrInstruction('ENDIF'));
}



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
CONNECTION WITH THE SOFTWARE OR THE USE		 OR OTHER DEALINGS IN THE SOFTWARE.
*/


/**
 * IR Class
 *
 * Stores IR code tree
 */	
function Ir(target) {

	this.target = target;

	this.symbols = {
		uniform : {
			next : 0,
			entries : {}
		},
		attribute : {
			next : 0,
			entries : {}
		},
		varying : {
			next : 0,
			entries : {}
		},
		temp : {
			next : 0
		}
	};

	this.code = [];
	this.last = null;
}

Ir.prototype.getTemp = function() {
	
	var t = 'temp@' + this.symbols.temp.next;

	this.symbols.temp.next++;
	
	return t;
};

/**
 * Add a symbol table entry into the local symbol table and return a new IR identifier
 *
 * @param   object   entry   Symbol table entry
 *
 * @return  string
 */
Ir.prototype.getUniform = function(entry) {

	var table = this.symbols.uniform, out;

	if (!table.entries[entry.name]) {
		table.entries[entry.name] = entry;
		entry.out = 'uniform@' + table.next;
		table.next += types[entry.type].slots;
	}

	return entry.out;
};

/**
 * Add a symbol table entry into the local symbol table and return a new IR identifier
 *
 * @param   object   entry   Symbol table entry
 *
 * @return  string
 */
Ir.prototype.getAttribute = function(entry) {

	var table = this.symbols.attribute, out;

	if (!table.entries[entry.name]) {
		table.entries[entry.name] = entry;
		entry.out = 'attribute@' + table.next;
		table.next += types[entry.type].slots;
	}

	return entry.out;
};

/**
 * Add a symbol table entry into the local symbol table and return a new IR identifier
 *
 * @param   object   entry   Symbol table entry
 *
 * @return  string
 */
Ir.prototype.getVarying = function(entry) {

	var table = this.symbols.varying, out;

	if (!table.entries[entry.name]) {
		table.entries[entry.name] = entry;
		entry.out = 'varying@' + table.next;
		table.next += types[entry.type].slots;
	}

	return entry.out;
};


Ir.prototype.get = function(i) {
	return this.code[i];	
};

Ir.prototype.push = function(ir) {
	this.code.push(ir);
	this.last = ir;
};

Ir.isSwizzle = function(swz) {

	if (swz.match(/[xyzw]+/)) {
		return true;	
	}

	if (swz.match(/[rgba]+/)) {
		return true;	
	}

	if (swz.match(/[stpq]+/)) {
		return true;	
	}
};

Ir.normalizeSwizzle = function(swz) {
	var n;

	if (!this.isSwizzle(swz)) {
		return null;
	}
	
	n = swz
	   .replace(/[rs]/g, 'x')
	   .replace(/[gt]/g, 'y')
	   .replace(/[bp]/g, 'z')
	   .replace(/[aq]/g, 'w')
	   ;

	return n;
};

Ir.swizzles = ["xyzw", "rgba", "stpq"];


/**
 * Replaces all instances of an operand name and base index in all instructions after start
 *
 * @param   integer     Starting instruction number
 * @param   string      Old name to search for
 * @param   string      New name to replace with
 * @param   integer     Add offset
 * @param   boolean     True if replacing with a completely new operand
 */
Ir.prototype.replaceName = function(start, old, nw, index, repl) {
	var i, j, ir, f, name, neg_const;
	neg_const = old.match(/^\-([0-9]+\.[0-9]+)/);
	if (neg_const) {
		old = neg_const[1];
		neg_const = true;
	}

	for (i = start; i < this.code.length; i++) {
		ir = this.code[i];

		//foreach each operand field
		for (j = 0; j < IR.operands.length; j++) {
			f = IR.operands[j];
			if (ir[f] && ir[f].name == old) {
				if (repl) {
					ir[f] = new Ir.Operand(ir[f].neg + nw);
				} else {
					ir[f].name = nw;
					ir[f].addOffset(index);
				}
				if (neg_const && ir[f].neg) {
					ir[f].neg = "";
				}
			}	
		}
		
	}
};
	
Ir.prototype.toString = function() {
	return this.code.join("\n");
};


/**
 * Builds instructions from code table record
 *
 * @param   array       List of instruction strings
 * @param   array       List of operands
 */
Ir.prototype.build = function(code, oprds) {
	var dest, i, j, o, n, oprd, ir, new_swz;

	//Parse operands
	for (i = 0; i < oprds.length; i++) {

		oprd = new IrOperand(oprds[i]);

		if (oprd.swizzle) {

			//need a new temp to move the swizzle so our code pattern works
			new_swz = swizzles[0].substring(0, oprd.swizzle.length);

			if (oprd.swizzle != new_swz) {
				dest = this.getTemp();
				ir = new IrInstruction('MOV', util.format("%s.%s", dest, new_swz), oprd.full);
				this.push(ir);
				oprd = new IrOperand(dest);
			}
		}

		oprds[i] = oprd;
	}

	//Merge template with passed operands
	for (i = 0; i < code.length; i++) {

		ir = new IrInstruction(code[i]);

		//For each operand
		for (j = 0; j < IrInstruction.operands.length; j++) {		
			o = IrInstruction.operands[j];
			oprd = ir[o];
			if (oprd && (n = oprd.name.match(/%(\d)/))) {
				n = parseInt(n[1]);
				ir[o] = new IrOperand(oprds[n - 1].toString());
				ir[o].addOffset(oprd.address);
				ir[o].swizzle = oprd.swizzle;
			}
		}

		this.push(ir);
	}
};


/**
 * Ir Error Class
 *
 * Used to differentiate between a compilation error and a compiler error
 */
function IrError(msg) {
	this.msg = msg;
	this.ir = true;
}
IrError.prototype = Error.prototype;






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
CONNECTION WITH THE SOFTWARE OR THE USE		 OR OTHER DEALINGS IN THE SOFTWARE.
*/


/**
 * Ir Operand Class
 */
function IrOperand(str) {
	this.name = "";
	this.neg = "";
	this.offset = "";
	this.offset2 = "";
	this.swizzle = "";
	this.parse(str);
}

/**
 * Parses operand string
 *
 * @param   string    string that represents a single variable
 */
IrOperand.prototype.parse = function(str) {
	var parts, swz;

	if (!str) {
		return;
	}
	
	//extract and check for swizzle at end
	parts = str.split('.');
	swz = parts.pop();
	if (!swz.match(/^([xyzw]+|[rgba]+|[stpq]+)$/)) {
		parts.push(swz);
		swz = "";
	}
	swz = swz.replace(/[xrs]/g, 'x');
	swz = swz.replace(/[ygt]/g, 'y');
	swz = swz.replace(/[zbp]/g, 'z');
	swz = swz.replace(/[waq]/g, 'w');
	this.swizzle = swz;

	//now split the rest
	parts = parts.join(".");
	parts = parts.match(/(\-?)([a-zA-Z0-9\$_\.]+)(?:(?:\[([0-9]+)\])*)$/);

	if (!parts) {
		throw new Error(util.format("Could not parse operand %s", str));
	}

	this.neg = parts[1];
	this.name = parts[2];
	this.offset = parts[3] ? parseInt(parts[3]) : "";
};

/**
 * Adds an offset
 *
 * @param   integer    Offset to add
 */
IrOperand.prototype.addOffset = function(offset) {
	if (offset === 0 && this.offset === "") {
		this.offset = 0;
	}
	if (!offset) {
		return;
	}
	this.offset = this.offset ? this.offset + offset : offset;	
};

/**
 * toString method
 *
 * @return  string
 */
IrOperand.prototype.toString = function() {
	var off, off2, swz;
	off = (this.offset || this.offset === 0) ? util.format("[%s]", this.offset) : "";
	off2 = (this.offset2 || this.offset2 === 0) ? util.format("[%s]", this.offset2) : null;
	swz = this.swizzle ? "." + this.swizzle : "";
	return util.format("%s%s%s%s", this.neg, this.name, off2 || off, swz);
};


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
 * IR Instruction Class
 *
 * Represents a single assembly-like instruction
 */
function IrInstruction(op, d, s1, s2, s3, gen) {
	var args;

	if (gen) {
		d = IRS.getTemp();
	}

	this.str = null;
	this.line = null;

	if (arguments.length == 1) {
		args = op.split(/[\s,]/);
		op = args[0];
		d = args[1];
		s1 = args[2];
		s2 = args[3];
		s3 = args[4];
	}

	this.op = op;
	this.d = this.operand(d);
	this.s1 = this.operand(s1);
	this.s2 = this.operand(s2);
	this.s3 = this.operand(s3);
}

IrInstruction.operands = ['d', 's1', 's2', 's3'];


IrInstruction.prototype.operand = function(opr) {
	return opr ? new IrOperand(opr) : "";
};

/**
 * Adds the offset to all operands
 *
 * @param   integer    The offset to set
 */
IrInstruction.prototype.addOffset = function(offset) {
	var i, o;

	for (i = 0; i < IrInstruction.operands.length; i++) {
		o = IrInstruction.operands[i];
		if (this[o]) {
			this[o].addOffset(offset);	
		}
	}
};

/**
 * Set the swizzle components on all operands
 *
 * @param   string    The swizzle to set
 */
IrInstruction.prototype.setSwizzle = function(swz) {
	var i, o;

	for (i = 0; i < IrInstruction.operands.length; i++) {
		o = IrInstruction.operands[i];
		if (this[o] && !this[o].swizzle) {
			this[o].swizzle = swz;
		}
	}
};

/**
 * toString method
 *
 * @return  string
 */
IrInstruction.prototype.toString = function() {
	var out;
	out = util.format("%s%s%s%s%s;",
		this.op,
		this.d  ? ' '  + this.d  : '',
		this.s1 ? ', ' + this.s1 : '',
		this.s2 ? ', ' + this.s2 : '',
		this.s3 ? ', ' + this.s3 : ''
		);
	return out;
};

/**
 * IR Comment Class
 *
 * Represents a single comment
 */
function IrComment(comment, loc) {
	this.comment = comment;
	this.loc = loc;
}

IrComment.prototype.toString = function() {
	var c = this.comment;

	if (this.loc) {
		c = util.format("[%s:%s-%s:%s] %s", this.loc.first_line, this.loc.first_column, this.loc.last_line, this.loc.last_column, c);
	}
	c = '# ' + c;

	return c;
};


/**
 * IR Operand Class
 *
 * Represents a single operand
 */
function IrOperand(str, raw) {

	this.full = "";
	this.neg = "";
	this.name = "";
	this.address = "";
	this.swizzle = "";
	this.number = "";
	this.raw = "";

	if (raw) {
		this.full = str;
		this.raw = str;
	} else {
		this.parse(str);
	}
}

/**
 * Parses operand string
 *
 * @param   string    string that represents a single variable
 */
IrOperand.prototype.parse = function(str) {
	var parts, regex;

	if (!str) {
		return;
	}

	//neg
	regex = "(\-)?";

	//name (include '%' for our code substitution rules)
	regex += "([\\w%]+)";

	//address
	regex += "(?:@(\\d+))?";

	//swizzle
	regex += "(?:\\.([xyzw]+))?";

	regex = new RegExp("^" + regex + "$");

	if (parts = str.match(regex)) {

		this.neg = parts[1] || "";
		this.name = parts[2];
		this.address = parseInt(parts[3]) || 0;
		this.swizzle = parts[4] || "";
	} else {
		if (parts = str.match(/^"(.*)"$/)) {
			this.raw = parts[1];
		} else {
			this.raw = str;
		}
	}

	this.full = this.toString();
};

/**
 * Adds an offset
 *
 * @param   integer    Offset to add
 */
IrOperand.prototype.addOffset = function(offset) {

	this.address = this.address || 0;

	this.address += offset;
};

/**
 * toString method
 *
 * @return  string
 */
IrOperand.prototype.toString = function() {
	var str;

	if (this.raw) {
		str = this.raw;	
	} else {
		str = this.neg + this.name + ("@" + this.address) + (this.swizzle ? "." + this.swizzle : "");
	}
	
	return str;
};






/*
Copyright (c) 2014 Cimaron Shanahan

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
 * GlslProgramJavascript class
 */
function GlslProgramJavascript() {

	this.vertex_code = [];
	this.fragment_code = [];

	this.symbols = new GlslProgramJavascriptVars();
	this.context = new GlslProgramJavascriptContext();

	this.library = {
		tex : function() {
			return [0, 0, 0, 0];	
		}
	};

	this.vertex = null;
	this.shader = null;
}

var proto = GlslProgramJavascript.prototype;

GlslProgramJavascript.translation_table = {
	'ABS' : '%1.* = Math.abs(%2.*)',
	'ADD' : '%1.* = (%2.*) + (%3.*)',
	//'ARL' : false,
	'CMP' : '%1.* = ((%2.* < 0.0) ? (%3.*) : (%4.*))',
	//'COS' : 'Math.cos(%2)',
	'DP3' : '%1.x = ((%2.x) * (%3.x) + (%2.y) * (%3.y) + (%2.z) * (%3.z))',
	'DP4' : '%1.x = ((%2.x) * (%3.x) + (%2.y) * (%3.y) + (%2.z) * (%3.z) + (%2.w) * (%3.w))',
	//'DPH' : '%1.* = (%2.x * %3.x + %2.y * %3.y + %2.z + %3.z + %3.w)',
	//'DST' : '%1.* = [1, %2.y * %3.y, %2.z, %3.w]',
	'ELSE'  : '} else {',
	'ENDIF' : '}', 
	'IF'  : 'if (%1.x) {',
	'MAD' : '%1.* = (%2.* * %3.*) + %4.*;',
	'MAX' : '%1.* = Math.max((%2.*), (%3.*))',
	'MOV' : '%1.* = %2.*;',
	'MUL' : '%1.* = %2.* * %3.*;',
	'POW' : '%1.x = Math.pow(%2.x, %3.x)',
	'RET' : 'return',
	'RSQ' : '%1.* = (1.0 / Math.sqrt(%2.*))',
	'SEQ' : '%1.* = (%2.* === %3.*) ? 1.0 : 0.0',
	'SGE' : '%1.* = (%2.* >= %3.*) ? (1.0) : (0.0)',
	'SLT' : '%1.* = (%2.* <  %3.*) ? (1.0) : (0.0)',
	'SUB' : '%1.* = (%2.*) - (%3.*)',
	'TEX' : '%1.* = tex(%3, %2.x, %2.y, 0)'
}; 


/**
 * Translates IR code into a javascript representation
 *
 * @return  bool      true if there were no errors
 */
proto.addObjectCode = function(object, target) {
	var i, errors;

	//optimize(irs, symbols);

	this.mergeSymbols(object);

	this.current = [];

	for (i = 0; i < object.code.length; i++) {
		try {
			this.instruction(object.code[i]);
		} catch (e) {
			this.error = e;
			return false;
		}
	}

	if (target == glsl.target.vertex) {
		this.vertex_code = this.current;
	} else if (target == glsl.target.fragment) {
		this.fragment_code = this.current;
	}

	return true;
};

/**
 * Merge symbol code into program table
 */
proto.mergeSymbols = function(object) {
	var s, t, n, entry, start, size;
	
	for (s in object.symbols) {
		
		t = object.symbols[s].entries;	

		for (n in t) {
		
			entry = t[n];
			start = parseInt(entry.out.split('@')[1]);
			size = types[entry.type].size;
			
			if (s == 'uniform') {
				this.symbols.addUniform(entry.name, start, size);
			} else if (s == 'attribute') {					
				this.symbols.addAttribute(entry.name, start, size);
			} else if (s == 'varying') {
				this.symbols.addVarying(entry.name, start, size);				
			}

		}
	}	
};

/**
 * Build a program
 *
 * @return  function
 */
proto.build = function() {

	var module, shaders;

	module = new Function("stdlib", "foreign", "heap",
		"\"use asm\";\n" +
		"var\n" +
		"uniform_f32 = new stdlib.Float32Array(heap, 0, 128),\n" +
		"attribute_f32 = new stdlib.Float32Array(heap, 128, 128),\n" +
		"varying_f32 = new stdlib.Float32Array(heap, 256, 128),\n" +
		"result_f32 = new stdlib.Float32Array(heap, 384, 128),\n" +
		"temp_f32 = new stdlib.Float32Array(heap, 512, 128),\n" +
		"jstemp = new stdlib.Float32Array(heap, 636, 4),\n" +
		"tex = foreign.tex;\n" +
		";\n" +
		"function vs() {\n" +
			this.vertex_code.join("\n") + "\n" +
		"}\n" +
		"function fs() {\n" +
			this.fragment_code.join("\n") + "\n" +
		"}\n" +
		"return { fragment : fs, vertex : vs };"
	);

	shaders = module(window, this.library, this.context.heap);

	this.vertex = shaders.vertex;
	this.fragment = shaders.fragment;	
};

/**
 * Translates ASM instruction into output format
 *
 * @param   string    string that represents a single instruction
 */
proto.instruction = function(ins) {
	var tpl, dest, src, i, j, k, code, js;

	if (ins instanceof IrComment) {
		this.current.push('    // ' + ins.toString());
		return;
	}

	this.current.push('// ' + ins.toString());

	if (!(tpl = GlslProgramJavascript.translation_table[ins.op])) {
		throw new Error("Could not translate opcode");
	}

	//variables
	dest = this.buildComponents(ins.d, true);
	if (!dest) {
		this.current.push("    " + tpl);
		return;
	}

	src = [];
	src.push(this.buildComponents(ins.s1));
	src.push(this.buildComponents(ins.s2));
	src.push(this.buildComponents(ins.s3));

	for (i = 0; i < dest.components.length; i++) {

		js = this.replaceOperand(tpl, '%1', dest, i);

		for (j = 0; j < 3; j++) {
			
			if (src[j]) {
				js = this.replaceOperand(js, '%' + (j + 2), src[j], i);
			}

		}

		this.current.push("    " + js);
	}

	this.current.push("");

	return;


	//fix atomic => non-atomic operations causing incorrect result
	temps = [];
	checkNeedTemp(dest, src1, temps);
	checkNeedTemp(dest, src2, temps);
	checkNeedTemp(dest, src3, temps);
	
	for (j = 0; j < code.length; j++) {	

		//if vector operation, we need to loop over each vector and grab the appropriate element
		for (i = 0; i < dest.count; i++) {
			
			trans = code[j];
			c = dest.comp[i];

			d = dest.out + c;
			s1 = buildVariable(src1, i, c);
			s2 = buildVariable(src2, i, c);
			s3 = buildVariable(src3, i, c);

			if (src1 && src1.comp[i].indexOf('jstemp') != -1) {
				s1 = src1.comp[i];
			}
			if (src2 && src2.comp[i].indexOf('jstemp') != -1) {
				s2 = src2.comp[i];
			}
			if (src3 && src3.comp[i].indexOf('jstemp') != -1) {
				s3 = src3.comp[i];
			}

			//vector with component
			trans = trans.replace(/%1\*/g, d);
			trans = trans.replace(/%2\*/g, s1);
			trans = trans.replace(/%3\*/g, s2);
			trans = trans.replace(/%4\*/g, s3);

			//vector without component
			trans = trans.replace(/%1/g, dest.out);
			trans = trans.replace(/%2/g, src1.out);
			trans = trans.replace(/%3/g, src2.out);
			trans = trans.replace(/%4/g, src3.out);

			//index of current component
			trans = trans.replace('%i', i);

			body.push(util.format("%s;", trans));

			if (!code[j].match(/%[0-9]+\*/)) {
				//break 1
				i = dest.count;
			}
		}
	}
};

proto.replaceOperand = function(tpl, from, op, n) {
	var out, i, swz = ['x', 'y', 'z', 'w'];
	
	if (op.components) {
		out = tpl.replace(from + '.*', util.format("%s[%s]", op.name, op.start + op.components[n]));
	} else {
		out = tpl.replace(from + '.*', op.name);	
	}
	
	for (i = 0; i < swz.length; i++) {
		out = out.replace(new RegExp(from + '\.' + swz[i], 'g'), util.format("%s[%s]", op.name, op.start + i));
	}

	return out;
};


/**
 * Prepares info on IR operand
 *
 * @param   IrOperand   opr    Operand
 * @param   bool        dest   Is destination?
 *
 * @return  object
 */
proto.buildComponents = function(opr, dest) {
	var i, swz, out;

	if (!opr) {
		return null;	
	}

	out = {};

	if (opr.raw) {
		out.name = opr.raw;
		return out;
	}
	
	out.name = opr.name + '_f32';
	out.start = 4 * opr.address;
	out.components = [];

	//generate array representation of swizzle components, expanding if necessary
	swz = opr.swizzle || "xyzw";
	swz = swz.split("");

	for (i = 0; i < 4; i++) {
		//exact swizzle specified and less than 4 components, grab last one
		if (swz.length <= i) {
			if (!dest) {
				//repeat last one
				out.components.push(out.components[i - 1]);	
			}
		} else {
			out.components.push("xyzw".indexOf(swz[i]));
		}
	}

	return out;
};

/**
 * Get Uniform Location
 *
 * @param   string   name   Name
 *
 * @return  int
 */
proto.getUniformLocation = function(name) {

	if (this.symbols.uniform[name]) {
		return this.symbols.uniform[name].start;	
	}

	return false;
};

/**
 * Get Uniform Size
 *
 * @param   string   name   Name
 *
 * @return  int
 */
proto.getUniformSize = function(name) {

	if (this.symbols.uniform[name]) {
		return this.symbols.uniform[name].size;	
	}
	
	return false;
};

/**
 * Set Uniform data
 * 
 * @param   string   name   Name
 * @param   array    data   Data
 */
proto.setUniformData = function(name, data) {
	var i, l, s, d;
	
	d = data.length;
	l = this.getUniformSize(name);
	s = this.getUniformLocation(name);
	
	if (l === false) {
		return;	
	}
	
	this.context.uniform_f32.set(data, i + s);
};

/**
 * Get Attribute Location
 *
 * @param   string   name   Name
 *
 * @return  int
 */
proto.getAttributeLocation = function(name) {

	if (this.symbols.attribute[name]) {
		return this.symbols.attribute[name].start;	
	}
	
	return false;
};

/**
 * Get Attribute Size
 *
 * @param   string   name   Name
 *
 * @return  int
 */
proto.getAttributeSize = function(name) {

	if (this.symbols.attribute[name]) {
		return this.symbols.attribute[name].size;	
	}
	
	return false;
};

/**
 * Set Attribute data
 * 
 * @param   string   name   Name
 * @param   array    data   Data
 */
proto.setAttributeData = function(name, data) {
	var i, l, s, d;
	
	d = data.length;
	l = this.getAttributeSize(name);
	s = this.getAttributeLocation(name);
	
	if (l === false) {
		return;	
	}
	
	this.context.attribute_f32.set(data, i + s);
};

/**
 * Get result data
 *
 * @param   int   start   Start pos
 * @param   int   size    Size
 *
 * @return  array
 */
proto.getResultData = function(start, size) {
	var res;
	res = Array.prototype.slice.apply(this.context.result_f32, [start, size]);
	return res;
};

/**
 * Set TEX lookup function
 *
 * 
 */
proto.setTexFunction = function(func) {
	this.extern.tex = func;
};


glsl.program = GlslProgramJavascript;




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
 * GlslProgramJavascriptContext class
 */
function GlslProgramJavascriptContext() {

	this.heap = new ArrayBuffer(512 * 4);

	this.uniform_f32 = new Float32Array(this.heap, 0, 128);
	this.attribute_f32 = new Float32Array(this.heap, 128, 128);
	this.result_f32 = new Float32Array(this.heap, 256, 128);

	//Alias
	this.varying_f32 = this.attribute_f32;
}

var proto = GlslProgramJavascriptContext.prototype;



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
 * GlslProgramJavascriptVars Class
 */
function GlslProgramJavascriptVars() {
	this.uniform = {};
	this.attribute = {};
	this.varying = {};
}

var proto = GlslProgramJavascriptVars.prototype;


/**
 * Add uniform variable
 */
proto.addUniform = function(name, pos, size) {
	this.uniform[name] = new GlslProgramJavascriptVar(name, pos, size);
};

/**
 * Add attribute variable
 */
proto.addAttribute = function(name, pos, size) {
	this.attribute[name] = new GlslProgramJavascriptVar(name, pos, size);
};

/**
 * Add varying variable
 */
proto.addVarying = function(name, pos, size) {
	this.varying[name] = new GlslProgramJavascriptVar(name, pos, size);
};



/**
 * GlslProgramJavascriptVar Class
 */
function GlslProgramJavascriptVar(name, pos, size) {
	this.name = name;
	this.pos = pos;
	this.size = size;
}




	this.glsl = glsl;

}());

