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

	//The following are used during IR generation
	this.Dest = null;
	this.Type = null;
	this.Const = false;
}

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

/**
 * Return string representation of node
 *
 * @return  string
 */
proto.toString = function() {
	return this.constructor.name;
};

proto.ir = function(state, irs) {
	//throw new Error("Missing ir generator for node of type " + this.constructor.name);
};


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
	highp : 1,
	mediump : 2,
	lowp : 3
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

util.inherits(AstTypeSpecifier, AstNode);
proto = AstTypeSpecifier.prototype;

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
 * Return string representation of node
 *
 * @return  string
 */
proto.toString = function() {
	var i, prec;
	
	if (this.is_precision_statement) {
		
		for (i in ast_precision) {
			if (ast_precision.hasOwnProperty(i) && ast_precision[i] === this.precision) {
				prec = i;
				break;
			}
		}
		
		return util.format("precision %s %s;", prec, this.type_name);
	}
	
	return (this.type_specifier === types.struct ? this.structure : this.type_name)
	    + (this.is_array ? util.format("[%s]", this.array_size || "") : "")
		;
};


/**
 * AST Function Class
 */
function AstFunction() {
	AstNode.apply(this);

	this.return_type = null;
	this.identifier = null;
	this.parameters = [];

	this.entry = null;
}

util.inherits(AstFunction, AstNode);
proto = AstFunction.prototype;

/**
 * Return string representation of node
 *
 * @return  string
 */
proto.toString = function() {
	return util.format("%s %s(%s)", this.return_type, this.identifier, this.parameters.join(", "));			
};

/**
 * AST Expression Class
 */
function AstExpression(oper, ex0, ex1, ex2) {
	AstNode.apply(this);

	this.oper = oper;
	this.grouped = false;
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

/**
 * Return string representation of node
 *
 * @return  string
 */
proto.toString = function() {

	var output;

	switch (this.oper) {
		case '=':
		case '+':
		case '-':
		case '*':
		case '/':
		case '%':
		case "<<":
		case ">>":
		case "<":
		case ">":
		case "<=":
		case ">=":
		case "==":
		case "!=":
		case "&":
		case "^":
		case "|":
		case "~":
		case "&&":
		case "^^":
		case "||":
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
			output = util.format("%s %s %s", this.subexpressions[0], this.oper, this.subexpressions[1]);
			break;

		case '.':
			output = util.format("%s.%s", this.subexpressions[0], this.primary_expression.identifier);
			break;

		case 'POS':
		case 'NEG':
		case '!':
		case '++x':
		case '--x':
			output = util.format("%s%s", this.oper.replace('x', ''), this.subexpressions[0]);
			break;
		
		case 'x++':
		case 'x--':
			output = util.format("%s%s", this.subexpressions[0], this.oper.replace('x', '') );
			break;

		case '?:':
			output = util.format("%s ? %s : %s", this.subexpressions[0], this.subexpressions[1], this.subexpressions[2]);				
			break;

		case '[]':
			output = util.format("%s[%s]", this.subexpressions[0], this.subexpressions[1]);				
			break;

		case '()':
			output = util.format("%s(%s)", this.subexpressions[0], this.expressions.join(", "));
			break;

		case 'ident':
			output = util.format("%s", this.primary_expression.identifier);
			break;
		
		case 'float':
			output = util.format("%s", this.primary_expression.float_constant);
			break;
		
		case 'int':
			output = util.format("%s", this.primary_expression.int_constant);
			break;
		
		case 'bool':
			output = util.format("%s", this.primary_expression.bool_constant ? 'true' : 'false');
			break;

	}

	return this.grouped ? util.format("(%s)", output ) : output;
};

/**
 * AST Fully Specified Type Class
 */
function AstFullySpecifiedType() {
	AstNode.apply(this);
	
	this.qualifier = [];
	this.specifier = null;
}

util.inherits(AstFullySpecifiedType, AstNode);
proto = AstFullySpecifiedType.prototype;

/**
 * Return string representation of node
 *
 * @return  string
 */
proto.toString = function() {
	var output;

	output = this.qualifier.slice(0);
	output.push(this.specifier);

	return output.join(' ');
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

util.inherits(AstDeclaration, AstNode);
proto = AstDeclaration.prototype;

/**
 * Return string representation of node
 *
 * @return  string
 */
proto.toString = function() {
	return this.identifier
	    + (this.initializer ? util.format(" = %s", this.initializer) : "")
		;
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

util.inherits(AstDeclaratorList, AstNode);
proto = AstDeclaratorList.prototype;

/**
 * Return string representation of node
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

util.inherits(AstParameterDeclarator, AstNode);
proto = AstParameterDeclarator.prototype;

/**
 * Return string representation of node
 *
 * @return  string
 */
proto.toString = function() {
	return this.type
	    + (this.identifier ? " " + this.identifier : "")
		+ (this.is_array ? util.format("[%s]", this.array_size) : "")
		;
};


/**
 * AST Expression Statement Class
 */
function AstExpressionStatement(ex) {
	AstNode.apply(this);

	this.expression = ex;
}

util.inherits(AstExpressionStatement, AstNode);
proto = AstExpressionStatement.prototype;

/**
 * Return string representation of node
 *
 * @return  string
 */
proto.toString = function() {
	return util.format("%s;\n", this.expression || "");
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

util.inherits(AstCompoundStatement, AstNode);
proto = AstCompoundStatement.prototype;

/**
 * Return string representation of node
 *
 * @return  string
 */
proto.toString = function() {
	var str,
		stmts,
	    indent
		;

	AstCompoundStatement._depth++;
	indent = new Array(AstCompoundStatement._depth).join("  ")

	stmts = indent + "  " + this.statements.join(indent + "  ");

	str = "\n" + indent + "{\n"
	    + stmts
		+ indent + "}\n"
		;

	AstCompoundStatement._depth--;

	return str;
};

//Used for toString indentation
AstCompoundStatement._depth = 0;

/**
 * AST Function Definition Class
 */
function AstFunctionDefinition() {
	AstNode.apply(this);

	this.proto_type = null;
	this.body = null;
}

util.inherits(AstFunctionDefinition, AstNode);
proto = AstFunctionDefinition.prototype;

/**
 * Return string representation of node
 *
 * @return  string
 */
proto.toString = function() {
	return util.format("%s %s", this.proto_type, this.body);
};

/**
 * AST Function Definition Class
 */
function AstExpressionBin(oper, ex0, ex1) {
	AstExpression.apply(this, [oper, ex0, ex1]);
}

util.inherits(AstExpressionBin, AstExpression);
proto = AstExpressionBin.prototype;

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

util.inherits(AstFunctionExpression, AstExpression);
proto = AstFunctionExpression.prototype;

proto.is_constructor = function() {
	return this.cons;
};

proto.toString = function() {
	return util.format("%s(%s)", this.subexpressions[0], this.expressions.join(", "));
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

util.inherits(AstSelectionStatement, AstNode);
proto = AstSelectionStatement.prototype;

/**
 * Return string representation of node
 *
 * @return  string
 */
proto.toString = function() {
	return util.format("if (%s) %s %s", this.condition, this.then_statement, this.else_statement ? util.format("else %s", this.else_statement) : "");
};


/**
 * AST Struct Specifier Class
 */
function AstStructSpecifier(identifier, declarator_list) {
	AstNode.apply(this);
	this.name = null;
	this.declarations = [];

	if (identifier === null) {
		identifier = util.format("#anon_struct%d", AstStructSpecifier.anon_count);
		AstStructSpecifier.anon_count++;
	}
	this.name = identifier;
	this.declarations = declarator_list.declarations;
}

AstStructSpecifier.anon_count = 1;

util.inherits(AstStructSpecifier, AstNode);
proto = AstStructSpecifier.prototype;



/**
 * AST Jump 
 */
function AstJumpStatement(mode, return_value) {
	AstNode.apply(this);

	this.opt_return_value = null;
	this.mode = mode;

	if (mode === ast_jump_modes._return) {
		this.opt_return_value = return_value;	
	}	
}

var ast_jump_modes = {
	_continue : 0,
	_break : 1,
	_return : 2,
	_discard : 3
};

util.inherits(AstJumpStatement, AstNode);
proto = AstJumpStatement.prototype;

/**
 * Return string representation of node
 *
 * @return  string
 */
proto.toString = function() {
	
	switch (this.mode) {

		case ast_jump_modes._continue:
			return "continue;\n";
		
		case ast_jump_modes._break:
			return "break;\n";
		
		case ast_jump_modes._return:
			return util.format("return%s;\n", this.opt_return_value ? " " + this.opt_return_value : "");
		
		case ast_jump_modes._discard:
			return "discard;\n";
	}
};


glsl.ast = {
	Node : AstNode,
	TypeSpecifier : AstTypeSpecifier,
	Function : AstFunction,
	Expression : AstExpression,	
	FullySpecifiedType : AstFullySpecifiedType,
	Declaration : AstDeclaration,
	DeclaratorList : AstDeclaratorList,
	ParameterDeclarator : AstParameterDeclarator,
	ExpressionStatement : AstExpressionStatement,
	CompoundStatement : AstCompoundStatement,
	FunctionDefinition : AstFunctionDefinition,
	ExpressionBin : AstExpressionBin,
	FunctionExpression : AstFunctionExpression,
	SelectionStatement : AstSelectionStatement,
	StructSpecifier : AstStructSpecifier,
	JumpStatement : AstJumpStatement	
};

