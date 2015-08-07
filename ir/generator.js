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
	var irs,
	    ast,
	    i,
		main
		;

	symbol_table_init(state, state.symbols, state.options.target);

	irs = new Ir(state.options.target);
	ast = state.getAst();
	
	try {

		for (i = 0; i < ast.length; i++) {
			ast[i].ir(state, irs);
		}

		main = state.symbols.get_function('main');

		//Accept main, but warn if params not void
		if (main.definition.join(",") !== "void") {
			state.addWarning("main() should take no parameters");
		}

		state.symbols.add_variable("<returned>", irs.getTemp(main.getType().slots));
		
		if (main.type != 'void') {
			state.addWarning("main() should be type void");	
		}

		if (!main) {
			state.addError("main() is not defined");
			return false;
		}

		main.Ast.body.ir(state, irs);

	} catch (e) {

		if (!e.ir) {
			e.message = "compiler error: " + e.message;
		}

		state.addError(e.message, e.lineNumber, e.columnNumber);
		return false;
	}

	state.setIR(irs);

	return true;
};

/**
 * Constructs an error message
 *
 * @param   string     The error message
 * @param   AstNode    The error AstNode
 *
 * @return  string
 */
AstNode.prototype.ir_error = function(message) {
	var e = new IrError();

	if (this.location) {
		e.lineNumber = this.location.first_line;
		e.columnNumber = this.location.first_column;
		e.message = message;
	}

	throw e;
}

/**
 * Default IR
 */
AstNode.prototype.irx = function(state, irs) {
	this.ir_error(util.format("Can't generate ir for %s", this.typeOf()));
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

//	this.ir_error("Cannot generate type specifier");
};


/**
 * Constructs a declaration list
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstDeclaratorList.prototype.ir = function(state, irs) {
	var i;

	for (i = 0; i < this.declarations.length; i++) {
		this.declarations[i].ir(state, irs, this.type);
	}
};

/**
 * Constructs a declaration
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstDeclaration.prototype.ir = function(state, irs, type) {
	var qualifier, name, entry, constant, assign, lhs, size;

	if (type.qualifier) {
		qualifier = type.qualifier;
	}

	name = this.identifier;

	//add symbol table entry
	entry = state.symbols.add_variable(name);
	entry.type = type.specifier.type_name;
	entry.qualifier = qualifier;

	if (qualifier.indexOf('uniform') !== -1) {
		entry.out = irs.getUniform(entry);
	} else if (qualifier.indexOf('attribute') !== -1) {
		entry.out = irs.getAttribute(entry);
	} else if (qualifier.indexOf('varying') !== -1) {
		entry.out = irs.getVarying(entry);
	} else {
		entry.out = irs.getTemp(entry.getType().slots);
	}

	constant = (qualifier === 'const');

	if (this.is_array) {
		
		this.array_size.ir(state, irs);

		if (this.array_size.Type != 'int') {
			this.ir_error("array size must be an integer");
		}

		if (!this.array_size.Const) {
			this.ir_error("array size must be constant");
		}

		size = parseInt(this.array_size.Dest);

		if (size < 1) {
			this.ir_error("array size cannot be less than 1");
		}

		entry.size = size;

		//Change the type of the entry so that expressions without indexing will fail
		entry.base_type = entry.type;
		entry.type += '[]';
	}

	if (this.initializer) {

		//@todo: generate constants at compile time (this may be able to be taken care of in the generator)
		if (constant) {
			//entry.constant = this.initializer.Dest;
		} else {
			lhs = new AstExpression('ident');
			lhs.primary_expression.identifier = name;
			assign = new AstExpression('=', lhs, this.initializer);
			assign.setLocation(this.location);
			assign.ir(state, irs);
		}

	} else {
		if (constant) {
			this.ir_error("Declaring const without initialier");
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

	//handle function proto
	this.proto_type.ir(state, irs);

	this.proto_type.entry.Ast = this;
};


/**
 * Constructs a function header code block
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstFunction.prototype.ir = function(state, irs) {
	var i, def, entry;

	def = [];

	if (this.parameters.length == 0) {
		def.push('void');
	}

	//generate param list
	for (i = 0; i < this.parameters.length; i++) {
		def.push(this.parameters[i].type.specifier.type_name);
	}

	entry = state.symbols.get_function(this.identifier, def);
	
	if (entry && entry !== this.entry) {
		this.ir_error(util.format("Cannot redefine %s", this.identifier));
	}

	this.entry.definition = def;
};


/**
 * Constructs a compound statement code block
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstCompoundStatement.prototype.ir = function(state, irs) {
	var i, stmt, retd_entry, maybe_returned;

	retd_entry = state.symbols.get_variable("<returned>");
	maybe_returned = false;

	for (i = 0; i < this.statements.length; i++) {

		stmt = this.statements[i];

		stmt.ir(state, irs);

		if (stmt instanceof AstJumpStatement && stmt.mode == 'return') {
			
			//Returning from block, set return status, and skip following instructions in block (unreachable)
			retd_entry.Passed = true;
			irs.push(new IrInstruction("MOV", retd_entry.out + ".x", "1.0"));
			break;
		}
		
		if (!maybe_returned && retd_entry.Passed) {
			maybe_returned = true;
			irs.push(new IrInstruction("IF", retd_entry.out + ".x"));
		}
	}
	
	if (maybe_returned) {
		irs.push(new IrInstruction("ENDIF"));	
	}
};


/**
 * Constructs an expression statement code block
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstExpressionStatement.prototype.ir = function(state, irs) {

	if (this.expression !== null) {
		this.expression.ir(state, irs);
	}
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
	if (this.constructor === AstTypeSpecifier) {
		this.Type = this.type_specifier;
		return;
	}

	this.ir_error("Could not translate unknown expression type");
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

		case 'POS':
			//useless
			this.Dest = se[0].Dest;
			this.Type = se[0].Type;
			break;

		case 'NEG':

			if (se[0].Dest.substring(0, 1) != '-') {
				this.Dest = "-" + se[0].Dest;	
			} else {
				this.Dest = se[0].Dest.substring(1);	
			}
			
			this.Type = se[0].Type;
			
			if (se[0].Const) {
				this.Const = se[0].Const;	
			}
			
			break;

		//Arithmetic
		case '+':
		case '-':
		case '*':
		case '/':
		case '%':
		case '&':
		case '^':
		case '|':
		case '~':
		case '<<':
		case '>>':
			this.ir_generate(state, irs, 2, true);
			break;
		
		//Boolean
		case '<':
		case '>':
		case '<=':
		case '>=':
		case '==':
		case '!=':
		case '&&':
		case '^^':
		case '||':
			this.ir_generate(state, irs, 2);
			break;
		case '!':
			this.ir_generate(state, irs, 1);
			break;

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
			this.ir_op_assign(state, irs);
			break;

		case '?':
			this.ir_ternary(state, irs);
			break;
		
		//Increment / Decrement
		case '++x':
		case '--x':
		case 'x++':
		case 'x--':
			this.ir_incdec(state, irs);
			break;
		//case '.': break;
		case '[]':
			this.ir_arr_index(state, irs);
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
			this.ir_error(util.format("Could not translate unknown expression %s (%s)", this, this.oper));
	}
};


/**
 * Constructs an assignment expression
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstExpression.prototype.ir_assign = function(state, irs, skip_comment/*, local*/) {
	var cond, ir, temp, size, slots, swz, i, entry, lhs, rhs, com;

	lhs = this.subexpressions[0];
	rhs = this.subexpressions[1];

	if (lhs.Type != rhs.Type || rhs.Const) {
		this.ir_cast.apply(rhs, [state, irs, lhs.Type]);
	}

	this.Type = lhs.Type;

	if (lhs.Entry && lhs.Entry.constant) {
		this.ir_error(util.format("Cannot assign value to constant %s", lhs.Dest));
	}

	if (lhs.Entry && lhs.Entry.readonly) {
		this.ir_error(util.format("Cannot assign value to read-only variable %s", lhs.Entry.name));	
	}

	if (!skip_comment) {
		com = util.format("%s => %s %s <%s>", rhs.Dest, lhs.Type, lhs.Dest, lhs.toString());
		irs.push(new IrComment(com, this.location));
	}

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
 * Constructs an operator assignment expression (e.g. +=)
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstExpression.prototype.ir_op_assign = function(state, irs, type) {
	var op, expr, assign;

	//cut off '='
	op = this.oper.slice(0, -1);

	expr = new AstExpression(op, this.subexpressions[0], this.subexpressions[1]);

	assign = new AstExpression('=', this.subexpressions[0], expr);
	
	assign.ir(state, irs);

	this.Dest = assign.Dest;
	this.Type = assign.Type;
	this.Const = assign.Const;
};


/**
 * Constructs a cast operation
 */
AstExpression.prototype.ir_cast = function(state, irs, type) {

	//Can cast to type?
	if (Type.canCast(this.Type, type)) {

		//Simple case, constant
		if (this.Const) {
			this.Dest = Type.castTo(this.Dest, this.Type, type);
			this.Type = type;
		} else {
			//@todo: generate cast instructions
			this.ir_error(util.format("Could not assign value of type %s to %s", this.Type, type));
		}

	} else {
		this.ir_error(util.format("Could not assign value of type %s to %s", this.Type, type));
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

		if (!entry /*|| !entry.type*/) {
			this.ir_error(util.format("%s is undefined", name));
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
	if (this.primary_expression.type == 'float') {
		this.Type = 'float';
		this.Dest = this.primary_expression.float_constant;
		this.Const = true;
		return;
	}

	//int constant
	if (this.primary_expression.type == 'int') {
		this.Type = 'int';
		this.Dest = this.primary_expression.int_constant;
		this.Const = true;
		return;
	}

	//bool constant
	if (this.primary_expression.type == 'bool') {
		this.Type = 'bool';
		this.Dest = this.primary_expression.bool_constant;
		this.Const = true;
		return;
	}

	this.ir_error("Cannot translate unknown simple expression type");
};

/**
 * Constructs the code for an expression
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstExpression.prototype.ir_generate = function(state, irs, len, arith) {
	var table, se, oprd_types, dest, i, j, def, match, comment, cnst;

	if (!(table = builtin.oper[this.oper])) {
		this.ir_error(util.format("Could not generate operation %s", this.oper));
	}

	se = this.subexpressions;

	//Fold constants
	if (state.options.opt.fold_constants && arith) {
		if (se[0].Const && se[1].Const) {

			cnst = eval(se[0].Dest + this.oper + se[1].Dest);

			//If the calculation results in an error, resume normal IR generation and let it be handled at runtime
			if (Number.isFinite(cnst)) {
				this.Dest = "" + cnst;
				this.Type = 'float';
				this.Const = true;
				return;
			}
		}
	}

	oprd_types = [];
	dest = [];

	for (i = 0; i < len; i++) {
		oprd_types.push(se[i].Type);
		dest.push(se[i].Dest);
	}

	def = new RegExp(oprd_types.join(",") + "\:(.*)");
	for (j in table) {
		if (match = j.match(def)) {
			break;
		}
	}

	if (!match) {
		this.ir_error(util.format("Could not apply operation %s to %s", this.oper, oprd_types.join(", ")));
	}

	this.Type = match[1];
	this.Dest = irs.getTemp(types[this.Type].slots);
	
	dest.splice(0, 0, this.Dest);

	if (len <= 4) {
		//this.Dest += util.format(".%s", swizzles[0].substring(0, glsl.type.size[this.Type]));
	}

	if (len == 1) {
		comment = util.format("(%s %s %s) => %s %s", this.oper, se[0].Type, se[0].Dest, this.Type, this.Dest);
	} else if (len == 2) {
		comment = util.format("(%s %s %s %s %s) => %s %s", se[0].Type, se[0].Dest, this.oper, se[1].Type, se[1].Dest, this.Type, this.Dest);
	} else if (len == 3) {
		comment = util.format("(%s %s ? %s %s : %s %s) => %s %s", se[0].Type, se[0].Dest, se[1].Type, se[1].Dest, se[2].Type, se[2].Dest, this.Type, this.Dest);
	}

	irs.push(new IrComment(comment, this.location));

	irs.build(table[j], dest);
};

/**
 * Constructs an pre/post increment/decrement expression
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstExpression.prototype.ir_incdec = function(state, irs) {
	var se, op, ins, post, type, i, ir;

	se = this.subexpressions[0];

	op = this.oper.replace('x', '');
	ins = op === '++' ? 'ADD' : 'SUB';
	post = this.oper.indexOf('x') === 0;
	type = types[se.Type];

	//Type check: base type must be int or float
	if (type.base != 'int' && type.base != 'float') {
		this.ir_error(util.format("Could not apply operation %s to %s", op, se.Type));
	}

	this.Type = se.Type;

	if (post) {
		//For post increment, the returned happens before the increment, so we need a temp to store it
		this.Dest = irs.getTemp(type.slots);
	} else {
		this.Dest = se.Dest;
	}

	irs.push(new IrComment(util.format("(%s%s) => %s %s", post ? se.Dest : op, post ? op : se.Dest, this.Type, this.Dest), this.location));
	
	for (i = 0; i < type.slots; i++) {

		if (post) {
			this.Dest = irs.getTemp(type.slots);
			ir = new IrInstruction('MOV', this.Dest, se.Dest);
			ir.addOffset(i);
			ir.setSwizzle(type.swizzle);
			irs.push(ir);
		}

		ir = new IrInstruction(ins, se.Dest, se.Dest, "1.0");
		ir.addOffset(i);
		ir.setSwizzle(type.swizzle);
		irs.push(ir);
	}

};

/**
 * Constructs a ternary expression
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstExpression.prototype.ir_ternary = function(state, irs) {
	var cond, pass, ontrue, onfalse, dest, test;
	
	cond = this.subexpressions[0];
	ontrue = this.subexpressions[1];
	onfalse = this.subexpressions[2];


	if (!Type.canCast(cond.Type, 'bool')) {
		this.ir_error("boolean expression expected");
	}

	if (ontrue.Type != onfalse.Type) {
		this.ir_error("Types do not match: %s, %s", ontrue.Type, onfalse.Type);	
	}

	this.Type = ontrue.Type;

	//test can be evaluated at compile time
	if (cond.Const) {
		
		pass = eval(cond.Dest);

		if (pass) {
			this.Dest = ontrue.Dest;
			this.Const = ontrue.Const;
			return;
		} else {
			this.Dest = onfalse.Dest;
			this.Const = onfalse.Const;
			return;
		}
	}

	this.Dest = irs.getTemp(types[ontrue.Type].slots);

	irs.push(new IrComment(util.format("(%s %s ? %s %s : %s %s) => %s %s", cond.Type, cond.Dest, ontrue.Type, ontrue.Dest, onfalse.Type, onfalse.Dest, this.Type, this.Dest), this.location));

	dest = new IrOperand(this.Dest);
	dest.makeSize(types[this.Type].components);

	test = new IrInstruction("IF", cond.Dest);
	test.d.makeSize(1);

	irs.push(test);
	irs.push(new IrInstruction("MOV", dest, ontrue.Dest));
	irs.push(new IrInstruction("ELSE"));
	irs.push(new IrInstruction("MOV", dest, onfalse.Dest));
	irs.push(new IrInstruction("ENDIF"));
};

/**
 * Constructs an array index expression
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstExpression.prototype.ir_arr_index = function(state, irs) {
	var arr, idx, entry, size, cnst, oprd;
	
	arr = this.subexpressions[0];
	idx = this.subexpressions[1];
	
	entry = arr.Entry;

	//Ensure array index is integer
	if (idx.Type != 'int') {
		this.ir_error("array index out of bounds");
	}

	//@todo: Need to implement array indexing syntax for vector components
	if (!entry.size) {
		this.ir_error("cannot index a non-array value");	
	}

	//@todo: Need to implement array indexing for matrices
	if (types[entry.base_type].slots > 1) {
		this.ir_error("array indexing for matrices not implemented yet");	
	}

	this.Type = entry.base_type;

	//If constant index, we can do some additional error checking
	if (idx.Const) {

		cnst = parseInt(idx.Dest);

		if (cnst < 0 || cnst >= entry.size) {
			this.ir_error("array index out of bounds");	
		}

		oprd = new IrOperand(arr.Dest);
		oprd.index = cnst;
	
		this.Dest = oprd.toString();

	} else {

		//@todo: variable indexing is permitted by spec, but behavior is undefined for out of bounds

		this.ir_error("variable indexing not implemented yet");	
	}
};

/**
 * Constructs a function expression
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstFunctionExpression.prototype.ir = function(state, irs) {
	var i, e, name, entry, ret_entry, retd_entry, call_types, operands, param, proto, loc;

	if (this.cons) {
		return this.ir_constructor(state, irs);
	}

	name = this.subexpressions[0].primary_expression.identifier;

	operands = [];
	call_types = [];
	
	for (i = 0; i < this.expressions.length; i++) {

		e = this.expressions[i];
		e.ir(state, irs);

		call_types.push(e.Type);
		operands.push(e.Dest);
	}

	entry = state.symbols.get_function(name, call_types);
	if (!entry) {
		this.ir_error(util.format("Function %s(%s) is not defined", name, call_types.join(", ")));
	}

	this.Type = entry.type;
	this.Dest = irs.getTemp(entry.getType().slots);

	irs.push(new IrComment(util.format("%s(%s) => %s %s", name, operands.join(", "), this.Type, this.Dest), this.location));

	if (entry.code) {

		//Use function template
		operands.unshift(this.Dest);
		irs.build(entry.code, operands);
		
	} else if (entry.Ast) {

		//Rebuild inline function from AST
		state.symbols.push_scope();

		//Enter vars into local symbol table
		proto = entry.Ast.proto_type;
		for (i = 0; i < proto.parameters.length; i++) {
			param = proto.parameters[i];
			loc = state.symbols.add_variable(param.identifier, param.type.specifier.type_name);
			loc.out = irs.getTemp(loc.getType().slots);
			
			//Add MOV operation from called param to local param
			irs.push(new IrComment(util.format("PARAM %s => %s %s", operands[i], loc.out, param.type.specifier.type_name), param.location));

			//Piggy-back off assignment generation
			lhs = new AstExpression('<param>');
			lhs.setLocation(this.getLocation());
			lhs.Type = loc.type;
			lhs.Dest = loc.out;

			assign = new AstExpression('=', lhs, this.expressions[i]);
			assign.setLocation(this.getLocation());
			assign.ir_assign(state, irs, true);
		}
		
		//Create a return entry for the new call scope
		ret_entry = state.symbols.add_variable("<return>", this.Type);
		ret_entry.out = this.Dest;

		retd_entry = state.symbols.add_variable("<returned>", "bool");
		retd_entry.out = irs.getTemp(retd_entry.getType().slots);

		entry.Ast.body.ir(state, irs);

		state.symbols.pop_scope();
	}
};


/**
 * Constructs a type constructor
 *
 * @param   object   state   GLSL state
 * @param   object   irs     IR representation
 */
AstFunctionExpression.prototype.ir_constructor = function(state, irs) {
	var type, comment_text, comment, i, expr, src_expr, src_i, src_c, dest_i, dest_c, oprd, dest;

	type = this.subexpressions[0].type_specifier;

	this.Type = type.name;
	this.Dest = irs.getTemp(type.slots);

	comment_text = [];
	comment = new IrComment("", this.location);
	irs.push(comment);

	//Prepare components
	for (i = 0; i < this.expressions.length; i++) {
		
		expr = this.expressions[i];

		if (expr) {
			expr.ir(state, irs);	
			comment_text.push(expr.Dest);
		}
		
	}

	src_expr = this.expressions[0];
	src_i = 0; //Source expression index
	src_c = 0; //Component of source expression

	dest_i = 0; //Dest index
	dest_c = 0; //Component of dest

	for (i = 0; i < type.size; i++) {

		if (!src_expr) {
			this.ir_error("Not enough parameters to constructor");				
		}

		//@todo: need to add support for > vec4
		if (types[src_expr.Type].slots > 1) {
			this.ir_error("Matrix components not implemented yet");	
		}

		//compute destination
		if (type.slot == 1) {
			dest = util.format("%s.%s", this.Dest, Ir.swizzles[0][dest_c]);			
		} else {
			dest = util.format("%s[%s].%s", this.Dest, dest_i, Ir.swizzles[0][dest_c]);
		}

		//compute source
		oprd = new IrOperand(src_expr.Dest);

		if (!oprd.swizzle) {
			oprd.swizzle = Ir.swizzles[0][src_c];
		}

		irs.push(new IrInstruction('MOV', dest, oprd.toString()));

		src_c++;

		//Get next source component expression
		if (src_c >= types[src_expr.Type].size) {
			if (this.expressions[src_i + 1]) {
				src_i++;
				src_expr = this.expressions[src_i];
				src_c = 0;
			}
		}

		dest_c++;
		if (dest_c >= 4) {
			dest_c = 0;
			dest_i++;
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
	var field, swz, base, se, valid_swz, type;

	//pick swizzle set
	field = this.primary_expression.identifier;

	se = this.subexpressions[0];
	se.ir(state, irs);

	if (Ir.isSwizzle(field)) {

		type = types[se.Type];

		//error on scalar
		if (type.size == 1) {
			this.ir_error("Invalid component selection on a scalar");
		}

		//error on matrix
		if (type.slots > 1) {
			this.ir_error("Invalid component selection on a matrix");
		}

		//error if components outside of base component range (e.g. 'z' in vec2)
		swz = Ir.normalizeSwizzle(field);
		valid_swz = "xyzw".substr(0, type.components);
		if (!Ir.matchComponents(swz, valid_swz)) {
			this.ir_error(util.format("Invalid component selection on %s", se.Type));
		}

		base = type.base;
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

		//error 
		if (field.length > 4) {
			this.ir_error("Invalid component selection: too many components");
		}

		if (!this.Type) {
			this.ir_error("Invalid component selection");
		}

		this.Dest = util.format("%s.%s", se.Dest, swz);
	}
	
	if (se.Entry) {
		this.Entry = se.Entry;	
	}
}


/**
 * Constructs a selection statement
 *
 * @param   ast_node    Statement
 */
AstSelectionStatement.prototype.ir = function(state, irs) {
	var ir, cond, pass;

	this.condition.ir(state, irs);

	cond = this.condition;

	if (!Type.canCast(cond.Type, 'bool')) {
		this.ir_error("boolean expression expected");
	}

	//test can be evaluated at compile time
	if (cond.Const) {

		pass = eval(cond.Dest);
		
		if (pass) {
			this.then_statement.ir(state, irs);
			return;
		} else {
			this.else_statement.ir(state, irs);
			return;
		}
	}

	irs.push(new IrComment(util.format("if %s then", cond.Dest), this.location));

	//set a flag based on the result
	ir = new IrInstruction('IF', cond.Dest);
	ir.d.makeSize(1);

	irs.push(ir);

	this.then_statement.ir(state, irs);

	if (this.else_statement) {

		irs.push(new IrInstruction('ELSE'));

		this.else_statement.ir(state, irs);
	}

	irs.push(new IrInstruction('ENDIF'));
}

/**
 * Constructs a jump statement
 *
 * Note: jump semantics are a bit different in glsl as there is no true "jumping":
 * functions are inlined, loops are unrolled, etc.
 *
 * @param   ast_node    Statement
 */
AstJumpStatement.prototype.ir = function(state, irs) {
	var ret, ret_entry, assign, lhs;

	switch (this.mode) {

		case 'return':

			ret = this.opt_return_value;
		
			if (ret) {
				
				ret.ir(state, irs);
		
				ret_entry = state.symbols.get_variable('<return>');
		
				//@todo: need to compare return value type with current function type
		
				irs.push(new IrComment(util.format("return => %s %s", ret.Dest, ret.Type), this.location));
		
				//Piggy-back off assignment generation
				lhs = new AstExpression('<return>');
				lhs.setLocation(this.getLocation());
				lhs.Type = ret.Type;
				lhs.Dest = ret_entry.out;
		
				assign = new AstExpression('=', lhs, ret);
				assign.setLocation(this.getLocation());
				assign.ir_assign(state, irs, true);
		
			} else {
				irs.push(new IrComment("return", this.location));
			}
		
			break;
		
		default:
			//@todo:
	}

};


/**
 * Constructs an iteration statement
 *
 * @param   ast_node    Statement
 */
AstIterationStatement.prototype.ir = function(state, irs) {
	var comment, init;

	switch (this.mode) {
		
		case 'for':
			this.ir_for(state, irs);
			break;
		
		default:
			this.ir_error("Unknown iteration statement type");
	}
};

/**
 * Constructs an iteration statement
 *
 * Note: jump semantics are a bit different in glsl as there is no true "jumping":
 * functions are inlined, loops are unrolled, etc.
 *
 * @param   ast_node    Statement
 */
AstIterationStatement.prototype.ir_for = function(state, irs) {
	var i, comment, cond, decl;

	state.symbols.push_scope();

	comment = new IrComment("", this.location);
	irs.push(comment);

	/*
	this.init = init;
	this.condition = condition;
	this.rest_expression = rest_expression;
	this.body = body;
	*/

	this.init.ir(state, irs);

	irs.push(new IrInstruction("REP"));

	//Evaluate loop condition and break if necessary
	this.condition.ir(state, irs);
	irs.push(cond = new IrInstruction("IFN", this.condition.Dest));
	cond.d.makeSize(1);
	
	irs.push(new IrInstruction("BRK"));
	irs.push(new IrInstruction("ENDIF"));

	this.body.ir(state, irs);

	//Evaluate rest expression (e.g. i++);
	this.rest_expression.ir(state, irs);

	irs.push(new IrInstruction("ENDREP"));

	decl = [];
	for (i = 0; i < this.init.declarations.length; i++) {
		decl.push(state.symbols.get_variable(this.init.declarations[i].identifier).out);
	}

	comment.comment = util.format("for (%s) until (%s)", decl.join(", "), this.condition.Dest);
	console.log(this);

};




