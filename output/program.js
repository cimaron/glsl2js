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
 * GlslProgram class
 *
 * @param   object   options   Configuration
 */
function GlslProgram(options) {

	this.setOptions(options);
	
	this.vertex_code = [];
	this.fragment_code = [];
	this.errors = [];

	this.symbols = new GlslProgramVars(this.options);

	this.library = {
		tex2D : function(dest, i, sampler, src, j, dim) {
			dest[i] = 0;
			dest[i + 1] = 0;
			dest[i + 2] = 0;
			dest[i + 3] = 1;
		},
		texCUBE : function(dest, i, sampler, src, j, dim) {
			dest[i] = 0;
			dest[i + 1] = 0;
			dest[i + 2] = 0;
			dest[i + 3] = 1;
		}
	};

	this.vertex = null;
	this.shader = null;
}


//@note: Double webgl minimum
GlslProgram.constants = {
	MAX_VERTEX_ATTRIBUTE_VECTORS : 16,
	MAX_VERTEX_UNIFORM_VECTORS : 256,
	MAX_VARYING_VECTORS : 16,
	MAX_FRAGMENT_UNIFORM_VECTORS : 32,
	MAX_REGISTER_VECTORS : 128
};

var proto = GlslProgram.prototype;

/**
 * Set program options
 *
 * @param   object   options   Configuration
 */
proto.setOptions = function(options) {
	var i, n;

	this.options = {};
	options = options || {};

	for (i in GlslProgram.constants) {
		n = i.toLowerCase();
		this.options[n] = options[n] || GlslProgram.constants[i];	
	}
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
			this.errors.push(util.format("%s at %s:%s", e.message, e.lineNumber, e.columnNumber));
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
	var s, t, n, entry, sym, start, slots, comp;
	
	for (s in object.symbols) {

		t = object.symbols[s].entries;	

		for (n in t) {

			entry = t[n];
			start = parseInt(entry.out.split('@')[1]);
			slots = entry.getType().slots;
			comp = entry.getType().size / slots;

			if (s == 'uniform') {
				
				sym = this.symbols.addUniform(entry.name, start, slots, comp);
				
				if (this.findSymbolCollision(this.symbols.uniform, sym)) {
					this.rewriteSymbol(this.symbols.uniform, sym, object);
				}

			} else if (s == 'attribute') {					
				this.symbols.addAttribute(entry.name, start, slots, comp);
			} else if (s == 'varying') {
				this.symbols.addVarying(entry.name, start, slots, comp);				
			}

		}
	}
};

/**
 * Scan symbol table to find collisions
 */
proto.findSymbolCollision = function(table, symbol) {
	var i, my_start, my_end, start, end;

	my_start = symbol.pos;
	my_end = my_start + symbol.slots - 1;

	for (i in table) {

		if (i == symbol.name) {
			continue;	
		}
		
		start = table[i].pos;
		end = start + table[i].slots - 1;
		
		if ((my_start >= start && my_start <= end) || (my_end >= start && my_end <= end)) {
			return true;
		}
		
	}

	return false;
};

/**
 * Rewrite symbol table entry position in code
 */
proto.findNewSymbolPosition = function(table, symbol) {
	var i, size, addresses, last, next;

	addresses = [];

	//find new address
	for (i in table) {
		
		if (symbol.name == i) {
			continue;	
		}
		
		//start address
		addresses.push(table[i].pos);
		
		//end address
		addresses.push(table[i].pos + table[i].slots - 1);
	}
	
	addresses.sort(function(a, b) {
		return a - b;
	});

	//Can insert at beginning
	if (addresses[0] >= symbol.slots) {
		return 0;
	}

	//Can insert in between
	for (i = 1; i < addresses.length; i += 2) {		
		last = addresses[i];
		next = addresses[i + 1];
		
		if (next - last - 1 > symbol.slots) {
			return last + 1;	
		}
	}

	//Can insert at end

	return addresses.slice(-1)[0] + 1;
};

/**
 * Rewrite symbol table entry position in code
 */
proto.rewriteSymbol = function(table, symbol, object) {
	var pos, old_start, old_end, diff, i, ins;

	old_start = symbol.pos;
	old_end = old_start + symbol.slots - 1;

	symbol.pos = this.findNewSymbolPosition(table, symbol);
	diff = symbol.pos - old_start;

	for (i = 0; i < object.code.length; i++) {

		ins = object.code[i];
		
		if (!(ins instanceof IrInstruction)) {
			continue;	  
		}

		this.rewriteOperandAddress(ins.d, old_start, old_end, diff, symbol);
		this.rewriteOperandAddress(ins.s1, old_start, old_end, diff, symbol);
		this.rewriteOperandAddress(ins.s2, old_start, old_end, diff, symbol);
		this.rewriteOperandAddress(ins.s3, old_start, old_end, diff, symbol);
	}
};

/**
 * Rewrite symbol table entry position in code
 */
proto.rewriteOperandAddress = function(oprd, old_start, old_end, diff, symbol) {
	var diff;
	
	if (!oprd) {
		return;	
	}

	if (oprd.name != symbol.type) {
		return;
	}

	if (oprd.address >= old_start && oprd.address <= old_end) {
		oprd.address += diff;
	}
};

/**
 * Build a program
 *
 * @return  function
 */
/* abstract */ proto.build = function() {
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
 * Set TEX lookup function
 *
 * @param   function   func   Texture function 
 */
proto.setTexFunction2D = function(func) {
	this.library.tex2D = func;
};

/**
 * Set TEX lookup function
 *
 * @param   function   func   Texture function 
 */
proto.setTexFunctionCube = function(func) {
	this.library.texCUBE = func;
};

/**
 * Program Constructor
 *
 * @param   string   type   Program type
 */
glsl.createProgram = function(type, options) {
	var cons;

	cons = this.program[type];

	if (!cons) {
		cons = GlslProgram;
	}

	return new cons(options);
}

glsl.program = {};


