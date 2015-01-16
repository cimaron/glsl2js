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
		tex : function(dest, i, sampler, src, j, dim) {
			dest[i] = 0;
			dest[i + 1] = 0;
			dest[i + 2] = 0;
			dest[i + 3] = 1;
		}
	};

	this.vertex = null;
	this.shader = null;
}

var proto = GlslProgramJavascript.prototype;

GlslProgramJavascript.translation_table = {
	'ABS'  : '%1.* = Math.abs(%2.*);',
	'ADD'  : '%1.* = %2.* + %3.*;',
	'AND'  : '%1.* = %2.* & %3.*;',
	//'ARL' : false,
	'CEIL' : '%1.* = Math.ceil(%2.*);',
	'CMP'  : '%1.* = (%2.* < 0.0) ? %3.* : %4.*;',
	'COS'  : '%1.* = Math.cos(%2.*);',
	'DIV'  : '%1.* = %2.* / %3.*;',
	'DBGR' : 'debugger;',
	'DP2'  : '%1.x = (%2.x * %3.x) + (%2.y * %3.y);',
	'DP3'  : '%1.x = (%2.x * %3.x) + (%2.y * %3.y) + (%2.z * %3.z);',
	'DP4'  : '%1.x = (%2.x * %3.x) + (%2.y * %3.y) + (%2.z * %3.z) + (%2.w * %3.w);',
	//'DPH' : '%1.* = (%2.x * %3.x + %2.y * %3.y + %2.z + %3.z + %3.w);',
	//'DST' : '%1.* = [1, %2.y * %3.y, %2.z, %3.w];',
	'ELSE' : '} else {',
	'ENDIF': '}',
	'FLR'  : '%1.* = Math.floor(%2.*);',
	'FRC'  : '%1.* = %2.* - Math.floor(%2.*);',
	'IF'   : 'if (%1.x) {',
	'MAD'  : '%1.* = (%2.* * %3.*) + %4.*;',
	'MAX'  : '%1.* = Math.max(%2.*, %3.*);',
	'MIN'  : '%1.* = Math.min(%2.*, %3.*);',
	'MOD'  : '%1.* = %2.* % %3.*;',
	'MOV'  : '%1.* = %2.*;',
	'MUL'  : '%1.* = %2.* * %3.*;',
	'OR'   : '%1.* = %2.* | %3.*;',
	'POW'  : '%1.x = Math.pow(%2.x, %3.x);',
	'RET'  : 'return;',
	'RSQ'  : '%1.* = (1.0 / Math.sqrt(%2.*));',
	'SEQ'  : '%1.* = (%2.* === %3.*) ? 1.0 : 0.0;',
	'SGE'  : '%1.* = (%2.* >=  %3.*) ? 1.0 : 0.0;',
	'SGT'  : '%1.* = (%2.* >   %3.*) ? 1.0 : 0.0;',
	'SIN'  : '%1.* = Math.sin(%2.*);',
	'SLE'  : '%1.* = (%2.* <=  %3.*) ? 1.0 : 0.0;',
	'SLT'  : '%1.* = (%2.* <   %3.*) ? 1.0 : 0.0;',
	'SNE'  : '%1.* = (%2.* !== %3.*) ? 1.0 : 0.0;',
	'SUB'  : '%1.* = %2.* - %3.*;',
	'TAN'  : '%1.* = Math.tan(%2.*);', //Non-standard opcode for NV_gpu
	'TEX'  : 'tex(%1, %4, %2, %5, %3.x, 0);', //%4 = address of %1, %5 = address of %2
	'XOR'  : '%1.* = %2.* ^ %3.*;'
}; 

/**
 * Return string representation of program
 *
 * @param   int   target   target
 *
 * @return  string
 */
proto.toString = function(target) {

	if (target === glsl.target.fragment) {
		return this.fragment_code.join("\n");	
	} else if (target === glsl.target.vertex) {
		return this.vertex_code.join("\n");	
	} else {
		return this.current.join("\n");
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
			this.error = util.format("%s at %s:%s", e.message, e.lineNumber, e.columnNumber);
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
	
	addresses.sort();
	
	//Can insert at beginning
	if (addresses[0] >= symbol.slots) {
		return 0;
	}

	//Can insert in between
	for (i = 1; i < addresses.length; i += 2) {		
		last = addresses[i];
		next = addresses[i];
		
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
proto.build = function() {

	var module, shaders;

	module = new Function("stdlib", "foreign", "heap",
		"//\"use asm\";\n" +
		"var\n" +
		"uniform_f32   = new stdlib.Float32Array(heap,   0, 128),\n" +
		"attribute_f32 = new stdlib.Float32Array(heap, 512, 128),\n" +
		"varying_f32   = new stdlib.Float32Array(heap, 1024, 128),\n" +
		"result_f32    = new stdlib.Float32Array(heap, 1536, 128),\n" +
		"temp_f32      = new stdlib.Float32Array(heap, 2048, 128),\n" +
		"jstemp        = new stdlib.Float32Array(heap, 2544,   4),\n" +
		"tex           = foreign.tex;\n" +
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
		this.current.push('// ' + ins.toString().replace("\n", ""));
		return;
	}

	this.current.push('// ' + ins.toString());

	if (!(tpl = GlslProgramJavascript.translation_table[ins.op])) {
		throw new Error(util.format("Could not translate opcode '%s'", ins.op));
	}

	//variables
	dest = this.buildComponents(ins.d, true);

	if (!dest) {
		this.current.push(tpl);
		return;
	}

	src = [];
	src.push(this.buildComponents(ins.s1));
	src.push(this.buildComponents(ins.s2));
	src.push(this.buildComponents(ins.s3));

	if (ins.op == 'TEX') {
		js = tpl.replace(/%1/g, dest.name);
		js = js.replace(/%2/g, src[0].name);
		js = this.replaceOperand(js, '%3', src[1], 0);
		js = js.replace(/%4/g, dest.start);
		js = js.replace(/%5/g, src[0].start);

		this.current.push(js);
		this.current.push("");
		return;
	}

	this.generateTemp(dest, src, tpl);

	for (i = 0; i < dest.components.length; i++) {

		js = this.replaceOperand(tpl, '%1', dest, i);

		for (j = 0; j < 3; j++) {
			
			if (src[j]) {
				js = this.replaceOperand(js, '%' + (j + 2), src[j], i);
			}

		}

		this.current.push(js);
	}

	this.current.push("");
};


/**
 * Replace an operand into code template
 *
 * @param   string   tpl    Template
 * @param   string   from   Template operand
 * @param   object   op     Operand info
 * @param   int      n      Current component iteration  
 */
proto.replaceOperand = function(tpl, from, op, n) {
	var i,
	    out,
	    name,
	    addr,
	    swz = ['x', 'y', 'z', 'w']
		;

	if (op.raw) {
		name = op.name;
	} else {
		if (op.jstemp && op.jstemp[n]) {
			name = 'jstemp';
			addr = n;
		} else {
			name = op.name;
			if (op.components) {
				addr = op.start + op.components[n];
			}
		}
	}

	if (op.components) {
		out = tpl.replace(from + '.*', util.format("%s[%s]", name, addr));
	} else {
		out = tpl.replace(from + '.*', name);
	}

	for (i = 0; i < swz.length; i++) {
		out = out.replace(new RegExp(from + '\.' + swz[i], 'g'), util.format("%s[%s]", name, op.start + i));
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
		out.raw = true;
		return out;
	}

	out.name = opr.neg + opr.name + '_f32';
	out.start = 4 * opr.address + 4 * opr.index;
	out.components = [];
	out.jstemp = [];

	//generate array representation of swizzle components, expanding if necessary
	swz = opr.swizzle || "xyzw";
	swz = swz.split("");

	for (i = 0; i < 4; i++) {
		//exact swizzle specified and less than 4 components, grab last one
		if (swz.length <= i) {
			if (!dest) {
				//repeat last one
				out.components.push(out.components[i - 1]);	
				out.jstemp.push(null);
			}
		} else {
			out.components.push("xyzw".indexOf(swz[i]));
			out.jstemp.push(null);
		}
	}

	return out;
};

proto.generateTemp = function(dest, src, tpl) {
	var i,
	    c,
		op,
	    written
		;
	
	for (i = 0; i < dest.components.length; i++) {
		written = dest.components.slice(0, i);

		for (c = 0; c < src.length; c++) {
			op = src[c];
			if (op && op.name == dest.name && op.start == dest.start && written.indexOf(op.components[i]) != -1) {
				op.jstemp[i] = true;
				this.current.push(util.format("jstemp[%s] = %s[%s]", i, op.name, op.start + op.components[i]));
			}
		}
	}
	
	//console.log(tpl, dest, src);
	//debugger;
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
	this.library.tex = func;
};


glsl.program = GlslProgramJavascript;



