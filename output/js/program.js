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
 * GlslProgramJS class
 */
function GlslProgramJS() {
	GlslProgram.apply(this, arguments);

	this.context = new GlslProgramJSContext(this.options);
}

util.inherits(GlslProgramJS, GlslProgram);
glsl.program.js = GlslProgramJS;

var proto = GlslProgramJS.prototype;

GlslProgramJS.translation_table = {
	'ABS'  : '%1.* = Math.abs(%2.*);',
	'ADD'  : '%1.* = %2.* + %3.*;',
	'AND'  : '%1.* = %2.* & %3.*;',
	'BRK'  : 'break;',
	//'ARL' : false,
	'CEIL' : '%1.* = Math.ceil(%2.*);',
	'CMP'  : '%1.* = (%2.* < 0.0) ? %3.* : %4.*;',
	'COS'  : '%1.* = Math.cos(%2.*);',
	'DIV'  : '%1.* = %2.* / %3.*;',
	'DP2'  : '%1.x = (%2.x * %3.x) + (%2.y * %3.y);',
	'DP3'  : '%1.x = (%2.x * %3.x) + (%2.y * %3.y) + (%2.z * %3.z);',
	'DP4'  : '%1.x = (%2.x * %3.x) + (%2.y * %3.y) + (%2.z * %3.z) + (%2.w * %3.w);',
	//'DPH' : '%1.* = (%2.x * %3.x + %2.y * %3.y + %2.z + %3.z + %3.w);',
	//'DST' : '%1.* = [1, %2.y * %3.y, %2.z, %3.w];',
	'ELSE' : '} else {',
	'ENDIF': '}',
	'ENDREP' : '}',
	'EVAL' : '%1;',
	'FLR'  : '%1.* = Math.floor(%2.*);',
	'FRC'  : '%1.* = %2.* - Math.floor(%2.*);',
	'IF'   : 'if (%1.*) {',
	'IFN'  : 'if (!%1.*) {',
	'MAD'  : '%1.* = (%2.* * %3.*) + %4.*;',
	'MAX'  : '%1.* = Math.max(%2.*, %3.*);',
	'MIN'  : '%1.* = Math.min(%2.*, %3.*);',
	'MOD'  : '%1.* = %2.* % %3.*;',
	'MOV'  : '%1.* = %2.*;',
	'MUL'  : '%1.* = %2.* * %3.*;',
	'OR'   : '%1.* = %2.* | %3.*;',
	'POW'  : '%1.x = Math.pow(%2.x, %3.x);',
	'REP'  : 'while (true) {',
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
 * Build a program
 *
 * @return  function
 */
proto.build = function() {
	var i, module, shaders, init, start, vars;

	vars = {
		uniform_f32   : Math.max(this.options.max_vertex_uniform_vectors, this.options.max_fragment_uniform_vectors),
		attribute_f32 : this.options.max_vertex_attribute_vectors,
		varying_f32   : this.options.max_varying_vectors,
		result_f32    : 2,
		temp_f32      : this.options.max_register_vectors,
		jstemp_f32    : 1
	}

	code = [];
	//init.push("\"use asm\";");
	code.push("var");

	start = 0;
	for (i in vars) {
		count = vars[i];
		code.push(util.format("%s = new stdlib.Float32Array(heap, %d, %d),", i, start, count * 4));
		start += count * 16;
	}

	code.push("tex2D = foreign.tex2D,");
	code.push("texCUBE = foreign.texCUBE;");

	code.push("function vs() {");
	code.push(this.vertex_code.join("\n"));
	code.push("}");

	code.push("function fs() {");
	code.push(this.fragment_code.join("\n"));
	code.push("}");
	
	code.push("return { fragment : fs, vertex : vs };");

	module = new Function("stdlib", "foreign", "heap", code.join("\n"));

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
		this.current.push("");
		this.current.push('// ' + ins.toString().replace("\n", ""));
		return;
	}

	this.current.push('// ' + ins.toString());

	if (!(tpl = GlslProgramJS.translation_table[ins.op])) {
		throw new Error(util.format("Could not translate opcode '%s'", ins.op));
	}

	if (ins.op == 'EVAL') {
		tpl = tpl.replace(/%1/g, ins.d.full);
		ins.d = ins.s1;
		ins.s1 = ins.s2;
		ins.s2 = ins.s3;
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
		js = tpl.replace(/tex/, 'tex' + src[2].name);
		js = js.replace(/%1/g, dest.name);
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
			name = 'jstemp_f32';
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
		out = out.replace(new RegExp(from + '\.' + swz[i], 'g'), op.raw ? name : util.format("%s[%s]", name, op.start + i));
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
				this.current.push(util.format("jstemp_f32[%s] = %s[%s]", i, op.name, op.start + op.components[i]));
			}
		}
	}
	
	//console.log(tpl, dest, src);
	//debugger;
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




