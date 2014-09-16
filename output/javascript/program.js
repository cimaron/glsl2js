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
 * GlslProgramJavascript class
 */
function GlslProgramJavascript() {

	this.symbols = {};
	this.vertex_code = [];
	this.fragment_code = [];

	this.vertex = null;
	this.shader = null;
}

var proto = GlslProgramJavascript.prototype;

GlslProgramJavascript.translation_table = {
	'ABS' : '%1* = Math.abs(%2*)',
	'ADD' : '%1* = (%2*) + (%3*)',
	//'ARL' : false,
	'CMP' : '%1* = ((%2* < 0.0) ? (%3*) : (%4*))',
	//'COS' : 'Math.cos(%2)',
	'DP3' : '%1[0] = ((%2[0]) * (%3[0]) + (%2[1]) * (%3[1]) + (%2[2]) * (%3[2]))',
	'DP4' : '%1[0] = ((%2[0]) * (%3[0]) + (%2[1]) * (%3[1]) + (%2[2]) * (%3[2]) + (%2[3]) * (%3[3]))',
	//'DPH' : '%1.* = (%2.x * %3.x + %2.y * %3.y + %2.z + %3.z + %3.w)',
	//'DST' : '%1.* = [1, %2.y * %3.y, %2.z, %3.w]',
	'ELSE'  : '} else {',
	'ENDIF' : '}', 
	'IF'  : 'if (%1[0]) {',
	'MAD' : '%1.* = (%2.* * %3.*) + %4.*;',
	'MAX' : '%1* = Math.max((%2*), (%3*))',
	'MOV' : '%1.* = %2.*;',
	'MUL' : '%1.* = %2.* * %3.*;',
	'POW' : '%1[0] = Math.pow(%2[0], %3[0])',
	'RET' : 'return',
	'RSQ' : '%1* = (1.0 / Math.sqrt(%2*))',
	'SEQ' : '%1.* = (%2.* === %3.*) ? 1.0 : 0.0',
	'SGE' : '%1* = (%2* >= %3*) ? (1.0) : (0.0)',
	'SLT' : '%1* = (%2* <  %3*) ? (1.0) : (0.0)',
	'SUB' : '%1* = (%2*) - (%3*)',
	'TEX' : '%1.* = tex(%3, %2[0], %2[1], 0)'
}; 


/**
 * Translates IR code into a javascript representation
 *
 * @return  bool      true if there were no errors
 */
proto.translate = function(irs, target) {
	var i, errors;

	//optimize(irs, symbols);

	this.current = [];

	for (i = 0; i < irs.code.length; i++) {
		try {
			this.instruction(irs.code[i]);
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
 * Build a program
 *
 * @return  function
 */
proto.build = function(stdlib, foreign, heap) {

	var module, shaders;

	module = new Function("stdlib", "foreign", "heap",
		"\"use asm\";\n" +
		"var temp = new stdlib.Float32Array(heap, 0, 128),\n" +
		"jstemp = new stdlib.Float32Array(heap, 124, 4),\n" +
		"vertex = new stdlib.Float32Array(heap, 128, 128),\n" +
		"fragment = new stdlib.Float32Array(heap, 256, 128),\n" +
		"result = new stdlib.Float32Array(heap, 384, 128)\n" +
		"uniform = new stdlib.Float32Array(heap, 512, 128)\n" +
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

	shaders = module(stdlib, foreign, heap);

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
		js = tpl;

		if (dest) {
			js = js.replace('%1.*', util.format("%s[%s]", dest.name, dest.start + dest.components[i]));
		}

		for (j = 0; j < 3; j++) {
			
			if (!src[j]) {
				continue;	
			}

			if (src[j].components) {
				js = js.replace('%' + (j + 2) + '.*', util.format("%s[%s]", src[j].name, src[j].start + src[j].components[i]));
			} else {
				js = js.replace('%' + (j + 2) + '.*', src[j].name);
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
	
	out.name = opr.name;
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
 * Make a new program data context
 */
proto.makeContext = function() {
	var ctx;
	ctx = new GlslProgramJavascriptContext();
	return ctx;
};

glsl.translator = GlslProgramJavascript;



