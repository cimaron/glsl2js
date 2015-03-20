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
				name : 'gl_FragCoord',
				out : 'result@0',
				readonly : true
			},
			{
				position : 0,
				type : 'float',
				name : 'gl_FragDepth',
				out : 'result@0.z'
			},
			{
				position : 1,
				type : 'vec4',
				name : 'gl_FragColor',
				out : 'result@1'
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
			"int,int:int" : ["ADD %1.x %2.x %3.x"],
			"float,float:float" : ["ADD %1.x %2.x %3.x"],
			"float,vec2:vec2" : ["ADD %1.xy %2.x %3.xy"],
			"float,vec3:vec3" : ["ADD %1.xyz %2.x %3.xyz"],
			"float,vec4:vec4" : ["ADD %1 %2.x %3"],
			"vec2,float:vec2" : ["ADD %1.xy %2.xy %3.x"],
			"vec3,float:vec3" : ["ADD %1.xyz %2.xyz %3.x"],
			"vec4,float:vec4" : ["ADD %1 %2 %3.x"],
			"vec2,vec2:vec2" : ["ADD %1.xy %2.xy %3.xy"],
			"vec3,vec3:vec3" : ["ADD %1.xyz %2.xyz %3.xyz"],
			"vec4,vec4:vec4" : ["ADD %1 %2 %3"]
			},
		"-" : {
			"int,int:int" : ["SUB %1.x %2.x %3.x"],
			"float,float:float" : ["SUB %1.x %2.x %3.x"],
			"float,vec2:vec2" : ["SUB %1.xy %2.x %3.xy"],
			"float,vec3:vec3" : ["SUB %1.xyz %2.x %3.xyz"],
			"float,vec4:vec4" : ["SUB %1 %2.x %3"],
			"vec2,float:vec2" : ["SUB %1.xy %2.xy %3.x"],
			"vec3,float:vec3" : ["SUB %1.xyz %2.xyz %3.x"],
			"vec4,float:vec4" : ["SUB %1 %2 %3.x"],
			"vec2,vec2:vec2" : ["SUB %1.xy %2.xy %3.xy"],
			"vec3,vec3:vec3" : ["SUB %1.xyz %2.xyz %3.xyz"],
			"vec4,vec4:vec4" : ["SUB %1 %2 %3"]
		},
		"*" : {
			"int,int:int" : ["MUL %1.x %2.x %3.x"],
			"float,float:float" : ["MUL %1.x %2.x %3.x"],
			"float,vec2:vec2" : ["MUL %1.xy %2.x %3.xy"],
			"float,vec3:vec3" : ["MUL %1.xyz %2.x %3.xyz"],
			"float,vec4:vec4" : ["MUL %1 %2.x %3"],
			"vec2,float:vec2" : ["MUL %1.xy %2.xy %3.x"],
			"vec3,float:vec3" : ["MUL %1.xyz %2.xyz %3.x"],
			"vec4,float:vec4" : ["MUL %1 %2 %3.x"],
			"vec2,vec2:vec2" : ["MUL %1.xy %2.xy %3.xy"],
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
		"/" : {
			"int,int:int" : ["DIV %1.x %2.x %3.x"],
			"float,float:float" : ["DIV %1.x %2.x %3.x"],
			"float,vec2:vec2" : ["DIV %1.xy %2.x %3.xy"],
			"float,vec3:vec3" : ["DIV %1.xyz %2.x %3.xyz"],
			"float,vec4:vec4" : ["DIV %1 %2.x %3"],
			"vec2,float:vec2" : ["DIV %1.xy %2.xy %3.x"],
			"vec3,float:vec3" : ["DIV %1.xyz %2.xyz %3.x"],
			"vec4,float:vec4" : ["DIV %1 %2 %3.x"],
			"vec2,vec2:vec2" : ["DIV %1.xy %2.xy %3.xy"],
			"vec3,vec3:vec3" : ["DIV %1.xyz %2.xyz %3.xyz"],
			"vec4,vec4:vec4" : ["DIV %1 %2 %3"]
			},
		"<" : {
			"int,int:bool" : ["SLT %1.x %2.x %3.x"],
			"float,float:bool" : ["SLT %1.x %2.x %3.x"]
			},
		">" : {
			"int,int:bool" : ["SGT %1.x %2.x %3.x"],
			"float,float:bool" : ["SGT %1.x %2.x %3.x"]
			},
		"<=" : {
			"int,int:bool" : ["SLE %1.x %2.x %3.x"],
			"float,float:bool" : ["SLE %1.x %2.x %3.x"]
			},
		">=" : {
			"int,int:bool" : ["SGE %1.x %2.x %3.x"],
			"float,float:bool" : ["SGE %1.x %2.x %3.x"]
			},
		"==" : {
			"int,int:bool" : ["SEQ %1.x %2.x %3.x"],
			"float,float:bool" : ["SEQ %1.x %2.x %3.x"]
			},
		"!=" : {
			"int,int:bool" : ["SNE %1.x %2.x %3.x"],
			"float,float:bool" : ["SNE %1.x %2.x %3.x"]
			},
		"&&" : {
			"bool,bool:bool" : [
				"AND %1.x %2.x %3.x",
				"AND %1.x %1.x 1"
				]
			},
		"^^" : {
			"bool,bool:bool" : [
				"XOR %1.x %2.x %3.x",
				"AND %1.x %1.x 1"
				]
			},
		"||" : {
			"bool,bool:bool" : [
				"OR %1.x %2.x %3.x",
				"AND %1.x %1.x 1"
				]
			}
	},

	/**
	 * List of instructions for built in functions
	 * 
	 * Denoted by function name, then by definition of param types to output type
	 */
	func : {
		"abs": {
			"float:float" : ["ABS %1.x %2.x"],
			"vec2:vec2" : ["ABS %1.xy %2.xy"],
			"vec3:vec3" : ["ABS %1.xyz %2.xyz"],
			"vec4:vec4" : ["ABS %1 %2"],
			},
		"ceil": {
			"float:float" : ["CEIL %1.x %2.x"],
			"vec2:vec2" : ["CEIL %1.xy %2.xy"],
			"vec3:vec3" : ["CEIL %1.xyz %2.xyz"],
			"vec4:vec4" : ["CEIL %1 %2"],
			},
		"clamp": {
			"float,float,float:float" : [
				"MAX %1.x %2.x %3.x",
				"MIN %1.x %1.x %4.x"
				],
			"vec2,float,float:vec2" : [
				"MAX %1.xy %2.xy %3.x",
				"MIN %1.xy %1.xy %4.x"
				],
			"vec3,float,float:vec3" : [
				"MAX %1.xyz %2.xyz %3.x",
				"MIN %1.xyz %1.xyz %4.x"
				],
			"vec4,float,float:vec4" : [
				"MAX %1 %2 %3.x",
				"MIN %1 %1 %4.x"
				],
			"vec2,vec2,vec2:vec2" : [
				"MAX %1.xy %2.xy %3.xy",
				"MIN %1.xy %1.xy %4.xy"
				],
			"vec3,vec3,vec3:vec3" : [
				"MAX %1.xyz %2.xyz %3.xyz",
				"MIN %1.xyz %1.xyz %4.xyz"
				],
			"vec4,vec4,vec4:vec4" : [
				"MAX %1 %2 %3",
				"MIN %1 %1 %4"
				]
			},
		"cos": {
			"float:float" : ["COS %1.x %2.x"],
			"vec2:vec2" : ["COS %1.xy %2.xy"],
			"vec3:vec3" : ["COS %1.xyz %2.xyz"],
			"vec4:vec4" : ["COS %1 %2"],
			},
		"degrees": {
			"float:float" : ["MUL %1.x %2.x " + (180 / Math.PI)],
			"vec2:vec2" : ["MUL %1.xy %2.xy " + (180 / Math.PI)],
			"vec3:vec3" : ["MUL %1.xyz %2.xyz " + (180 / Math.PI)],
			"vec4:vec4" : ["MUL %1 %2 " + (180 / Math.PI)]
			},
		"dot": {
			"vec2,vec2:float" : ["DP2 %1.x %2.xy %3.xy"],
			"vec3,vec3:float" : ["DP3 %1.x %2.xyz %3.xyz"],
			"vec4,vec4:float" : ["DP4 %1.x %2 %3"]
			},
		"floor": {
			"float:float" : ["FLR %1.x %2.x"],
			"vec2:vec2" : ["FLR %1.xy %2.xy"],
			"vec3:vec3" : ["FLR %1.xyz %2.xyz"],
			"vec4:vec4" : ["FLR %1 %2"],
			},
		"fract": {
			"float:float" : ["FRC %1.x %2.x"],
			"vec2:vec2" : ["FRC %1.xy %2.xy"],
			"vec3:vec3" : ["FRC %1.xyz %2.xyz"],
			"vec4:vec4" : ["FRC %1 %2"],
			},
        "max": {
			"float,float:float" : ["MAX %1.x %2.x %3.x"],
			"vec2,float:vec2" : ["MAX %1.xy %2.xy %3.x"],
			"vec3,float:vec3" : ["MAX %1.xyz %2.xyz %3.x"],
			"vec4,float:vec4" : ["MAX %1 %2 %3.x"],
			"vec2,vec2:vec2" : ["MAX %1.xy %2.xy %3.xy"],
			"vec3,vec3:vec3" : ["MAX %1.xyz %2.xyz %3.xyz"],
			"vec4,vec4:vec4" : ["MAX %1 %2 %3"]
			},
        "min": {
			"float,float:float" : ["MIN %1.x %2.x %3.x"],
			"vec2,float:vec2" : ["MIN %1.xy %2.xy %3.x"],
			"vec3,float:vec3" : ["MIN %1.xyz %2.xyz %3.x"],
			"vec4,float:vec4" : ["MIN %1 %2 %3.x"],
			"vec2,vec2:vec2" : ["MIN %1.xy %2.xy %3.xy"],
			"vec3,vec3:vec3" : ["MIN %1.xyz %2.xyz %3.xyz"],
			"vec4,vec4:vec4" : ["MIN %1 %2 %3"]
			},
        "mix": {
			"float,float,float:float" : [
				"MAD %1.x -%2.x %4.x %2.x",
				"MAD %1.x %3.x %4.x %1.x"
				],
			"vec2,vec2,float:vec2" : [
				"MAD %1.xy -%2.xy %4.x %2.xy",
				"MAD %1.xy %3.xy %4.x %1.xy"
				],
			"vec3,vec3,float:vec3" : [
				"MAD %1.xyz -%2.xyz %4.x %2.xyz",
				"MAD %1.xyz %3.xyz %4.x %1.xyz"
				],
			"vec4,vec4,float:vec4" : [
				"MAD %1 -%2 %4.x %2",
				"MAD %1 %3 %4.x %1"
				],
			"vec2,vec2,vec2:vec2" : [
				"MAD %1.xy -%2.xy %4.xy %2.xy",
				"MAD %1.xy %3.xy %4.xy %1.xy"
				],
			"vec3,vec3,vec3:vec3" : [
				"MAD %1.xyz -%2.xyz %4.xyz %2.xyz",
				"MAD %1.xyz %3.xyz %4.xyz %1.xyz"
				],
			"vec4,vec4,vec4:vec4" : [
				"MAD %1 -%2 %4 %2",
				"MAD %1 %3 %4 %1"
				]
			},
        "mod": {
			"float,float:float" : ["MOD %1.x %2.x %3.x"],
			"vec2,float:vec2" : ["MOD %1.xy %2.xy %3.x"],
			"vec3,float:vec3" : ["MOD %1.xyz %2.xyz %3.x"],
			"vec4,float:vec4" : ["MOD %1 %2 %3.x"],
			"vec2,vec2:vec2" : ["MOD %1.xy %2.xy %3.xy"],
			"vec3,vec3:vec3" : ["MOD %1.xyz %2.xyz %3.xyz"],
			"vec4,vec4:vec4" : ["MOD %1 %2 %3"]
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
		"radians": {
			"float:float" : ["MUL %1.x %2.x " + (Math.PI / 180)],
			"vec2:vec2" : ["MUL %1.xy %2.xy " + (Math.PI / 180)],
			"vec3:vec3" : ["MUL %1.xyz %2.xyz " + (Math.PI / 180)],
			"vec4:vec4" : ["MUL %1 %2 " + (Math.PI / 180)],
			},
		"sign": {
			"float:float" : [
				"SGT %t1.x %2.x 0",
				"SLT %t1.y %2.x 0",
				"ADD %1.x %t1.x -%t1.y"
				],
			"vec2:vec2" : [
				"SGT %t1.xy %2.xy 0",
				"SLT %t1.zw %2.zw 0",
				"ADD %1.xy %t1.xy -%t1.zw"
				],
			"vec3:vec3" : [
				"SGT %t1.xyz %2.xyz 0",
				"SLT %t2.xyz %2.xyz 0",
				"ADD %1.xyz %t1.xyz -%t2.xyz"
				],
			"vec4:vec4" : [
				"SGT %t1 %2 0",
				"SLT %t2 %2 0",
				"ADD %1 %t1 -%t2"
				],
			},
		"sin": {
			"float:float" : ["SIN %1.x %2.x"],
			"vec2:vec2" : ["SIN %1.xy %2.xy"],
			"vec3:vec3" : ["SIN %1.xyz %2.xyz"],
			"vec4:vec4" : ["SIN %1 %2"],
			},
		"step": {
			"float,float:float" : ["SGE %1.x %3.x %2.x"],
			"float,vec2:vec2" : ["SGE %1.xy %3.x %2.xy"],
			"float,vec3:vec3" : ["SGE %1.xyz %3.x %2.xyz"],
			"float,vec4:vec4" : ["SGE %1 %3.x %2"],
			"vec2,vec2:vec2" : ["SGE %1.xy %3.xy %2.xy"],
			"vec3,vec3:vec3" : ["SGE %1.xyz %3.xyz %2.xyz"],
			"vec4,vec4:vec4" : ["SGE %1 %3 %3"],
			},
		"tan": {
			"float:float" : ["TAN %1.x %2.x"],
			"vec2:vec2" : ["TAN %1.xy %2.xy"],
			"vec3:vec3" : ["TAN %1.xyz %2.xyz"],
			"vec4:vec4" : ["TAN %1 %2"],
			},
		"texture2D": {
			"sampler2D,vec2:vec4" : ["TEX %1 %3 %2 \"2D\""]
			},
		"textureCube": {
			"samplerCube,vec3:vec4" : ["TEX %1 %3 %2 \"CUBE\""]
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


function symbol_table_init(state, table, target) {
	var i, j, vars, v, entry, types, name;

	vars = (target === glsl.target.vertex) ? builtin.vars.vertex : builtin.vars.fragment;

	for (i = 0; i < vars.length; i++) {
		v = vars[i];
		entry = table.add_variable(v.name, v.type);
		entry.position = v.position;
		entry.out = v.out;
		entry.readonly = v.readonly;
	}

	vars = builtin.func;

	for (name in vars) {
		v = vars[name];
		for (j in v) {
			types = _builtinParseType(j);
			entry = table.add_function(name, types.dest, types.src);
			entry.code = v[j];
		}
	}

	state.extensions.call('symbol_table_init', [state, table, target]);
}

