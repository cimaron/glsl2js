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
function GlslProgramVars() {
	this.uniform = {};
	this.attribute = {};
	this.varying = {};
}

var proto = GlslProgramVars.prototype;


/**
 * Add uniform variable
 */
proto.addUniform = function(name, pos, slots, comp) {

	this.uniform[name] = new GlslProgramVar(name, pos, slots, comp, 'uniform');
	
	return this.uniform[name];	
};

/**
 * Add attribute variable
 */
proto.addAttribute = function(name, pos, slots, comp) {

	this.attribute[name] = new GlslProgramVar(name, pos, slots, comp, 'attribute');

	return this.attribute[name];	
};

/**
 * Add varying variable
 */
proto.addVarying = function(name, pos, slots, comp) {

	this.varying[name] = new GlslProgramVar(name, pos, slots, comp, 'varying');

	return this.varying[name];
};



/**
 * GlslProgramVar Class
 */
function GlslProgramVar(name, pos, slots, comp, type) {
	this.name = name;
	this.pos = pos;
	this.slots = slots;
	this.components = comp;
	this.type = type;
}

