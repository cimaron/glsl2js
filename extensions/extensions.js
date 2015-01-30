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
CONNECTION WITH THE SOFTWARE OR THE USE		 OR OTHER DEALINGS IN THE SOFTWARE.
*/


function GlslExtensions() {
	this.enabled = [];
}

proto = GlslExtensions.prototype = {};


/**
 * Enable extension
 *
 * @param   string   name   Name of extensions
 *
 * @return  bool
 */
proto.enable = function(name) {

	if (!(name in glsl.extensions)) {
		return false;	
	}

	if (this.enabled.indexOf(name) === -1) {
		this.enabled.push(name);
	}

	return true;
};

/**
 * Disable extension
 *
 * @param   string   name   Name of extensions
 *
 * @return  bool
 */
proto.disable = function(name) {
	var i;

	if (!(name in glsl.extensions)) {
		return false;	
	}

	i = this.enabled.indexOf(name);

	if (i !== -1) {
		this.enabled.splice(i, 1);
	}

	return true;
};

/**
 * Enable all extensions
 */
proto.enableAll = function() {
	this.enabled = Object.keys(glsl.extensions);
};

/**
 * Disable all extensions
 */
proto.disableAll = function() {
	this.enabled = [];
};

/**
 * Execute extension hook on a single extension
 *
 * @param   string   name   Extension name
 * @param   string   hook   Hook name
 * @param   array    args   Arguments
 *
 * @return  array
 */
proto.callSingle = function(name, hook, args) {
	var ext, res;

	if (this.enabled.indexOf(name) === -1) {
		return false;
	}

	ext = glsl.extensions[name];

	if (!ext) {
		return false;	
	}

	if (ext[hook]) {
		res = ext[hook].apply(ext, args);
	}

	return res;
};

/**
 * Execute extension hook on all enabled extensions
 *
 * @param   string   hook   Hook name
 * @param   array    args   Arguments
 * @param   string   name   Specific extension (optional)
 *
 * @return  array
 */
proto.call = function(hook, args, name) {
	var i, ext, res = [];

	for (i = 0; i < this.enabled.length; i++) {
		res.push(this.callSingle(this.enabled[i], hook, args));
	}

	return res;
};

