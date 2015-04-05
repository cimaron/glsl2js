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

glsl.events = {};

/**
 * Add an event listener
 *
 * @param   string     name       Event name
 * @param   function   listener   Callback
 */
glsl.on = function(name, listener) {

	this.events[name] = this.events[name] || [];
	
	this.events[name].push(listener);
};

/**
 * Call all listeners attached to event
 *
 * @param   string   name   Event name
 * @param   array    args   Arguments to pass to listeners
 *
 * @return  array
 */
glsl.fire = function(name, args) {
	var list, ret, e, i;

	list = this.events[name];
	ret = [];

	if (!list) {
		return [];
	}

	for (i = 0; i < list.length; i++) {
		e = list[i];
		ret.push(e.apply(null, args));
	}

	return ret;
};

/**
 * Remove an event listener
 *
 * @param   string     name        Event name
 * @param   function   listener   Callback
 */
glsl.removeListener = function(name, listener) {
	var idx;

	if (this.events[name] && (idx = this.events[name].indexOf(listener)) !== -1) {
		this.events[name].splice(idx, 1);
	}
};


