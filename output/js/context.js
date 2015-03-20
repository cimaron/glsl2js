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
 * GlslProgramJSContext class
 */
function GlslProgramJSContext(options) {
	var uniform, attribute, varying, result, temp, jstemp, total, start;

	uniform = Math.max(options.max_vertex_uniform_vectors, options.max_fragment_uniform_vectors);
	attribute = options.max_vertex_attribute_vectors;
	varying = options.max_varying_vectors;
	result = 2;	
	temp = options.max_register_vectors;
	jstemp = 1;

	total = uniform + attribute + varying + result + temp + jstemp;

	//Each vector is 16 bytes (4 comp * 4 bytes)
	this.heap = new ArrayBuffer(total * 16);

	start = 0;
	this.uniform_f32 = new Float32Array(this.heap, start, uniform * 4);

	start += (uniform * 16);
	this.attribute_f32 = new Float32Array(this.heap, start, attribute * 4);

	start += (attribute * 16);
	this.varying_f32 = new Float32Array(this.heap, start, varying * 4);

	start += (varying * 16);
	this.result_f32 = new Float32Array(this.heap, start, result * 4);
}

var proto = GlslProgramJSContext.prototype;


