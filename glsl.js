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

var glsl = {
	
	compile : function(src, options) {
		var state,
			result,
		    irs
		    ;

		state = new GlslState(options);
		state.setSource(src);

		this.fire('init', [state]);

		//Preprocess
		result = this.preprocessor.process(state);
		this.fire('preprocess', [state]);

		//Parse into AST
		if (result) {
			result = this.parser.parse(state);
			this.fire('parse', [state]);
		}

		//Generate IR
		if (result) {
			result = this.generate(state);
			this.fire('generate', [state]);
		}

		if (result) {
			state.status = true;
			this.fire('complete', [state]);
		} else {
			this.fire('fail', [state]);	
		}

		return state;
	},

	/**
	 * Extensions
	 */
	extensions : {},

	/**
	 * Compilation targets
	 */
	target : {
		fragment : 0,
		'x-fragment' : 0,
		'x-shader/x-fragment' : 0,
		vertex : 1,
		'x-vertex' : 1,
		'x-shader/x-vertex' : 1
	}
};


