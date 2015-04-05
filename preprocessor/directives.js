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

Preprocessor.modules.directives = {

	state : {
		lines : [],
		defines : {}
	},

	process : function(src, state) {
		var i, l;
		
		this.state.lines = src.split("\n");
		this.state.defines = {
			GL_ES : '1',
			__FILE__ : '0',
			__LINE__ : '0',
			__VERSION__ : '300'
		};
		this.state.cond_stack = [];
		this.state.compile_state = state;

		i = 0;
		l = this.state.lines.length;

		while (i < l) {
			this.state.lines[i] = this.processLine(this.state.lines[i], i);
			i++;
		}

		return this.state.lines.join("\n");
	},

	processLine : function(line, i) {
		var d, matches, raw, e, data, sub, cond, lmatches, res;

		matches = line.match(/^([ \t]*)#(.*)$/);
		if (!matches) {

			if (this.state.cond_stack.length !== 0 && !this.state.cond_stack.slice(-1)[0]) {
				return "";
			}

			line = this.processDefines(line, i);

			return line;
		}

		raw = matches[2];

		if (raw.match(/^\s*$/)) {
			return "";
		}

		lmatches = raw.split(/\s+/);

		data = {
			modstate : this.state,
			line : i + 1,
			text : line,
			matches : lmatches
		};

		res = glsl.fire('preprocess.directive', [this.state.compile_state, data]);

		try {

			switch (lmatches[0]) {
				
				case 'define':
				case 'undef':
				case 'ifdef':
				case 'endif':
				case 'extension':
					this[lmatches[0]](line, lmatches);
					return "";
			}

			throw new Error("Invalid directive");

		} catch (e) {

			e.lineNumber = i + 1;
			e.columnNumber = matches[1].length + 1;
	
			throw e;
		}

	},

	processDefines : function(line, i) {
		
		this.state.defines.__LINE__ = i + 1;

		for (d in this.state.defines) {
			//easy global replace
			line = line.split(d).join(this.state.defines[d]);
		}

		return line;
	},

	define : function(line, matches) {

		if (matches.length <= 1 || matches.length > 3) {
			throw new Error("Syntax error in #define");
		}

		this.state.defines[matches[1]] = matches[2] || "";		
	},

	extension : function(line, matches) {
 		var ext_name, behavior, all, ext_warn, extensions;
 
		if (matches.length <= 1 || matches.length >= 5 || matches[2] != ':') {
			throw new Error("Syntax error in #extension");
		}
 
 		ext_name = matches[1];
 		behavior = matches[3];
		all = (ext_name == 'all');

		extensions = this.state.compile_state.extensions;

		if (!all && !(ext_name in glsl.extensions)) {
			ext_warn = util.format("Extension %s not supported", ext_name);
		}

		if (all && (behavior == 'require' || behavior == 'enable')) {
			throw new Error("Syntax error in #extension");				
		}	

		switch (behavior) {
			
			case 'require':

				if (ext_warn) {
					throw new Error(ext_warn);
				}

				extensions.enable(ext_name);
				
				break;

			case 'enable':

				if (ext_warn) {
					this.state.compile_state.addWarning(ext_warn);
					return;
				}

				extensions.enable(ext_name);

				break;
			
			case 'disable':

				if (all) {
					extensions.disableAll();
					return;
				}

				if (ext_warn) {
					this.state.compile_state.addWarning(ext_warn);
					return;
				}

				extensions.disable(ext_name);
				
				break;

			case 'warn':

				if (ext_warn) {
					this.state.compile_state.addWarning(ext_warn);
					return;
				}

				//@todo: Need to devise warning method of extension calls

				this.state.compile_state.addWarning("Extension warnings not implemented yet");
				
				extensions.enable(ext_name);

				break;
		}		
	},

	undef : function(line, matches) {

		if (matches.length != 2) {
			throw new Error("Syntax error in #undef");
		}

		delete this.state.defines[matches[1]];
	},
	
	ifdef : function(line, matches) {

		var def;

		def = !!this.state.defines[matches[1]];

		this.state.cond_stack.push(def);		
	},

	endif : function(line, matches) {

		if (this.state.cond_stack.length) {
			this.state.cond_stack.pop();	
		} else {
			throw new Error("unmatched #endif");
		}
	}

};

