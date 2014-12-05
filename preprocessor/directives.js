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
		defines : {
		}
	},

	process : function(src) {
		var i, l;
		
		this.state.lines = src.split("\n");
		this.state.defines = {
			GL_ES : '1',
			__FILE__ : '0',
			__LINE__ : '0',
			__VERSION__ : '300'
		};

		i = 0;
		l = this.state.lines.length;

		while (i < l) {
			this.state.lines[i] = this.processLine(this.state.lines[i], i);
			i++;
		}

		return this.state.lines.join("\n");
	},
	
	processLine : function(line, i) {
		var d, matches, raw, e, sub;

		matches = line.match(/^([ \t]*)#(.*)$/);
		if (!matches) {

			line = this.processDefines(line, i);

			return line;
		}

		raw = matches[2];

		if (raw.match(/^\s*$/)) {
			return "";
		}

		lmatches = raw.split(/\s+/);
		console.log(line, lmatches);	
		
		try {

			if (lmatches[0] === 'define') {
				this.define(line, lmatches);
				return "";
			}
			
			if (lmatches[0] === 'undef') {
				this.undef(line, lmatches);
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

	undef : function(line, matches) {

		if (matches.length != 2) {
			throw new Error("Syntax error in #undef");
		}
console.log("undef", matches);
		delete this.state.defines[matches[1]];
	}

};

