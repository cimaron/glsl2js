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

Preprocessor.modules.comments = {

	process : function(src) {
		var i,
		    chr,
		    la,
		    out = "",
		    line = 1,
		    in_single = 0,
		    in_multi = 0
			;
		
		for (i = 0; i < src.length; i++) {

			chr = src.substr(i, 1);
			la = src.substr(i + 1, 1);
			
			//Enter single line comment
			if (chr == '/' && la == '/' && !in_single && !in_multi) {
				in_single = line;
				i++;
				continue;
			}
			
			//Exit single line comment
			if (chr == "\n" && in_single) {
				in_single = 0;
			}
			
			//Enter multi line comment
			if (chr == '/' && la == '*' && !in_multi && !in_single) {
				in_multi = line;
				i++;
				continue;
			}
			
			//Exit multi line comment
			if (chr == '*' && la == '/' && in_multi) {

				//Treat single line multi-comment as space
				if (in_multi == line) {
					out += " ";
				}

				in_multi = 0;
				i++;				
				continue;
			}

			//Newlines are preserved
			if ((!in_multi && !in_single) || chr == "\n") {
				out += 	chr;
				line++;
			}
		}
		
		return out;
	}

};

