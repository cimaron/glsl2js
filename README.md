glsl2js
=======
## Usage
````js
var objectCode = glsl.compile(src, {target : glsl.target.fragment});
var error = glsl.getLastError();
if (error.length) {
	console.log(error);
} else {
	var prgm = new glsl.program();
	prgm.addObjectCode(objectCode, glsl.target.fragment);
	prgm.build();
	
  prgm.setUniformData('someVec4Uniform', [1, 2, 3, 4]);
  progm.setAttributeData('someVec4Attr', [1, 2, 3, 4])
	
	prgm.fragment();
	
  console.log(prgm.context.result_f32.slice(0, 4));
}
````

## License
The MIT License (http://opensource.org/licenses/MIT)

