var chai = require('chai'),
	glsl = require('../build/glsl.parser.js'),
	expect = chai.expect;

function parseFragment( source ) {
	var parsed =  glsl.parse( source, { target : glsl.target.fragment } );
	if( parsed.errors.length ) {
		throw new Error( parsed.errors[ 0 ] );
	}
	return parsed;
}
function parseVertex( source ) {
	var parsed =  glsl.parse( source, { target : glsl.target.vertex } );
	if( parsed.errors.length ) {
		throw new Error( parsed.errors[ 0 ] );
	}
	return parsed;
}

describe( 'Regression Parser Tests', function() {

	it.only( 'Commas', function() {
		var parsed = parseFragment([
			'void main() {',
				'vec3 a;',
                'a.x = 1, a.y = 2;',
				'gl_FragColor = a-1.0;',
			'}'
		].join('\n') );
		expect( parsed.ast.toString() ).to.contain( 'a.x' );
		expect( parsed.ast.toString() ).to.contain( 'a.y' );
	});

	it( 'Parses a substraction correctly', function() {
		var parsed = parseFragment([
			'void main() {',
				'vec3 a;',
				'gl_FragColor = a-1.0;',
			'}'
		].join('\n') );
		expect( parsed.ast.toString() ).to.contain( 'a - 1.0' );
	});

	it( 'Parses a subtraction correctly', function() {
		var parsed = parseFragment([
			'void main() {',
				'vec3 a;',
				'gl_FragColor = a-1.0;',
			'}'
		].join('\n') );
		expect( parsed.ast.toString() ).to.contain( 'a - 1.0' );
	});

	it( 'Parses #extension without error', function() {
		var parsed = parseFragment([
			'#extension GL_OES_standard_derivatives : enable',
			'void main() {',
				'gl_FragColor = 1.0;',
			'}'
		].join('\n') );
		expect( parsed.directives.extensions.GL_OES_standard_derivatives ).to.contain( 'enable' );
	});

	it( 'Parses for loop', function() {
		var parsed = parseFragment([
			'void main() {',
				'for (float f = 1.0 ; f <= 10.0 ; f++ ) {',
				'	vec2 position;',
				'}',
			'}'
		].join('\n') );
		expect( parsed.ast.toString() ).to.contain( 'for (float f = 1.0; f <= 10.0; f++)' );
		expect( parsed.ast.toString() ).to.contain( 'vec2 position' );
	});

	it( 'Parses for loop with optional ', function() {
		var parsed = parseFragment([
			'void main() {',
				'for (float f = 1.0 ; f <= 10.0 ;) {',
				'	vec2 position;',
				'}',
			'}'
		].join('\n') );
		expect( parsed.ast.toString() ).to.contain( 'for (float f = 1.0; f <= 10.0; )' );
		expect( parsed.ast.toString() ).to.contain( 'vec2 position' );
	});

});

