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

});

