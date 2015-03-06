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

%{
%}

/* lexical grammar */
%lex

DEC_INT		[1-9][0-9]*
HEX_INT		0[xX][0-9a-fA-F]+
OCT_INT		0[0-7]*
INT		({DEC_INT}|{HEX_INT}|{OCT_INT})
SPC		[ \t]*
SPCP		[ \t]+
HASH		^{SPC}#{SPC}

%s PRAGMA PP

%%

[ \r\t]+		;

/* Preprocessor tokens. */ 
[ \t]*\#[ \t]*$             {}
[ \t]*\#[ \t]*"version"     { this.begin('PP'); return 'VERSION'; }
[ \t]*\#[ \t]*"extension"   { this.begin('PP'); return 'EXTENSION'; }

{HASH}"line"{SPCP}{INT}{SPCP}{INT}{SPC}$ {
	
	/* Eat characters until the first digit is
	 * encountered
	 */
	
	var ptr = 0;
	while (yytext.slice(0, 1) < '0' || yytext.slice(0, 1) > '9') {
		ptr++;
	}

	/* Subtract one from the line number because
	 * yylineno is zero-based instead of
	 * one-based.
	 */
	yylineno = parseInt(yytext.slice(0, 1), 10) - 1;
	yylloc.source = parseInt(yytext.slice(0), 10);
}

{HASH}"line"{SPCP}{INT}{SPC}$	{
				   /* Eat characters until the first digit is
				    * encountered
				    */
					var ptr = 0;
					while (yytext.slice(0, 1) < '0' || yytext.slice(0, 1) > '9')
						ptr++;

				   /* Subtract one from the line number because
				    * yylineno is zero-based instead of
				    * one-based.
				    */
				   yylineno = parseInt(yytext.slice(0, 1), 10) - 1;
				}
{SPC}\#{SPC}"pragma"{SPCP}"debug"{SPC}\({SPC}"on"{SPC}\) {
				  this.begin('PP');
				  return 'PRAGMA_DEBUG_ON';
				}
{SPC}\#{SPC}"pragma"{SPCP}"debug"{SPC}\({SPC}"off"{SPC}\) {
				  this.begin('PP');
				  return 'PRAGMA_DEBUG_OFF';
				}
{SPC}\#{SPC}"pragma"{SPCP}"optimize"{SPC}\({SPC}"on"{SPC}\) {
				  this.begin('PP');
				  return 'PRAGMA_OPTIMIZE_ON';
				}
{SPC}\#{SPC}"pragma"{SPCP}"optimize"{SPC}\({SPC}"off"{SPC}\) {
				  this.begin('PP');
				  return 'PRAGMA_OPTIMIZE_OFF';
				}
{SPC}\#{SPC}"pragma"{SPCP}"STDGL"{SPCP}"invariant"{SPC}\({SPC}"all"{SPC}\) {
				  this.begin('PP');
				  return 'PRAGMA_INVARIANT_ALL';
				}
{SPC}\#{SPC}"pragma"{SPCP}	{ this.begin('PRAGMA'); }

<PRAGMA>[\n]			{ this.begin('INITIAL'); yylineno++; yycolumn = 0; }
<PRAGMA>.			{ }

<PP>\/\/[^\n]*			{ }
<PP>[ \t\r]*			{ }
<PP>":"				return ":";
<PP>[_a-zA-Z][_a-zA-Z0-9]*	{
				   yylval.identifier = strdup(yytext);
				   return 'IDENTIFIER';
				}
<PP>[1-9][0-9]*			{
				    yylval.n = parseInt(yytext);
				    return 'INTCONSTANT';
				}
<PP>[\n]				{ this.begin('INITIAL'); yylineno++; yycolumn = 0; return 'EOL'; }

[\n]		{ /*yylineno++; yycolumn = 0;*/ }

"attribute"	return 'ATTRIBUTE';
"const"		return 'CONST';
"bool"		return 'BOOL';
"float"		return 'FLOAT';
"int"		return 'INT';

"break"		return 'BREAK';
"continue"	return 'CONTINUE';
"do"		return 'DO';
"while"		return 'WHILE';
"else"		return 'ELSE';
"for"		return 'FOR';
"if"		return 'IF';
"discard"		return 'DISCARD';
"return"		return 'RETURN';

"bvec2"		return 'BVEC2';
"bvec3"		return 'BVEC3';
"bvec4"		return 'BVEC4';
"ivec2"		return 'IVEC2';
"ivec3"		return 'IVEC3';
"ivec4"		return 'IVEC4';
"vec2"		return 'VEC2';
"vec3"		return 'VEC3';
"vec4"		return 'VEC4';
"mat2"		return 'MAT2X2';
"mat3"		return 'MAT3X3';
"mat4"		return 'MAT4X4';

"in"              return 'IN';
"out"             return 'OUT';
"inout"           return 'INOUT';
"uniform"	      return 'UNIFORM';
"varying"         return 'VARYING';
"invariant"       return 'INVARIANT';
"flat"            return 'FLAT';
"smooth"          return 'SMOOTH';

"sampler1D"	return 'SAMPLER1D';
"sampler2D"	return 'SAMPLER2D';
"sampler3D"	return 'SAMPLER3D';
"samplerCube"	return 'SAMPLERCUBE';
"sampler1DShadow"	return 'SAMPLER1DSHADOW';
"sampler2DShadow"	return 'SAMPLER2DSHADOW';


"struct"		return 'STRUCT';
"void"		return 'VOID';

"layout"		{/*copy manually*/}

"++"		return '++';
"--"		return '--';
"<="		return '<=';
">="		return '>=';
"=="		return '==';
"!="		return '!=';
"&&"		return '&&';
"||"		return '||';
"^^"		return '^^';
"<<"		return '<<';
">>"		return '>>';

"*="		return '*=';
"/="		return '/=';
"+="		return '+=';
"%="		return '%=';
"<<="		return '<<=';
">>="		return '>>=';
"&="		return '&=';
"^="		return '^=';
"|="		return '|=';
"-="		return '-=';

[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?[fF]?	{
			    this.yylval = parseFloat(yytext);
			    return 'FLOATCONSTANT';
			}
\.[0-9]+([eE][+-]?[0-9]+)?[fF]?		{
				this.yylval = parseFloat(yytext);
				return 'FLOATCONSTANT';
			}
[0-9]+\.([eE][+-]?[0-9]+)?[fF]?		{
			    this.yylval = parseFloat(yytext);
			    return 'FLOATCONSTANT';
			}
[0-9]+[eE][+-]?[0-9]+[fF]?		{
			    this.yylval = parseFloat(yytext);
			    return 'FLOATCONSTANT';
			}
[0-9]+[fF]		{
			    this.yylval = parseFloat(yytext);
			    return 'FLOATCONSTANT';
			}
0[xX][0-9a-fA-F]+	{
			    this.yylval = parseInt(yytext + 2, 16);
			    return 'INTCONSTANT';
			}
0[0-7]*		{
			    this.yylval = parseInt(yytext, 8);
			    return 'INTCONSTANT';
			}
[1-9][0-9]*	{
				this.yylval = parseInt(yytext);
				return 'INTCONSTANT';
			}
"true"			{
			    this.yylval = 1;
			    return 'BOOLCONSTANT';
			}
"false"			{
			    this.yylval = 0;
			    return 'BOOLCONSTANT';
			}


"asm"		return 'ASM'
"class"		return 'CLASS'
"union"		return 'UNION'
"enum"		return 'ENUM'
"typedef"		return 'TYPEDEF'
"template"	return 'TEMPLATE'
"this"		return 'THIS'
"packed"		return 'PACKED'
"goto"		return 'GOTO'
"switch"		return 'SWITCH'
"default"		return 'DEFAULT'
"inline"		return 'INLINE'
"noinline"	return 'NOINLINE'
"volatile"	return 'VOLATILE'
"public"		return 'PUBLIC'
"static"		return 'STATIC'
"extern"		return 'EXTERN'
"external"	return 'EXTERNAL'
"interface"	return 'INTERFACE'
"long"		return 'LONG'
"short"		return 'SHORT'
"double"		return 'DOUBLE'
"half"		return 'HALF'
"fixed"		return 'FIXED'
"unsigned"	return 'UNSIGNED'
"input"		return 'INPUT'
"output"		return 'OUTPUT'
"hvec2"		return 'HVEC2'
"hvec3"		return 'HVEC3'
"hvec4"		return 'HVEC4'
"dvec2"		return 'DVEC2'
"dvec3"		return 'DVEC3'
"dvec4"		return 'DVEC4'
"fvec2"		return 'FVEC2'
"fvec3"		return 'FVEC3'
"fvec4"		return 'FVEC4'

"sampler2DRect"         return 'SAMPLER2DRECT';
"sampler3DRect"         return 'SAMPLER3DRECT';
"sampler2DRectShadow"   return 'SAMPLER2DRECTSHADOW';
"sizeof"                return 'SIZEOF';
"cast"                  return 'CAST';
"namespace"             return 'NAMESPACE';
"using"                 return 'USING';

"lowp"                  return 'LOWP';
"mediump"               return 'MEDIUMP';
"highp"                 return 'HIGHP';
"precision"             return 'PRECISION';

[_a-zA-Z][_a-zA-Z0-9]* {
	yy.yylval = yytext;
	return yy.state.classify_identifier(yy.state, yytext);
}

.         return yytext;

<<EOF>>   return 'EOF';


