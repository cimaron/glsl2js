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

/* operator associations and precedence */

%left '+'
%left '*'
%right THEN ELSE

%start glsl-start

%% /* language grammar */

glsl-start :
			translation_unit EOF		{ return $1; }
		;

translation_unit: 
	version_statement extension_statement_list
	external_declaration_list
	;

/* Line: 229 */
version_statement:
	  /* blank - no #version specified: defaults are already set */
	| VERSION INTCONSTANT EOL
	;

/* Line: 270 */
pragma_statement:
	  'PRAGMA_DEBUG_ON' 'EOL'
	| 'PRAGMA_DEBUG_OFF' 'EOL'
	| 'PRAGMA_OPTIMIZE_ON' 'EOL'
	| 'PRAGMA_OPTIMIZE_OFF' 'EOL'
	| 'PRAGMA_INVARIANT_ALL' 'EOL'
	;

/* Line: 287 */
extension_statement_list:

	| extension_statement_list extension_statement
	;

/* Line: 292 */
any_identifier:
	variable_identifier
	| 'TYPE_IDENTIFIER'
	;

/* Line: 298 */
extension_statement:
	  'EXTENSION' any_identifier ':' any_identifier 'EOL'
	;

/* Line: 307 */
external_declaration_list:
	  external_declaration {
			if ($1 !== null) {
				yy.state.addAstNode($1);
			}
		}
	| external_declaration_list external_declaration {
			if ($2 !== null) {
				yy.state.addAstNode($2);
			}
		}
	;

/* Line: 326 */
variable_identifier:
		  'IDENTIFIER'
		| 'NEW_IDENTIFIER'
		;

/* Line: 331 */
primary_expression:
		  variable_identifier {
				$$ = new AstExpression('ident');
				$$.setLocation(@1);
				$$.primary_expression.identifier = $1; }
		| 'INTCONSTANT' {
				$$ = new AstExpression('int');
				$$.setLocation(@1);
				$$.primary_expression.int_constant = $1;
				$$.primary_expression.type = 'int'; }
		| 'FLOATCONSTANT' {
				$$ = new AstExpression('float');
				$$.setLocation(@1);
				$$.primary_expression.float_constant = $1;
				$$.primary_expression.type = 'float'; }
		| 'BOOLCONSTANT' {
				$$ = new AstExpression('bool');
				$$.setLocation(@1);
				$$.primary_expression.bool_constant = $1;
				$$.primary_expression.type = 'bool'; }
		| '(' expression ')' {
				$$ = $2;
				$$.grouped = true;
		}
		;

/* Line: 373 */
postfix_expression:
		  primary_expression
		| postfix_expression '[' integer_expression ']' {
				$$ = new AstExpression('[]', $1, $3);
				$$.setLocation(@1); }
		}
		| function_call
		| postfix_expression '.' any_identifier {
				$$ = new AstExpression('.', $1);
				$$.setLocation(@1);
				$$.primary_expression.identifier = $3; }
		| postfix_expression '++' {
				$$ = new AstExpression('x++', $1);
				$$.setLocation(@1); }
		| postfix_expression '--' {
				$$ = new AstExpression('x--', $1);
				$$.setLocation(@1); }
		;

/* Line: 406 */
integer_expression:
			expression
		;

/* Line: 410 */
function_call:
			function_call_or_method
		;

/* Line: 414 */
function_call_or_method:
			function_call_generic
		|	postfix_expression '.' method_call_generic
		;

/* Line: 424 */
function_call_generic:
			function_call_header_with_parameters ')'
		|	function_call_header_no_parameters ')'
		;

/* Line: 429 */
function_call_header_no_parameters:
			function_call_header 'VOID'
		|	function_call_header
		;

/* Line: 434 */
function_call_header_with_parameters:
		  function_call_header assignment_expression {
				$$ = $1;
				$$.setLocation(@1);
				$$.expressions.push($2); }	
		|  function_call_header_with_parameters ',' assignment_expression {
				$$ = $1;
				$$.setLocation(@1);
				$$.expressions.push($3); }	
		;

/* Line: 452 */
/* Fix conflict */
function_call_header:
		  type_specifier '(' {
				$$ = new AstFunctionExpression($1);
				$$.setLocation(@1);
			}
		| variable_identifier '(' {
				var callee = new AstExpression($1);
				$$ = new AstFunctionExpression(callee);
				$$.setLocation(@1); }
		| 'FIELD_SELECTION'
		;

/* Line: 456 */
/*
function_identifier:
	  type_specifier
	| variable_identifier
	| 'FIELD_SELECTION'
	;
*/

/* Line: 478 */
method_call_generic:
	  method_call_header_with_parameters ')'
	| method_call_header_no_parameters ')'
	;

/* Line: 489 */
method_call_header_no_parameters:
	  method_call_header 'VOID'
	| method_call_header
	;

/* Line: 489 */
method_call_header_with_parameters:
		method_call_header assignment_expression
	| method_call_header_with_parameters ',' assignment_expression
	;

/* Line: 507 */
method_call_header:
		variable_identifier '('
	;

/* Grammar Note: Constructors look like functions, but lexical analysis recognized most of them as
   keywords. They are now recognized through "type_specifier".
*/

	/* Grammar Note: No traditional style type casts. */
/* Line: 518 */
unary_expression:
		  postfix_expression
		| '++' unary_expression {
				$$ = new AstExpression('++x', $2);
				$$.setLocation(@1);
			}
		| '--' unary_expression {
				$$ = new AstExpression('--x', $2);
				$$.setLocation(@1);
			}
		| unary_operator unary_expression {
				$$ = new AstExpression($1, $2);
				$$.setLocation(@1);
			}
		;

	/* Grammar Note: No '*' or '&' unary ops. Pointers are not supported. */
/* Line: 570 */
unary_operator:
		  '+' {
				$$ = 'POS'; }
		| '-' {
				$$ = 'NEG'; }
		| '!'
		| '~'
		;

/* Line: 548 */
multiplicative_expression:
		  unary_expression
		| multiplicative_expression '*' unary_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		| multiplicative_expression '/' unary_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		| multiplicative_expression '%' unary_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		;

/* Line: 570 */
additive_expression:
		  multiplicative_expression
		| additive_expression '+' multiplicative_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		| additive_expression '-' multiplicative_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		;

/* Line: 586 */
shift_expression:
		  additive_expression
		| shift_expression '<<' additive_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		| shift_expression '>>' additive_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		;

/* Line: 602 */
relational_expression:
		  shift_expression
		| relational_expression '<' shift_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		| relational_expression '>' shift_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		| relational_expression '<=' shift_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		| relational_expression '>=' shift_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		;

/* Line: 630 */
equality_expression:
		  relational_expression
		| equality_expression '==' relational_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		| equality_expression '!=' relational_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		;

/* Line: 646 */
and_expression:
		  equality_expression
		| and_expression '&' equality_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		;

/* Line: 656 */
exclusive_or_expression:
		  and_expression
		| exclusive_or_expression '^' and_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		;

/* Line: 666 */
inclusive_or_expression:
		  exclusive_or_expression
		| inclusive_or_expression '|' exclusive_or_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		;

/* Line: 676 */
logical_and_expression:
		  inclusive_or_expression
		| logical_and_expression '&&' inclusive_or_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		;

/* Line: 686 */
logical_xor_expression:
		  logical_and_expression
		| logical_xor_expression '^^' logical_and_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		;

/* Line: 696 */
logical_or_expression:
		  logical_xor_expression
		| logical_or_expression '||' logical_xor_expression {
				$$ = new AstExpressionBin($2, $1, $3);
				$$.setLocation(@1);
			}
		;

/* Line: 706 */
conditional_expression:
		  logical_or_expression
		| logical_or_expression '?' expression ':' assignment_expression {
				$$ = new AstExpression($2, $1, $3, $5);
				$$.setLocation(@1);
			}
		;

/* Line: 716 */
assignment_expression:
		  conditional_expression
		| unary_expression assignment_operator assignment_expression {
				$$ = new AstExpression($2, $1, $3);
				$$.setLocation(@1);
			}
		;

/* Line: 726 */
assignment_operator:
		  '='
		| '*='
		| '/='
		| '%='
		| '+='
		| '-='
		| '<<='
		| '>>='
		| '&='
		| '^='
		| '|='
		;

/* Line: 740 */
expression:
		  assignment_expression {
				$$ = $1;
			}
		| expression ',' assignment_expression {
				if ($1.oper !== $2) {
					$$ = new AstExpression($2);
					$$.setLocation(@1);
					$$.expressions.push($1);
				} else {
					$$ = $1;
				}
				$$.expressions.push($3);
			}
		;

/* Line: 760 */
constant_expression:
		  conditional_expression
		;

/* Line: 764 */
declaration:
		  function_prototype ';' {
				yy.state.symbols.pop_scope();
				$$ = $1;
			}
		| init_declarator_list ';' {
				$$ = $1;
			}
		| 'PRECISION' precision_qualifier type_specifier_no_prec ';' {
				$3.precision = $2;
				$3.is_precision_statement = true;
				$$ = $3; }
		;

/* Line: 782 */
function_prototype:
		  function_declarator ')'
		;

/* Line: 786 */
function_declarator:
			function_header
		|	function_header_with_parameters
		;

/* Line: 791 */
function_header_with_parameters:
		  function_header parameter_declaration {
			  	$$ = $1;
				$$.parameters.push($2);
			}
		| function_header_with_parameters ',' parameter_declaration {
			  	$$ = $1;
				$$.parameters.push($3);
        }
		;

/* Line: 804 */
function_header:
		  fully_specified_type variable_identifier '(' {
				$$ = new AstFunction();
				$$.setLocation(@1);
				$$.return_type = $1;
				$$.identifier = $2;
				
				//Check for duplicates
				if ($2 == 'main') {
					if (yy.state.symbols.get_function($2)) {
						var e = new Error("Cannot define main() more than once");
						e.lineNumber = @1.first_line;
						e.columnNumber = @1.first_column;
						throw e;
					}
				}

				$$.entry = yy.state.symbols.add_function($2, $1.specifier.type_name);
				$$.entry.Ast = $$;
				yy.state.symbols.push_scope();
			}
		;

/* Line: 818 */
parameter_declarator:
		  type_specifier any_identifier {
				$$ = new AstParameterDeclarator();
				$$.setLocation(@1);
				$$.type = new AstFullySpecifiedType();
				$$.type.setLocation(@1);
				$$.type.specifier = $1;
				$$.identifier = $2; }
		| type_specifier any_identifier '[' constant_expression ']'
		;

/* Line: 843 */
parameter_declaration:
		  parameter_type_qualifier parameter_qualifier parameter_declarator {
				$1.concat($2);
				$$ = $3;
				$$.type.qualifier = $1; }
		| parameter_qualifier parameter_declarator {
				$$ = $2;
				$$.type.qualifier = $1; }
		| parameter_type_qualifier parameter_qualifier parameter_type_specifier {
				$1.concat($2);
				$$ = new AstParameterDeclarator();
				$$.setLocation(@1);
				$$.type = new AstFullySpecifiedType();
				$$.type.qualifier = $1;
				$$.type.specifier = $3; }
		| parameter_qualifier parameter_type_specifier {
				$$ = new AstParameterDeclarator();
				$$.setLocation(@1);
				$$.type = new AstFullySpecifiedType();
				$$.type.qualifier = $1;
				$$.type.specifier = $2; }
		;

/* Line: 878 */
parameter_qualifier:
		  /* empty */ {
			  $$ = []; }
		| 'IN' {
			$$ = ['in']; }
		| 'OUT' {
			$$ = ['out']; }
		| 'INOUT' {
			$$ = ['inout']; }
		;

/* Line: 901 */
parameter_type_specifier:
			type_specifier
		;

/* Line: 905 */
init_declarator_list:
		  single_declaration
		| init_declarator_list ',' any_identifier {
			var decl = new AstDeclaration($3, false);
			decl.setLocation(@1);
			$$ = $1;
			$$.declarations.push(decl);
			/*yy.state.symbols.add_variable($3);*/ }
		| init_declarator_list ',' any_identifier '[' ']'
		| init_declarator_list ',' any_identifier '[' constant_expression ']' {
			var decl = new AstDeclaration($3, true, $5);
			decl.setLocation(@1);
			$$ = $1;
			$$.declarations.push(decl);
			/*yy.state.symbols.add_variable($3);*/ }
		| init_declarator_list ',' any_identifier '[' ']' '=' initializer
		| init_declarator_list ',' any_identifier '[' constant_expression ']' '=' initializer
		| init_declarator_list ',' any_identifier '=' initializer {
			var decl = new AstDeclaration($3, false, null, $5);
			decl.setLocation(@1);
			$$ = $1;
			$$.declarations.push(decl);
			/*yy.state.symbols.add_variable($3);*/ }
		;

/* Grammar Note: No 'enum', or 'typedef'. */
/* Line: 969 */
single_declaration:
		  fully_specified_type {
				if ($1.specifier.type_specifier !== types.struct) {
					yy.state.addError("empty declaration list", @1.first_line, @1.first_column);
					return 0;
				}

				$$ = new AstDeclaratorList($1);
				$$.setLocation(@1); }
		| fully_specified_type any_identifier {
				var decl = new AstDeclaration($2, false);
				decl.setLocation(@2);
				$$ = new AstDeclaratorList($1);
				$$.setLocation(@1);
				$$.declarations.push(decl); }
		| fully_specified_type any_identifier '[' ']' {
				var decl = new AstDeclaration($2, true);
				decl.setLocation(@2);
				$$ = new AstDeclaratorList($1);
				$$.setLocation(@1);
				$$.declarations.push(decl); }
		| fully_specified_type any_identifier '[' constant_expression ']' {
				var decl = new AstDeclaration($2, true, $4);
				decl.setLocation(@2);
				$$ = new AstDeclaratorList($1);
				$$.setLocation(@1);
				$$.declarations.push(decl); }
		| fully_specified_type any_identifier '[' ']' '=' initializer {
				var decl = new AstDeclaration($2, true, null, $6);
				decl.setLocation(@2);
				$$ = new AstDeclaratorList($1);
				$$.setLocation(@1);
				$$.declarations.push(decl); }
		| fully_specified_type any_identifier '[' constant_expression ']' '=' initializer {
				var decl = new AstDeclaration($2, true, $4, $7);
				decl.setLocation(@2);
				$$ = new AstDeclaratorList($1);
				$$.setLocation(@1);
				$$.declarations.push(decl); }
		| fully_specified_type any_identifier '=' initializer {
				var decl = new AstDeclaration($2, false, null, $4);
				decl.setLocation(@2);
				$$ = new AstDeclaratorList($1);
				$$.setLocation(@1);
				$$.declarations.push(decl); }
		| 'INVARIANT' variable_identifier
		;

/* Line: 1049 */
fully_specified_type:
		  type_specifier {
				$$ = new AstFullySpecifiedType();
				$$.setLocation(@1);
				$$.specifier = $1; }
		| type_qualifier type_specifier {
				$$ = new AstFullySpecifiedType();
				$$.setLocation(@1);
				$$.qualifier = $1;
				$$.specifier = $2; }
		;

/* Line: 1067 */
layout_qualifier:
		  'LAYOUT' '(' layout_qualifier_id_list ')' {
				$$ = $3; }
		;

/* Line: 1074 */
layout_qualifier_id_list:
			layout_qualifier_id
		|	layout_qualifier_id_list ',' layout_qualifier_id
		;

/* Line: 1094 */
layout_qualifier_id:
			any_identifier
		|	any_identifier '=' 'INTCONSTANT'
		;

/* Line: 1164 */
interpolation_qualifier:
			'SMOOTH'
		|	'FLAT'
		|	'NOPERSPECTIVE'
		;

/* Line: 1182 */
parameter_type_qualifier:
		  'CONST' {
				$$ = ['const']; }
		;

/* Line: 1190 */
type_qualifier:
			storage_qualifier
		|	layout_qualifier
		|	layout_qualifier storage_qualifier
		|	interpolation_qualifier
		|	interpolation_qualifier storage_qualifier
		|	'INVARIANT' storage_qualifier
		|	'INVARIANT' interpolation_qualifier storage_qualifier
		|	'INVARIANT'
		;

/* Line: 1222 */
storage_qualifier:
		  'CONST' {
				$$ = ['const']; }
		| 'ATTRIBUTE' /* Vertex only. */ {
				$$ = ['attribute']; }
		| 'VARYING' {
				$$ = ['varying']; }
		| 'CENTROID' 'VARYING' {
				$$ = ['centroid', 'varying']; }
		| 'IN' {
				$$ = ['in']; }
		| 'OUT' {
				$$ = ['out']; }
		| 'CENTROID' 'IN' {
				$$ = ['centroid', 'in']; }
		| 'CENTROID' 'OUT' {
				$$ = ['centroid', 'out']; }
		| 'UNIFORM' {
				$$ = ['uniform']; }
		;

/* Line: 1271 */
type_specifier:
		  type_specifier_no_prec {
				$$ = $1;  
			}
		| precision_qualifier type_specifier_no_prec {
				$$ = $2;
				$$.precision = $1;
			}
		;

/* Line: 1283 */
type_specifier_no_prec:
			type_specifier_nonarray
		|	type_specifier_nonarray '[' ']'
		|	type_specifier_nonarray '[' constant_expression ']'
		;

/* Line: 1299 */
type_specifier_nonarray:
	  basic_type_specifier_nonarray {
		  	$$ = new AstTypeSpecifier($1);
			$$.setLocation(@1);
		}
	| struct_specifier {
		  	$$ = new AstTypeSpecifier($1);
			$$.setLocation(@1);
		}
	| 'TYPE_IDENTIFIER' {
		  	$$ = new AstTypeSpecifier($1);
			$$.setLocation(@1);
		}
	;


/* Line: 1374 */
basic_type_specifier_nonarray:
			'VOID'
		|	'FLOAT'
		|	'DOUBLE'
		|	'INT'
		|	'BOOL'
		|	'VEC2'
		|	'VEC3'
		|	'VEC4'
		|	'BVEC2'
		|	'BVEC3'
		|	'BVEC4'
		|	'IVEC2'
		|	'IVEC3'
		|	'IVEC4'
		|	'MAT2X2'
		|	'MAT3X3'
		|	'MAT4X4'
		|	'SAMPLER1D'
		|	'SAMPLER2D'
		|	'SAMPLER3D'
		|	'SAMPLERCUBE'
		|	'SAMPLER1DSHADOW'
		|	'SAMPLER2DSHADOW'
		;

/* Line: 1374 */
precision_qualifier:
		  'HIGHP' {
				$$ = ast_precision.highp; }
		| 'MEDIUMP' {
				$$ = ast_precision.mediump; }
		| 'LOWP' {
				$$ = ast_precision.lowp; }
		;

/* Line: 1407 */
struct_specifier:
		  'STRUCT' any_identifier '{' struct_declaration_list '}' {
				$$ = new AstStructSpecifier($2, $4);
				$$.setLocation(@1);
				yy.state.symbols.add_type($2, types._void);
			}			  
		| 'STRUCT' '{' struct_declaration_list '}'
		;

/* Line: 1423 */
struct_declaration_list:
		  struct_declaration {
				$$ = new AstDeclaratorList();
				$$.declarations = [$1];
			}
		| struct_declaration_list struct_declaration {
				$$ = $1;
				$$.declarations.push( $2 );
			}
		;

/* Line: 1436 */
struct_declaration:
		  type_specifier struct_declarator_list ';' {
				var type = new AstFullySpecifiedType();
				type.setLocation(@1);
				type.specifier = $1;
				$$ = new AstDeclaratorList(type);
				$$.setLocation(@1);
				$$.declarations = [$2]; }
		;

/* Line: 1451 */
struct_declarator_list:
			struct_declarator
		|	struct_declarator_list ',' struct_declarator
		;

/* Line: 1464 */
struct_declarator:
		  any_identifier {
				$$ = new AstDeclaration($1, false);
				$$.setLocation(@1);
				yy.state.symbols.add_variable($1);
			}
		| any_identifier '[' constant_expression ']'
		;

/* Line: 1480 */
initializer:
			assignment_expression
		;

/* Line: 1484 */
declaration_statement:
			declaration
		;

/* Line: 1490 */
statement:
			compound_statement
		|	simple_statement
		;

/* Grammar Note: labeled statements for SWITCH only; 'goto' is not supported. */

/* Line: 1495 */
simple_statement:
		  declaration_statement
		| expression_statement
		| selection_statement
		| switch_statement {
				$$ = null; }
		| case_label {
				$$ = null; }
		| iteration_statement
		| jump_statement
		;

/* Line: 1505 */
compound_statement:
		  '{' '}' {
				$$ = new AstCompoundStatement(true);
				$$.setLocation(@1); }
		| '{' statement_list '}' {
			  	yy.state.symbols.push_scope();
				$$ = new AstCompoundStatement(true, $2);
				$$.setLocation(@1);
				yy.state.symbols.pop_scope(); }
		;

/* Line: 1525 */
statement_no_new_scope:
			compound_statement_no_new_scope
		|	simple_statement
		;

/* Line: 1530 */
compound_statement_no_new_scope:
		  '{' '}' {
				$$ = new AstCompoundStatement(false);
				$$.setLocation(@1); }
		| '{' statement_list '}' {
				$$ = new AstCompoundStatement(false, $2);
				$$.setLocation(@1); }
		;

/* Line: 1545 */
statement_list:
		  statement {
				if ($1 === null) {
					yy.state.addError("<nil> statement", @1.first_line, @1.first_column);
				} else {
					$$ = [$1];
				}
			}
		| statement_list statement {
				if ($2 === null) {
					yy.state.addError("<nil> statement", @1.first_line, @1.first_column);
				}
				$$ = $1;
				$$.push($2);
			}
		;

/* Line: 1567 */
expression_statement:
		  ';' {
			  	$$ = new AstExpressionStatement(null);
				$$.setLocation(@1); }
		| expression ';' {
				$$ = new AstExpressionStatement($1);
				$$.setLocation(@1); }
		;

/* Line: 1582 */
selection_statement:
		  'IF' '(' expression ')' selection_rest_statement {
				$$ = new AstSelectionStatement($3, $5.then_statement, $5.else_statement);
				$$.setLocation(@1); }
		;

/* Line: 1591 */
selection_rest_statement:
		  statement 'ELSE' statement {
		  		$$ = {};
				$$.then_statement = $1;
				$$.else_statement = $3; }
		| statement %prec THEN  {
				$$.then_statement = $1; }
		;

/* Line: 1604 */
condition:
			expression
		|	fully_specified_type any_identifier '=' initializer {
				var decl = new AstDeclaration($2, false, null, $4);
				$$ = new AstDeclaratorList($1, true);
				$$.setLocation(@1);
				$$.declarations.push(decl); }
		;

/* Line: 1622 */
switch_statement:
			'SWITCH' '(' expression ')' compound_statement
		;

/* Line: 1626 */
case_label:
			'CASE' expression ':'
		|	'DEFAULT' ':'
		;

/* Line: 1631 */
iteration_statement:
			'WHILE' '(' condition ')' statement_no_new_scope
		|	'DO' statement 'WHILE' '(' expression ')' ';'
		|	'FOR' '(' for_init_statement for_rest_statement ')' statement_no_new_scope {
				$$ = new AstIterationStatement('for', $3, $4.cond, $4.rest, $6);
				$$.setLocation(@1);
			}
		;

/* Line: 1655 */
for_init_statement:
			expression_statement {
				$$ = $1;
				$$.inline = true; }
		|	declaration_statement {
				$$ = $1;
				$$.inline = true; }
		;

/* Line: 1660 */
conditionopt:
			condition
		|	/* empty */
		;

/* Line: 1668 */
for_rest_statement:
			conditionopt ';' {
				$$ = {
					cond : $1,
					rest : null
				}; }
		|	conditionopt ';' expression {
				$$ = {
					cond : $1,
					rest : $3
				}; }
		;

/* Line: 1682 */
jump_statement:
		'CONTINUE' ';' {
			$$ = new AstJumpStatement('continue');
			$$.setLocation(@1); }
		| 'BREAK' ';' {
			$$ = new AstJumpStatement('break');
			$$.setLocation(@1); }
		| 'RETURN' ';' {
			$$ = new AstJumpStatement('return');
			$$.setLocation(@1); }
		| 'RETURN' expression ';' {
			$$ = new AstJumpStatement('return', $2);
			$$.setLocation(@1); }			
		| 'DISCARD' ';' { /* Fragment shader only.*/
			$$ = new AstJumpStatement('discard');
			$$.setLocation(@1); }
		;

/* Line: 1715 */
external_declaration:
		  function_definition {
				$$ = $1; }
		| declaration {
				$$ = $1; }
		| pragma_statement {
				$$ = null; }
		;

/* Line: 1721 */
function_definition:
		  function_prototype compound_statement_no_new_scope {
				$$ = new AstFunctionDefinition();
				$$.setLocation(@1);
				$$.proto_type = $1;
				$$.body = $2;
				yy.state.symbols.pop_scope(); }
		;

