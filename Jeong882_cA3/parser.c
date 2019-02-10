/*
* File Name		: parser.h
* Compiler		: MS Visual Studio 2015
* Author		: Seongyeop Jeong, ID# 040885882
* Course		: CST8152 - Compiler, Lab Section: 012
* Assignment	: 3
* Date			: 6 December 2018
* Professor		: Svillen Ranev
* Purpose		: to implement the parser (RDPP) followed by first set
* function list	: parser(), match(), syn_eh(), syn_printe(), gen_incode(), program(), opt_statements(),
statements(), statement(), statements_prime(), assignment_statement(), assignment_expression(), input_statement(), 
variable_list(), variable_identifier(), variable_list_prime(), output_statement(), opt_variable_list(), arithmetic_expression(),
unary_arithmetic_expression(), additive_arithmetic_expression(), additive_arithmetic_expression_prime(), 
multiplicative_arithmetic_expression(), multiplicative_arithmetic_expression_prime(), primary_arithmetic_expression(),
string_expression(), string_expression_prime(), primary_string_expression(), iteration_statement(), selection_statement(),
pre_condition(), conditional_expression(), logical_or_expression(), logical_or_expression_prime(), 
logical_and_expression(), logical_and_expression_prime(), relational_expression(), primary_a_relational_expression(), 
primary_s_relational_expression(), primary_a_relational_expression_prime(), primary_s_relational_expression_prime
*/
#include <stdlib.h>
#include "parser.h"

/**************************************************************************************
Purpose			:	
Author			:	Sv. Ranev
History/Version	:	1.0
Called Function	:	malar_next_token(), program(), match(), gen_incode()
Parameters		:	void
Return value	:	void
Algorithm		:	
***************************************************************************************/
void parser(void) {
	lookahead = malar_next_token();
	program();
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}
/**************************************************************************************
Purpose			:	it is to match two tokens (current input / required by the parser)
Author			:	Seongyeop Jeong
History/Version	:	1.0
Called Function	:	syn_eh(), malar_next_token(), syn_printe()
Parameters		:	int pr_token_code, int pr_token_attribute
Return value	:	void
Algorithm		:
***************************************************************************************/
void match(int pr_token_code, int pr_token_attribute) {
	/* If the match is unsuccessful, the function calls the error handler */
	if (lookahead.code != pr_token_code) {
		syn_eh(pr_token_code);
		return;
	}

	/* If the match is successful and the lookahead is SEOF_T */
	if (lookahead.code == SEOF_T)
		return;

	/* to parse the token code */
	switch (pr_token_code) {
	case KW_T:
	case LOG_OP_T:
	case ART_OP_T:
	case REL_OP_T:
		/* if the required token doesn't match the token attribute */
		if (pr_token_attribute != lookahead.attribute.get_int) {
			syn_eh(pr_token_code);
			return;
		}
		break;
	}

	/* If the match is successful and the lookahead is not SEOF_T, 
	the function advances to the next input token by executing the statement:
	lookahead = malar_next_token (); */
	lookahead = malar_next_token();
	/* If the new lookahead token is ERR_T*/
	if (lookahead.code == ERR_T) {
		syn_printe();
		lookahead = malar_next_token();
		synerrno++;
		return;
	}
}
/**************************************************************************************
Purpose			:	This function implements advancing to the next input token
Author			:	Seongyeop Jeong
History/Version	:	1.0
Called Function	:	syn_printe(), exit(), malar_next_token(),
Parameters		:	int sync_token_code
Return value	:	void
Algorithm		:
***************************************************************************************/
void syn_eh(int sync_token_code)
{
	/* the function calls syn_printe() 
	and increments the error counter. */
	syn_printe();
	synerrno++;

	/* Then the function implements a panic mode error recovery: */
	do {
		/* the function advances the input token */
		lookahead = malar_next_token();

		/* If the function looks for sync_token_code different from SEOF_T 
		and reaches the end of the source file, 
		the function calls exit(synerrno). */
		if (lookahead.code == SEOF_T) {
			if(sync_token_code != SEOF_T)
				exit(synerrno);
			return;
		}

		/* If a matching token is found and the matching token is not SEOF_T, 
		the function advances the input token one more time and returns. */
		if (lookahead.code == sync_token_code) {
			lookahead = malar_next_token();
			return;
		}

	/* until it finds a token code loop itself */
	} while (lookahead.code != sync_token_code);
}
/**************************************************************************************
Purpose			:	it is to call panic mode error recovery to advances the input token
					by advancing to the next input token
Author			:	Seongyeop Jeong
History/Version	:	1.0
Called Function	:	printf(), b_location()
Parameters		:	none
Return value	:	none
Algorithm		:
***************************************************************************************/
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/
 /**************************************************************************************
 Purpose			:	to print out error message 
 Author				:	Seongyeop Jeong
 History/Version	:	1.0
 Called Function	:	printf()
 Parameters			:	char* prints
 Return value		:	void
 Algorithm			:
 ***************************************************************************************/
void gen_incode(char* prints)
{
	printf("%s\n", prints);
}
/**************************************************************************************
Purpose				:	<program> -> PLATYPUS {<opt_statements>}
						 FIRST(<program>) = { KW_T(PLATYPUS) }
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), opt_statements(), gen_incode()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void program(void)
{
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}
/**************************************************************************************
Purpose				:	<opt_statements> -> <statements> | e
						 FIRST(<opt_statements>) = 
						 { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), e }
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	statements(), gen_incode()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void opt_statements(void) 
{ /* FIRST set: {AVID_T,SVID_T,KW_T(but not … see above),e} */
	switch (lookahead.code) 
	{
	case AVID_T:
	case SVID_T:
		statements();
		break;
	case KW_T:
		/* check for PLATYPUS, ELSE, THEN, REPEAT, TRUE, FALSE here and in statements_p()*/
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != REPEAT
			&& lookahead.attribute.get_int != TRUE
			&& lookahead.attribute.get_int != FALSE)
		{
			statements(); 
			break;
		}
	default: 
		/*empty string – optional statements*/; 
		gen_incode("PLATY: Opt_statements parsed");
	}
}
/**************************************************************************************
Purpose				:	<statements> -> <statement>,<statements> | e
						FIRST(<statements>) = 
						{AVID, SVID, KW_T(WHILE), KW_T(REPEAT), KW_T(READ), KW_T(WRITE), e}
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	statement(), statements_prime()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void statements(void)
{
	statement();
	statements_prime();
}
/**************************************************************************************
Purpose				:	<statement> ->
						<assignment statement>
						| <selection statement>
						| <iteration statement>
 						| <input statement>
						| <output statement>
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	assignment_statement(), iteration_statement(), selection_statement()
						input_statement(), output_statement(), syn_printe()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void statement(void) 
{
	switch (lookahead.code) 
	{
	case AVID_T:
	case SVID_T:
		assignment_statement();
		break;
	case KW_T:
		if (lookahead.attribute.get_int == WHILE)
			iteration_statement();
		else if (lookahead.attribute.get_int == IF)
			selection_statement();
		else if (lookahead.attribute.get_int == READ)
			input_statement();
		else if (lookahead.attribute.get_int == WRITE)
			output_statement();
		break;

	default:
		syn_printe();
	}
}
/**************************************************************************************
Purpose				:	<statements'> -> <statement><statements'> | e
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	statement(), statements_prime(), statements() 
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void statements_prime(void) 
{
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		statement();
		statements_prime();
		break;
	case KW_T:
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != REPEAT)
		{
			statements();
			break;
		}
	}
}
/**************************************************************************************
Purpose				:	<assignment statement> -> <assignment expression>
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	assignment_expression(), match(), gen_incode()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void assignment_statement(void) 
{
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}
/**************************************************************************************
Purpose				:	<assignment expression> -> 
								  AVID = <arithmetic expression>
								| SVID = <string expression>
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), arithmetic_expression(), string_expression(), 
						gen_incode(), syn_printe()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void assignment_expression(void) 
{
	/* look for the token */
	switch (lookahead.code) 
	{
	case AVID_T:
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default:
		syn_printe();
	}
}
/**************************************************************************************
Purpose				:	<input statement> ->
							READ (<variable list>);
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), variable_list(), string_expression(),
						gen_incode()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void input_statement(void) 
{
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}
/**************************************************************************************
Purpose				:	<variable list> -> <variable identifier><variable list’>
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	variable_identifier(), variable_list_prime(), gen_incode()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void variable_list(void) 
{
	variable_identifier();
	variable_list_prime();
	gen_incode("PLATY: Variable list parsed");
}
/**************************************************************************************
Purpose				:	<variable list> -> <variable identifier><variable list’>
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), syn_printe()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void variable_identifier(void) 
{
	switch (lookahead.code) 
	{
	case AVID_T:
	case SVID_T:
		match(lookahead.code, NO_ATTR);
		break;

	default:
		syn_printe();
		break;
	}
}
/**************************************************************************************
Purpose				:	<variable list’> -> , <variable identifier> <variable list’> | ε
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), variable_identifier(), variable_list_prime()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void variable_list_prime(void) 
{
	/* look for the comma token */
	if (lookahead.code == COM_T) {
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_prime();
	}
}
/**************************************************************************************
Purpose				:	<output statement> ->
							  WRITE (<opt_variable list>);
							| WRITE (STR_T);
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), variable_identifier(), variable_list_prime()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void output_statement(void) 
{
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	opt_variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}
/**************************************************************************************
Purpose				:	<output list> ->
							<opt_variable list> | STR_T;
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	opt_variable_list(), match(), gen_incode()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void opt_variable_list(void)
{
	switch (lookahead.code) 
	{
	case AVID_T:
	case SVID_T:
		variable_list();
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
	default:
		gen_incode("PLATY: Output list (empty) parsed");
		break;
	}
}
/**************************************************************************************
Purpose				:	<arithmetic expression> ->    <unary arithmetic expression>
													| <additive arithmetic expression>
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	unary_arithmetic_expression(), syn_printe(), gen_incode(),
						additive_arithmetic_expression()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void arithmetic_expression(void) 
{
	/* look for the token */
	switch (lookahead.code) {
	case ART_OP_T:
		/* the attribute should be MULT adn DIV */
		switch (lookahead.attribute.arr_op) 
		{
		case MINUS:
		case PLUS:
			unary_arithmetic_expression();
			break;
		default:
			syn_printe();
			break;
		}
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additive_arithmetic_expression();
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	default:
		syn_printe();
		break;
	}
}
/**************************************************************************************
Purpose				:	<unary arithmetic expression> ->  - <primary arithmetic expression>
														| + <primary arithmetic expression>
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), primary_arithmetic_expression(), gen_incode(), syn_printe()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void unary_arithmetic_expression(void) 
{
	/* look for the token */
	switch (lookahead.code) 
	{
	case ART_OP_T:
		/* the attribute should be PLUS and MINUS */
		switch (lookahead.attribute.arr_op) 
		{
		case MINUS:
			match(ART_OP_T, MINUS);
			primary_arithmetic_expression();
			break;
		case PLUS:
			//match(lookahead.code, lookahead.attribute.get_int);
			match(ART_OP_T, PLUS);
			primary_arithmetic_expression();
			break;
		default:
			syn_printe();
			break;
		}
	default:
		break;
	}
	gen_incode("PLATY: Unary arithmetic expression parsed");
}
/**************************************************************************************
Purpose				:	<additive arithmetic expression> -> 
								<multiplicative arithmetic expression><additive arithmetic expression’>
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	multiplicative_arithmetic_expression(), additive_arithmetic_expression_prime()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void additive_arithmetic_expression(void)
{
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_prime();
}
/**************************************************************************************
Purpose				:	<additive arithmetic expression’> ->
								  + <multiplicative arithmetic expression><additive arithmetic expression’>
								| -  <multiplicative arithmetic expression><additive arithmetic expression’>
								| e
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), multiplicative_arithmetic_expression(), additive_arithmetic_expression_prime()
						, gen_incode(), syn_printe()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void additive_arithmetic_expression_prime(void) 
{
	/* look for the tokens */
	switch (lookahead.code) {
	case ART_OP_T:
		/* the attribute should be PLUS and MINUS */
		switch (lookahead.attribute.arr_op) 
		{
		case MINUS:
			match(ART_OP_T, MINUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_prime();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		case PLUS:
			match(ART_OP_T, PLUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_prime();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		default:
			syn_printe();
			break;
		}
	}
}
/**************************************************************************************
Purpose				:	<multiplicative arithmetic expression> ->
							<primary arithmetic expression><multiplicative arithmetic expression’>
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	primary_arithmetic_expression(), multiplicative_arithmetic_expression_prime()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void multiplicative_arithmetic_expression(void)
{
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_prime();
}
/**************************************************************************************
Purpose				:	<multiplicative arithmetic expression’> ->
								  * <primary arithmetic expression><multiplicative arithmetic expression’>
								| / <primary arithmetic expression><multiplicative arithmetic expression’>
								| e
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), primary_arithmetic_expression(), multiplicative_arithmetic_expression_prime()
						gen_incode();
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void multiplicative_arithmetic_expression_prime(void) 
{
	/* look for tokens */
	switch (lookahead.code) 
	{
	case ART_OP_T:
		/* the attribute should be MULT and DIV */
		switch (lookahead.attribute.arr_op) 
		{
		case MULT:
			match(ART_OP_T, MULT);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_prime();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;

		case DIV:
			match(ART_OP_T, DIV);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_prime();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		}
	}
}
/**************************************************************************************
Purpose				:	<primary arithmetic expression> ->
								  AVID_T
								| FPL_T
								| INL_T
								| (<arithmetic expression>)
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), arithmetic_expression(), syn_printe(), gen_incode()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void primary_arithmetic_expression(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case FPL_T:
	case INL_T:
		match(lookahead.code, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}

/*

*/
/**************************************************************************************
Purpose				:	<string expression> ->
							<primary string expression><string expression’>
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	primary_string_expression(), string_expression_prime(), gen_incode()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void string_expression(void)
{
	primary_string_expression();
	string_expression_prime();
	gen_incode("PLATY: String expression parsed");
}
/**************************************************************************************
Purpose				:	<string expression’> ->
								# <primary string expression><string expression’> | ε
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), primary_string_expression(), string_expression_prime()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void string_expression_prime(void)
{
	switch (lookahead.code)
	{
	case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_prime();
		break;
	default:
		return;
	}
}
/**************************************************************************************
Purpose				:	<primary string expression> ->
							  SVID_T
							| STR_T
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), syn_printe(), gen_incode()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void primary_string_expression(void)
{
	switch (lookahead.code)
	{
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		break;
	default:
		/* if there's epsilon, you just return. otherwise you use it. */
		syn_printe();
		break;
	}
	gen_incode("PLATY: Primary string expression parsed");
}
/**************************************************************************************
Purpose				:	<iteration statement> ->
								WHILE <pre-condition> (<conditional expression>)
								REPEAT { <statements>};
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), pre_condition(), conditional_expression(), statements(), gen_incode()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void iteration_statement(void)
{
	match(KW_T, WHILE);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);

	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");
}
/**************************************************************************************
Purpose				:	<selection statement> ->
							IF <pre-condition>  (<conditional expression>) THEN { <opt_statements> }
							ELSE { <opt_statements> } ;
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), pre_condition(), conditional_expression(), opt_statements(), gen_incode()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void selection_statement(void) 
{
	match(KW_T, IF);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);

	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);

	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");
}
/**************************************************************************************
Purpose				:	IF <pre-condition> 
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), syn_printe(),
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void pre_condition(void)
{
	if (lookahead.code == KW_T)
	{
		switch (lookahead.attribute.get_int)
		{
		case TRUE:
		case FALSE:
			match(KW_T, lookahead.attribute.get_int);
			break;
		default:
			syn_printe();
			break;
		}
	}
	else
		syn_printe();
}
/**************************************************************************************
Purpose				:	<conditional expression> ->
							<logical OR  expression>
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	logical_or_expression(), gen_incode(),
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void conditional_expression(void)
{
	logical_or_expression();
	gen_incode("PLATY: Conditional expression parsed");
}
/**************************************************************************************
Purpose				:	<logical OR expression> -> 
							<logical AND expression> <logical OR expression’>
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	logical_and_expression(), logical_or_expression_prime(),
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void logical_or_expression(void) 
{
	logical_and_expression();
	logical_or_expression_prime();
}
/**************************************************************************************
Purpose				:	<logical OR expression’> -> 
							.OR.  <logical AND expression><logical OR expression’> | e
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	logical_and_expression(), logical_or_expression_prime(),
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void logical_or_expression_prime(void) 
{
	/* look for tokens */
	switch (lookahead.code) 
	{
	case LOG_OP_T:
		/* the attribute logical operator should be OR */
		switch (lookahead.attribute.log_op) 
		{
		case OR:
			match(LOG_OP_T, OR);
			logical_and_expression();
			logical_or_expression_prime();
			gen_incode("PLATY: Logical OR expression parsed");
			break;
		default:
			break;
		}
	default:
		break;
	}
}
/**************************************************************************************
Purpose				:	<logical AND expression> ->
								<relational expression><logical AND expression’>
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	relational_expression(), logical_and_expression_prime(),
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void logical_and_expression(void) 
{
	relational_expression();
	logical_and_expression_prime();
}
/**************************************************************************************
Purpose				:	<logical AND expression’> -> 
							.AND.  <relational expression><logical AND expression’> | e
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), relational_expression(), logical_and_expression_prime(), gen_incode()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void logical_and_expression_prime(void)
{
	switch (lookahead.code)
	{
	case LOG_OP_T:
		switch (lookahead.attribute.log_op)
		{
		case AND:
			match(LOG_OP_T, AND);
			relational_expression();
			logical_and_expression_prime();
			gen_incode("PLATY: Logical AND expression parsed");
		default:
			break;
		}
	default:
		break;
	}
}
/**************************************************************************************
Purpose				:	<relational expression> ->
							  <primary a_relational expression> <primary  a_relational expression’>
							| <primary s_relational  expression><primary s_relational expression’>
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	primary_a_relational_expression(), primary_a_relational_expression_prime(), 
						primary_s_relational_expression(), primary_s_relational_expression_prime(),
						syn_printe(), gen_incode()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void relational_expression(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relational_expression();
		primary_a_relational_expression_prime();
		break;
	case SVID_T:
	case STR_T:
		primary_s_relational_expression();
		primary_s_relational_expression_prime();
		break;
	default:
		/* if there's epsilon, you just return. otherwise you use it. */
		syn_printe();
		break;
	}
	gen_incode("PLATY: Relational expression parsed");
}
/**************************************************************************************
Purpose				:	<primary a_relational expression> ->
							  AVID_T
							| FPL_T
							| INL_T
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), syn_printe(), gen_incode()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void primary_a_relational_expression(void) 
{
	switch (lookahead.code) 
	{
	case AVID_T:
	case FPL_T:
	case INL_T:
		match(lookahead.code, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}
/**************************************************************************************
Purpose				:	<primary s_relational expression> ->
								<primary string expression>
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	primary_s_relational_expression(), primary_s_relational_expression_prime(), syn_printe()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void primary_s_relational_expression(void) 
{
	switch (lookahead.code) 
	{
	case SVID_T:
	case STR_T:
		primary_string_expression();
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Primary s_relational expression parsed");
}
/**************************************************************************************
Purpose				:	<primary  a_relational expression’> ->
								  == <primary a_relational expression>
								| <> <primary a_relational expression>
								| > <primary a_relational expression>
								| < <primary a_relational expression>
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), primary_a_relational_expression(), syn_printe()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void primary_a_relational_expression_prime(void) 
{
	switch (lookahead.code) 
	{
	case REL_OP_T:
		switch (lookahead.attribute.rel_op) 
		{
		case EQ:
		case GT:
		case LT:
		case NE:
			match(REL_OP_T, lookahead.attribute.rel_op);
			primary_a_relational_expression();
		}
		break;
	default:
		syn_printe();
		break;
	}
}
/**************************************************************************************
Purpose				:	<primary  s_relational expression’> ->
								  == <primary s_relational expression>
								| <> <primary s_relational expression>
								| > <primary s_relational expression>
								| < <primary s_relational expression>
Author				:	Seongyeop Jeong
History/Version		:	1.0
Called Function		:	match(), primary_s_relational_expression(), syn_printe()
Parameters			:	void
Return value		:	void
Algorithm			:
***************************************************************************************/
void primary_s_relational_expression_prime(void) 
{
	switch (lookahead.code) 
	{
	case REL_OP_T:
		switch (lookahead.attribute.rel_op) 
		{
		case EQ:
		case NE:
		case GT:
		case LT:
			match(REL_OP_T, lookahead.attribute.rel_op);
			primary_s_relational_expression();
			break;
		default:
			syn_printe();
			break;
		}
	default:
		break;
	}
}
