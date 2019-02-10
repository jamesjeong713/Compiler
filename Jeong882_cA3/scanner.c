/***************************************************************************************
File Name		: scanner.c
Compiler		: MS Visual Studio 2015
Author			: Seongyeop Jeong (040885882) & Tanishq Bansal (040883753)
Course			: CST 8152 - Compilers, Lab Section: 012 & 014
Assignment		: 2
Date			: 4th November, 2018
Professor		: Sv. Ranev
Purpose			: The scanner reads a source program from a text file one character at a time and
produces a stream of token representations back in the buffer
Function List	: scanner_init(), malar_next_token(),  get_next_state(),
				  char_class(), aa_func02(), aa_func03(), aa_func08(), aa_func05(),
				  aa_func10(), aa_func11(), aa_func12(),iskeyword(),
****************************************************************************************/

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */

/**************************************************************************************
Purpose			:	Initializes the Scanner bufffer and clears buffer
Author			:	Sv. Ranev
History/Version	:	1.0
Called Function	:	b_isempty(psc_buf), b_rewind(psc_buf), b_clear(str_LTBL) 
Parameters		:	Buffer * psc_buf
Return value	:	EXIT_SUCCESS (0)
Algorithm		:	call functions and initialize variables
***************************************************************************************/
int scanner_init(Buffer * psc_buf) {
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}
/**************************************************************************************
Purpose			:	check the token recognization 
Author			:	Seongyeop Jeong(040885882), Tanishq Bansal(040883753)
History/Version	:	1.0
Called Function	:	b_getc(), b_retract(), b_getcoffset(), b_reset(), b_location()
					aa_table[](), b_addc()
Parameters		:	void
Return value	:	token t
Algorithm		:	in the while statement, compiler will check the next character
					if it is appropriate token with the character, assign the token code
					then return it.
					if it is number, alphabet, and double quotation, it will add the char
					to the buffer by resizing it before compact that. finally, compiler will
					call the token functions followed by each state. 
***************************************************************************************/
Token malar_next_token(void)
{
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS; /* type of state - initially not accepting */
	/* DECLARE YOUR LOCAL VARIABLES HERE IF NEEDED */
	int count = 0;

	while (1)
	{
		/* GET THE NEXT SYMBOL FROM THE INPUT BUFFER */
		c = b_getc(sc_buf);

		switch (c)
		{
		/* if SEOF return SEOF token */
		case SEOF:
		case SEOF_2:
			t.code = SEOF_T;
			t.attribute.seof = SEOF2;
			return t;
		case '\0':
			t.code = SEOF_T;
			t.attribute.seof = SEOF1;
			return t;
		/* if left paranthesis return LPR token */
		case '(':
			t.code = LPR_T;
			return t;
		/* if right paranthesis return RPR token */
		case ')':
			t.code = RPR_T;
			return t;
		/* if left brace return LBR token */
		case '{':
			t.code = LBR_T;
			return t;
		/* if right return RBR token */
		case '}':
			t.code = RBR_T;
			return t;
		/* Check '=': first - RelationalOperators */
		case '=':
			c = b_getc(sc_buf);
			switch (c)
			{
			case '=':
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
			}
			/* if it is not realtional operator, 
			it should be retracted the char then, it is assignment operator */
			b_retract(sc_buf);
			t.code = ASS_OP_T;
			return t;
			/* RelationalOperators */
		/* the not equal to operator */
		/*case '<>':
			t.code = REL_OP_T;
			t.attribute.rel_op = NE;
			return t;*/
		/* the greater than operator */
		case '>':
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
		/* the less than operator */
		case '<':
			t.code = REL_OP_T;
			t.attribute.rel_op = LT;
			if (b_getc(sc_buf) == '>')
			{
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;
				return t;
			}
			else 
			{
				b_retract(sc_buf);
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
				return t;
			}
			return t;
		/* End of the statement token */
		case ';':
			t.code = EOS_T;
			return t;
		case SPACE: 	/* white space */
		case HTAB:		/* horizontal tab (TAB) */
		case VTAB:		/* vertical tab */
		case FEED:		/* feed form */
			continue;
		case EOL: /* new line */
			line++;
			/* when the line terminator:
			it has to be started from CR (0x0d) to reset the begining of the text */
			continue;
		/* ArithmeticOperators */
		/* the plus operator */
		case '+':
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
		/* the minus operator */
		case '-':
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
		/* the divide operator */
		case '/':
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
		/* the multiplication operator */
		case '*':
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;
		/* the comma token */
		case ',':
			t.code = COM_T;
			return t;
		/* LogicalOperators */
		case '.': /* .AND. and .OR. */
			b_mark(sc_buf, b_getcoffset(sc_buf));
			/* to get the next character */
			c = b_getc(sc_buf);
			/* use the c variable to check if it is first one,
			then use the b_getc(sc_buf) to check next one again */
			if (c == 'A' && b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' &&
				b_getc(sc_buf) == '.')
			{
				t.code = LOG_OP_T;
				t.attribute.log_op = AND;
				return t;
			}
			else if (c == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.')
			{
				t.code = LOG_OP_T;
				t.attribute.log_op = OR;
				return t;
			}
			/* reset the character at the first one 
			which is already marked in line 190 */
			b_reset(sc_buf);
			/* set the token as error if there is error for AND or OR,
			then assign dot and the line terminator */
			t.code = ERR_T;
			t.attribute.err_lex[0] = '.';
			t.attribute.err_lex[1] = SEOF;
			return t;
		/* string concation */
		case '#':
			t.code = SCC_OP_T;
			return t;
		/* comment */
		case '!':
			/* mark the current location on the buffer */
			b_mark(sc_buf, b_getcoffset(sc_buf));
			c = b_getc(sc_buf);
			/* right char */
			if (c == '!') {
				/* check line terminator */
				while (c != EOL)
				{
					c = b_getc(sc_buf);
					/*check for SEOF and return SEOF token */
					if (c == SEOF || c == SEOF_2)
					{
						t.code = SEOF_T;
						t.attribute.seof = SEOF2;
						return t;
					}
				}
				line++;
				continue;
			}
			/* wrong char */
			/* to check the previous letter */
			b_retract(sc_buf);
			t.code = ERR_T;
			t.attribute.err_lex[0] = '!';
			t.attribute.err_lex[1] = c;
			t.attribute.err_lex[2] = '\0';
			while (c != EOL) /* loop until the new line */
				c = b_getc(sc_buf);
			line++;
			return t;
		}
		/* finite automaton */
		if (isdigit(c) || isalpha(c) || c=='"')
		{
			/* set the mark at the begning of the lexeme and save it in lexstart */
			lexstart = b_mark(sc_buf, b_getcoffset(sc_buf) - 1);
			/* state is already 0 as local variable and is being input char c
			also inside of the get_next_state, next is calling the transition table */
			state = get_next_state(state, c, &accept);
			/* if it is not accepting state, loop until it is accepting */
			while (accept == NOAS)
			{
				c = b_getc(sc_buf);
				/* to find the next state with the char c through transition table */
				state = get_next_state(state, c, &accept);
			}
			/* if it is accepting state, retract a lex previously */
			if (accept == ASWR)
				b_retract(sc_buf);
			/* set lexend to getc_offset */
			lexend = b_getcoffset(sc_buf);
			/* temp lexeme buffer */
			lex_buf = b_allocate(lexend - lexstart, 0, 'f');
			/* reset the current offset to add the lexeme from the place previously */
			b_reset(sc_buf);
			/* copy the lexeme by using the b_addc() with b_getc between lexstart and lexend
			FROM THE INPUT BUFFER INTO lex_buf USING b_addc(...), */
			for (count = lexstart; count < lexend; count++)
				b_addc(lex_buf, b_getc(sc_buf));
			b_compact(lex_buf, '\0');
			/* call the accpeting functions */
			t = aa_table[state](b_location(lex_buf, 0));
			b_free(lex_buf);
		}
		else 
		{
			t.code = ERR_T;
			t.attribute.err_lex[0] = c;
			t.attribute.err_lex[1] = SEOF;
		}
		return t;
	}//end while(1)
}
/**************************************************************************************
Purpose			:	This function is to move to next state by calling two functions
					1) char_class(c) to get column 
					2) st_table[][] to get value in transition table
					so that it will return the state number
Author			:	Sv. Ranev
History/Version	:	1.0
Called Function	:	assert(), st_table, as_table, char_class()
Parameters		:	int state, char c, int *accept
Return value	:	int next (next state)
Algorithm		:	1. set the columns by calling char_class(char)
					2. the initialize next by using the transition table
					3. assign the accepting table with next state, 
					4. add runtime diagonistic by using assert function
					5. and return the int next
***************************************************************************************/
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

	assert(next != IS);


#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}
/**************************************************************************************
Purpose			:	This function is to return the column index 
					to use the transition table (to select input symbol)
Author			:	Seongyeop Jeong (040885882)
History/Version	:	1.0
Called Function	:	isalpha(), isdigit()
Parameters		:	char c
Return value	:	0 ~ 7
Algorithm		:	1. the character has to be check with each if statement
					2. in the matched statement, it should return 0 ~ 7 appropriately
***************************************************************************************/
int char_class(char c)
{
	/* [a-zA-Z] */
	if (isalpha(c))
		return 0;
	/* if it is digits */
	else if (isdigit(c))
	{
		/* if it is [0] */
		if (c == '0')
			return 1;
		/* if it is [1-9] */
		else
			return 2;
	}
	/* [.] */
	else if (c == '.')
		return 3;
	/* concatenation [$] */
	else if (c == '$')
		return 4;
	/* SL ["] */
	else if (c == '"')
		return 6;
	/* line terminator [\0] */
	else if (c == '\0' || c == SEOF) 
		return 7;
	/* [others] */
	else
		return 5;
}
/**************************************************************************************
Purpose			:	CHECK IF THE LEXEME IS A KEYWORD. SET a AVID TOKEN.
Author			:	Tanishq Bansal(040883753), Seongyeop Jeong(040885882)
History/Version	:	1.0
Called Function	:	iskeyword()
Parameters		:	Pointer of char
Return value	:	the Token
Algorithm		:	CHECK IF THE LEXEME IS A KEYWORD.IF YES, IT MUST
					RETURN A TOKEN WITH THE CORRESPONDING ATTRIBUTE
					FOR THE KEYWORD. THE ATTRIBUTE CODE FOR THE KEYWORD
					IS ITS INDEX IN THE KEYWORD LOOKUP TABLE (kw_table in table.h).
					IF THE LEXEME IS NOT A KEYWORD,SET a AVID TOKEN.
					IF THE lexeme IS LONGER than VID_LEN (see token.h) CHARACTERS,
					ONLY FIRST VID_LEN CHARACTERS ARE STORED
					NTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[](see token.h) .
					ADD \0 AT THE END TO MAKE A C-type STRING.
***************************************************************************************/
Token aa_func02(char lexeme[])
{
	/* variable x for keyword */
	int x = 0;
	/* if the keyword is matched with the kw_table, assign to the x*/
	x = iskeyword(lexeme);
	/* variable h to check for variable identifier length */
	int h = 0;
	Token t;
	/* if the keyword is assigned successfully, it is not AVID.
	the reason that we did check keyword in AVID is that SVID has the $ end of the letter, 
	but AVID doesn't have anything so that we should figure out what AVID with keyword */
	if (x != RT_FAIL_1)
	{
		/* set token code as keyword (KW_T) */
		t.code = KW_T;
		/* assign keyword number into keyword table index number */
		t.attribute.kwt_idx = x;
		return t;
	}
	/* if the keyword is not assigned, it means the AVID */
	else
	{
		/* set the token code AVID_T */
		t.code = AVID_T;
		/* check the length of lexeme if it is greater than variable identifier length 
		to assign the lexeme until VID_LEN */
		if (strlen(lexeme) > VID_LEN)
		{
			for (h = 0; h < VID_LEN; h++) {
				t.attribute.vid_lex[h] = lexeme[h];
			}
			/* assign the line terminator in the end of the lexeme */
			t.attribute.vid_lex[VID_LEN] = '\0';
		}
		/* if the length of lexeme is less than 8 digits, 
		just copy to vid_lex in token attributes, 
		then put the line terminator into 8th of place of array */
		else
		{
			strcpy(t.attribute.vid_lex, lexeme);
			t.attribute.vid_lex[strlen(lexeme)] = '\0';
		}
	}
	return t;
}
/**************************************************************************************
Purpose			:	SET a SVID TOKEN.
Author			:	Tanishq Bansal(040883753), Seongyeop Jeong(040885882)
History/Version	:	1.0
Called Function	:	-
Parameters		:	Pointer of char
Return value	:	the Token
Algorithm		:	IF THE lexeme IS LONGER than VID_LEN characters,
					ONLY FIRST VID_LEN-1 CHARACTERS ARE STORED
					INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[],
					AND THEN THE $ CHARACTER IS APPENDED TO THE NAME.
					ADD \0 AT THE END TO MAKE A C-type STRING.
***************************************************************************************/
Token aa_func03(char lexeme[])
{
	Token t;
	int i = 0;
	/* set the token code as SVID_T */
	t.code = SVID_T;
	/* IF THE lexeme IS LONGER than VID_LEN characters */
	if(VID_LEN <= strlen(lexeme))
	{
		/*ONLY FIRST VID_LEN-1 CHARACTERS ARE STORED INTO 
		THE VARIABLE ATTRIBUTE ARRAY vid_lex[],*/
		strncpy(t.attribute.vid_lex, lexeme, (VID_LEN - 1));
		for (i = 0; i < VID_LEN; i++)
			/*storing the charcters in the array*/
			t.attribute.vid_lex[i] = lexeme[i];
		/*appending the $ to the name*/
		t.attribute.vid_lex[VID_LEN - 1] = '$';
		/*Adding the /0 the end*/
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	/*if lexeme is not longer the VID_LEN*/
	else
	{
		strncpy(t.attribute.vid_lex, lexeme, strlen(lexeme));
		t.attribute.err_lex[strlen(lexeme)] = '\0';
	}
	return t;
}
/**************************************************************************************
Purpose			:	to convert the string to the float number. if it is in the range
					of the float number, return appropriate token. if it is not, call
					error token function
Author			:	Tanishq Bansal(040883753), Seongyeop Jeong(040885882)
History/Version	:	1.0
Called Function	:	atof
Parameters		:	Pointer of char
Return value	:	the Token
Algorithm		:	1. by using the atof, convert string to float number
					2. check if it is right in the range
					3. if it is not, call the function for error token 
***************************************************************************************/
Token aa_func08(char lexeme[])
{
	Token t;
	/* convert string to float numbers */
	double newNumber = atof(lexeme);
	/* range of the float number || new number = 0 
	-> set the code and assign the number to the flt_value in token */
	if ((newNumber >= FLT_MIN && newNumber <= FLT_MAX) || newNumber == 0)
	{		
			t.code = FPL_T;
			t.attribute.flt_value = (float)newNumber;
			return t;	
	}
	/* after assigning converted number, 
	then it calls token 11 which is error token without retract, 
	if it is not in the range of the float numbers */
	return aa_table[11](lexeme);
}
/**************************************************************************************
Purpose			:	accept function for the integer literal(IL) - decimal constant (DIL)
Author			:	Seongyeop Jeong (040885882)
History/Version	:	1.0
Called Function	:	atol(), aa_table[]()
Parameters		:	char lexeme[]
Return value	:	token t
Algorithm		:	1. by using atol c lib, convert the string to a long integer
					2. in the range of the short integer, set the code as Integer
					   then, store short integer which is converted to int_value
					3. if it is not short number, call the function of error state
***************************************************************************************/
Token aa_func05(char lexeme[])
{
	Token t;
	/* convert the string to a long integer */
	long convInt = atol(lexeme);
	/* check range of short number */
	if (convInt >= SHRT_MIN && convInt <= SHRT_MAX)
	{
		/* set the code as integer */
		t.code = INL_T;
		/* store value as short integer */
		t.attribute.int_value = (short)convInt;
	}
	/* if it is not short number, */
	else {
		/* call the error token */
		t = aa_table[11](lexeme);
	}
	return t;
}
/**************************************************************************************
Purpose			:	This function is to store the lexeme into the string literal table
					(Accpeting function for the SL)
Author			:	Tanishq Bansal(040883753)
History/Version	:	1.0
Called Function	:	strlen(), b_limit(), b_addc()
Parameters		:	char lexeme[]
Return value	:	token t
Algorithm		:	1. set the str_offset by using b_limit()
					2. store a string of lexeme on the str_LTBL until maximum length of
					   lexeme - 1, then add line terminator
***************************************************************************************/
Token aa_func10(char lexeme[])
{
	Token t;
	int i = 0;
	/* store string token */
	t.code = STR_T;
	/* set the offset of current size of buffer(str_LTBL) by using b_limit() */
	t.attribute.str_offset = b_limit(str_LTBL); 
	/* add string buffer until before the line terminator which is the location of maximum length */
	for (i = 0; i < (int)strlen(lexeme); i++)
	{
		if (lexeme[i] != '"')
		/* store a string of lexeme on the str_LTBL */
			b_addc(str_LTBL, lexeme[i]);
		/* if lexeme has the new line, count line number */
		if (lexeme[i] == EOL)
			line++;
	}
	/* store line terminator in the end of the buffer */
	b_addc(str_LTBL, '\0');
	return t;
}
/**************************************************************************************
Purpose			:	This function is for accepting state for the error token WITHOUT RETRACT
Author			:	Seongyeop Jeong (040885882)
History/Version	:	1.0
Called Function	:	strlen(), strncpy()
Parameters		:	char lexeme[]
Return value	:	toekn t 
Algorithm		:	1. check length of lexeme, if it is shorter than 20 length, 
					   store lexeme to err_lex normally.
					2. otherwise (string length of lexeme >= 20), 
					   copy lexeme to err_lex until 17 length
***************************************************************************************/
Token aa_func11(char lexeme[])
{
	Token t;
	/* initialize the i to loop */
	int i = 0;
	/* set the token code as error token */
	t.code = ERR_T;
	/* if the string length is less than 20, */
	if (strlen(lexeme) < ERR_LEN)
	{
		/* store the lexeme into err_lex of tokens until length of lexeme */
		for (i = 0; i < (int)strlen(lexeme); i++)
		{
			t.attribute.err_lex[i] = (int)lexeme[i];
			/* if it is new line, count the line number */
			if (lexeme[i] == EOL)
				line++;
		}
		/* when the loop is finish until the before string length of lexeme, 
		store line terminator */
		t.attribute.err_lex[strlen(lexeme)] = '\0';
	}
	/* if the string length is greater than or equal to 20 */
	else
	{
		/* copy all strings into err_lex until 17 length */
		strncpy(t.attribute.err_lex, lexeme, (ERR_LEN - 3));
		/* then print out three dots, and line terminator */
		t.attribute.err_lex[ERR_LEN - 3] = '.';
		t.attribute.err_lex[ERR_LEN - 2] = '.';
		t.attribute.err_lex[ERR_LEN - 1] = '.';
		t.attribute.err_lex[ERR_LEN] = '\0';
	}
	return t;
}
/**************************************************************************************
Purpose			:	This function is for accepting state for the error state WITH RETRACT
					When the lexeme has something error, the compiler will store the error token
					then retract the char, and find the error state.
Author			:	Seongyeop Jeong (040885882)
History/Version	:	1.0
Called Function	:	strlen()
Parameters		:	char lexeme[]
Return value	:	token t
Algorithm		:	1. set the string length
					2. set the error token 
					3. it checks and store in each different length which is longer than ERR_LEN
					   or shorter than ERR_LEN
***************************************************************************************/
Token aa_func12(char lexeme[])
{
	Token t;
	/* set the length of lexeme */
	int length = strlen(lexeme);
	/* set the token */
	t.code = ERR_T;
	/* if the length is longer than 20 length, */
	if (length > ERR_LEN)
	{
		/* it should print out until 17 length*/
		for (int i = 0; i < ERR_LEN-3; i++)
		{
			t.attribute.err_lex[i] = lexeme[i];
			/* if there is new line, line counter ++ */
			if (lexeme[i] == EOL)
				line++;
		}
		/* after printing out 17 digits, compiler will show three dots, then '\0' */
		t.attribute.err_lex[ERR_LEN - 3] = '.';
		t.attribute.err_lex[ERR_LEN - 2] = '.';
		t.attribute.err_lex[ERR_LEN - 1] = '.';
		t.attribute.err_lex[ERR_LEN] = '\0';
	}
	/* if the length is not longer than 20 length, */
	else
	{
		/* it will store the lexeme into err_lex[] until 20 length */
		for (int i = 0; i< length; i++)
		{
			t.attribute.err_lex[i] = lexeme[i];
			if (lexeme[i] == EOL)
				line++;
		}
		/* then, it should be line terminator on the end of the line */
		t.attribute.err_lex[length] = '\0';
	}
	return t;
}
/**************************************************************************************
Purpose			:	Checks for a keyword of string patterns from the array of kw_table
Author			:	Seongyeop Jeong (040885882)
History/Version	:	1.0
Called Function	:	strncmp()
Parameters		:	Pointer of char
Return value	:	the index of the keyword
Algorithm		:	compare all characters with the kw_table[] so that it find the keyword
***************************************************************************************/
int iskeyword(char * kw_lexeme)
{
	int idx = 0;
	/* to avoid runtime error */
	if (kw_lexeme == NULL)
		return RT_FAIL_1;
	/* compare all the strings with keywords within kw_table */
	while (idx < KWT_SIZE)
	{
		/* when the both strings are equal, strncmp() will return 0 
		so that it is returned index number of keyword when it is equal */
		if (strncmp(kw_lexeme, kw_table[idx], strlen(kw_table[idx])) == 0)
			return idx;
		idx++;
	}
	return RT_FAIL_1;
}

