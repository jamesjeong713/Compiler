/***************************************************************************************
File Name		: table.h
Compiler		: MS Visual Studio 2015
Author			: Seongyeop Jeong (040885882) & Tanishq Bansal (040883753)
Course			: CST 8152 - Compilers, Lab Section: 012 & 014
Assignment		: 2
Date			: 4th November, 2018
Professor		: Sv. Ranev
Purpose			: contains transition table and constants
Function List	: -
****************************************************************************************/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

#define EOL 0x0a /* new line */
#define VTAB 0x0b /* vertical tab */
#define HTAB 0x09 /* horizontal tab (TAB) */
#define SPACE 0x20
#define FEED 0x0c /* feed form */
#define SEOF '\0'
/*   Source end-of-file (SEOF) sentinel symbol
*    '\0' or one of 255,0xFF,EOF
*/

/*  Special case tokens processed separately one by one
*  in the token-driven part of the scanner
*  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
*  white space
*  !!comment , ',' , ';' , '-' , '+' , '*' , '/', # ,
*  .AND., .OR. , SEOF, 'illegal symbol',
*/


/*REPLACE *ESN* and *ESR* WITH YOUR ERROR STATE NUMBER*/
#define ES  11 /* Error state  with no retract */
#define ER  12 /* Error state  with retract */
#define IS -1    /* Inavalid state */

/* State transition table definition */

/*REPLACE *CN* WITH YOUR COLUMN NUMBER*/

#define TABLE_COLUMNS 8
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/*.
	.YOUR TABLE INITIALIZATION HERE
	. input symbols 
	/* State 0 */{ 1,  6,  4,  ES, ES, ES, 9,  IS },
	/* State 1 */{ 1,  1,  1,  2, 3,  2,  2,  2 }, /* ES/AS2 */
	/* State 2 */{ IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 3 */{ IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 4 */{ ES, 4,  4,  7,  5,  5,  5,  5 }, /*ES/AS5*/
	/* State 5 */{ IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 6 */{ ES, 6,  ES, 7,  ES, 5,  5,  5 },
	/* State 7 */{ 8,  7,  7,  8,  8,  8,  8,  8 },
	/* State 8 */{ IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 9 */{ 9,  9,  9,  9,  9,  9,  10, 12 }, /*SHOULD CHECK*/
	/* State 10*/{ IS, IS, IS, IS, IS, IS, IS, IS }, /*ASNR / ES */
	/* State 11*/{ IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 12*/{ IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 13*//*{ES}*/ /* reserved for future use */
};

	/* Accepting state table definition */
	/*REPLACE *N1*, *N2*, and *N3* WITH YOUR NUMBERS*/
	/* Is it my arbitary number? or specific state num? DIL / FPL */
#define ASWR     0  /* accepting state with retract */
#define ASNR     1  /* accepting state with no retract */
#define NOAS     2  /* not accepting state */

int as_table[] = { 
	/*YOUR INITIALIZATION HERE - USE ASWR, ASNR, NOAS */
	/* State 00 */ NOAS,
	/* State 01 */ NOAS,
	/* State 02 */ ASWR,
	/* State 03 */ ASNR,
	/* State 04 */ NOAS,
	/* State 05 */ ASWR,
	/* State 06 */ NOAS,
	/* State 07 */ NOAS,
	/* State 08 */ ASWR,
	/* State 09 */ NOAS,
	/* State 10 */ ASNR,
	/* State 11 */ ASNR,
	/* State 12 */ ASWR,
	/* State 13 - reserved */ 
};

/* Accepting action function declarations */

/*FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
ONE FUNCTION PROTOTYPE. THEY ALL RETURN Token AND TAKE
ONE ARGUMENT: A string REPRESENTING A TOKEN LEXEME. */

Token aa_func02(char lexeme[]); /* AVID, KW */
Token aa_func03(char lexeme[]); /* VID SVID - ASNR */
Token aa_func05(char lexeme[]); /* DIL */
Token aa_func08(char lexeme[]); /* FPL */
Token aa_func10(char lexeme[]); /* STR_Literal */
Token aa_func12(char lexeme[]); /* ER token */
Token aa_func11(char lexeme[]); /* Error state */

/* defining a new type: pointer to function (of one char * argument)
returning Token
*/

typedef Token(*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
* Token (*aa_table[])(char lexeme[]) = {
*/

PTR_AAF aa_table[] = {

	/*HERE YOU MUST PROVIDE AN INITIALIZATION FOR AN ARRAY OF POINTERS
	TO ACCEPTING FUNCTIONS.THE ARRAY HAS THE SAME SIZE AS as_table[].
	YOU MUST INITIALIZE THE ARRAY ELEMENTS WITH THE CORRESPONDING
	ACCEPTING FUNCTIONS(FOR THE STATES MARKED AS ACCEPTING IN as_table[]).
	THE REST OF THE ELEMENTS MUST BE SET TO NULL.*/
	/* State 00 */ NULL,
	/* State 01 */ NULL,
	/* State 02 */ aa_func02,
	/* State 03 */ aa_func03,
	/* State 04 */ NULL,
	/* State 05 */ aa_func05,
	/* State 06 */ NULL,
	/* State 07 */ NULL,
	/* State 08 */ aa_func08,
	/* State 09 */ NULL,
	/* State 10 */ aa_func10,
	/* State 11 */ aa_func11,
	/* State 12 */ aa_func12,
	/* State 13 */ /*NULL,*/
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char * kw_table[] =
{
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"
};

#endif
