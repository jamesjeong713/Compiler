/*
* File Name		: buffer.h
* Compiler		: MS Visual Studio 2015
* Author		: Seongyeop Jeong, ID# 040885882
* Course		: CST8152 - Compiler, Lab Section: 012
* Assignment	: 1
* Date			: 2 October 2018
* Professor		: Svillen Ranev
* Purpose		: This file is to implement your source code to parse the bitwise.
* function list	: b_allocate, b_addc, b_clear, b_free, b_isfull, b_limit, b_capacity, b_mark,
*                 b_mode, b_incfactor, b_load, b_isempty, b_getc, b_eob, b_print, b_compact,
*				  b_rflag, b_retract, b_reset, b_getcoffset, b_rewind, b_location
*/
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <limits.h> /* implementation-defined data type ranges and limits */

/* definition of the B_FULL macro */
#define B_FULL
/* constant definitions */
#define RT_FAIL_1 -1         /* fail return value */
#define RT_FAIL_2 -2         /* fail return value */
#define LOAD_FAIL -2       /* load fail return value */

/* You should add your own constant definitions here */
#define TRUE 1
#define FALSE 0
#define O_MODE_ZERO 0
#define O_MODE_ONE 1
#define O_MODE_MINUS_ONE -1
#define	INC_FACTOR_ZERO 0
#define INC_FACTOR_MIN 1
#define INC_FACTOR_HUNDRED 100 /* it is for the operational mode 'm' */
#define INC_FACTOR_MAX 255
#define OFFSET_RESET 0
#define MAXIMUM_ALLOWED_NUMBER SHRT_MAX-1

/* Enter your bit-masks constant definitions here */
#define ERR_INC_FACTOR 0x100
#define DEFAULT_FALGS  0xFFFC		/* default flags value: 1111 1111 1111 1100 */
#define SET_EOB        0x0001       /* set eob mask: 0001 */
#define RESET_EOB      0xFFFE       /* reset eob mask: 1111 1111 1111 1110 */
#define CHECK_EOB      0x0001       /* check eob mask: same with set */
#define SET_R_FLAG     0x0002       /* set r_flag mask: 0010 */
#define RESET_R_FLAG   0xFFFD       /* reset r_flag mask: 1111 1111 1111 1101 */
#define CHECK_R_FLAG   0x0002       /*check r_flag mask: 0010 */

/* user data type declarations */
typedef struct BufferDescriptor {
	char *cb_head;   /* pointer to the beginning of character array (character buffer) */
	short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short markc_offset; /* the offset (in chars) to the mark location */
	char  inc_factor; /* character array increment factor */
	char  mode;       /* operational mode indicator*/
	unsigned short flags; /* contains character array reallocation flag and end-of-buffer flag */
} Buffer, *pBuffer;


/* function declarations */
/*
Place your function declarations here.
Do not include the function header comments here.
Place them in the buffer.c file
*/
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer *const pBD);
void b_free(Buffer * const pBD);
int b_isfull(Buffer * const pBD);
short b_limit(Buffer *const pBD);
short b_capacity(Buffer *const pBD);
short b_mark(pBuffer const pBD, short mark);
int b_mode(Buffer * const pBD);
size_t b_incfactor(Buffer * const pBD);
int b_load(FILE * const fi, Buffer * const pBD);
int b_isempty(Buffer * const pBD);
char b_getc(Buffer * const pBD);
int b_eob(Buffer * const pBD);
int b_print(Buffer * const pBD);
Buffer * b_compact(Buffer * const pBD, char symbol);
char b_rflag(Buffer* const pBD);
short b_retract(Buffer * const pBD);
short b_reset(Buffer * const pBD);
short b_getcoffset(Buffer * const pBD);
int b_rewind(Buffer * const pBD);
char * b_location(Buffer * const pBD, short loc_offset);
#endif

/* macro for b_isfull */
#ifdef B_FULL
#define b_isfull(pBD) ((pBD==NULL) ? \
RT_FAIL_1 : \
((pBD->addc_offset == pBD->capacity) ? \
TRUE : FALSE)) 
#elif !defined B_FULL
#undef B_FULL
#endif
