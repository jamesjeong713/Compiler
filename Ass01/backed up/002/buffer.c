/*
* File Name		: buffer.c
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

#include <stdlib.h>
#include "buffer.h"

/*
* Purpose: This method is to allocate new memory with malloc on the heap.
		   it will be different with increment values depending on what mode do user want to see.
		   (mode: fixed, additive, multiplicative)
* Author: Seongyeop Jeong
* Versions: 
* Called Functions: calloc(), malloc(), free()
* Parameters: short init_capacity, char inc_factor, char o_mode
* Return value: pBD, NULL
* Algorithm:	1. allocate memory for one Buffer;
				2. allocates memory for one dynamic character buffer;
				3. sets the buffer operational mode indicator mode
*/
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode)
{
	/* allocate memory for one Buffer descriptor */
	pBuffer pB = (Buffer *)calloc(1, sizeof(Buffer));
	/* If the pBuffer declare, it might be caused NULL even though pB is dereferenced.
	   so that it has to be returned NULL to avoid run-time error when pBuffer has null value */
	if (pB == NULL)
	{
		return NULL;
	}
	unsigned char unsignedIncFactor = (unsigned char)inc_factor;
	pB->flags |= DEFAULT_FALGS;
	/* allocates memory for one dynamic character buffer */
	pB->cb_head = (char *)malloc(sizeof(char) * init_capacity);
	/* sets the buffer operational mode indicator mode */
	if (o_mode == 'f' && unsignedIncFactor == INC_FACTOR_ZERO)
	{
		pB->mode = O_MODE_ZERO;
		pB->inc_factor = INC_FACTOR_ZERO;
	}
	else if (o_mode == 'f' && unsignedIncFactor != INC_FACTOR_ZERO)
	{
		pB->mode = O_MODE_ZERO;
		pB->inc_factor = INC_FACTOR_ZERO;
	}
	else if (o_mode == 'a' && (unsignedIncFactor >= INC_FACTOR_MIN) && (unsignedIncFactor <= INC_FACTOR_MAX))
	{
		pB->mode = O_MODE_ONE;
		pB->inc_factor = unsignedIncFactor;
	}
	else if (o_mode == 'm' && (unsignedIncFactor >= INC_FACTOR_MIN) && (unsignedIncFactor <= INC_FACTOR_HUNDRED))
	{
		pB->mode = O_MODE_MINUS_ONE;
		pB->inc_factor = unsignedIncFactor;
	}
	/* if there is another condition which developer do not want, 
	   it has to abort the buffer by using free function and return NULL. */
	else {
		free(pB);
		return NULL;
	}
	/* if allocation memory fails, abort and release it */
	if (pB->cb_head == NULL)
	{
		free(pB);
		return NULL;
	}
	/* copies the given init_capacity value into the Buffer structure capacity variable. */
	pB->capacity = init_capacity;
	return pB;
}

/*
* Purpose: The function resets r_flag to 0 and decide how many space it apply depending on the mode
		   then tries to add the character symbol to the character array which come through buffer pointed by pBD
		   if the reallocation is successful, this will be set r_flag to 1 with bitwise operation
* Author: Seongyeop Jeong
* Versions: 
* Called Functions: realloc()
* Parameters: pBuffer const pBD, char symbol
* Return value: pBuffer, NULL
NEED TO FIX IT
* Algorithm: 1. check the buffer is operational and not full, add the character to buffer
*			 2. check the buffer is full, resize the buffer
*			 3. check the operational mode, function returns different value
*				operational mode indicator mode
*/
pBuffer b_addc(pBuffer const pBD, char symbol)
{
	unsigned short newCapacity = 0;
	short available_space = 0;
	short newInc = 0;
	char* temp;

	if (pBD == NULL)
		return NULL;
	/* flags field reset the allocation */
	pBD->flags &= RESET_R_FLAG;

	if (pBD->addc_offset >= pBD->capacity)
	{
		if (pBD->mode == O_MODE_ZERO)
			return NULL;		
		else if (pBD->mode == O_MODE_ONE)
		{
			/* if the result is positive and does not exceed minus 1, this function proceeds*/
			if (pBD->capacity < 0 || pBD->capacity >= SHRT_MAX -1)
				return NULL;

			newCapacity = (unsigned short)pBD->capacity + (unsigned char)pBD->inc_factor;
			if (newCapacity > SHRT_MAX - 1 && pBD->capacity < SHRT_MAX - 1)
			{
				newCapacity = SHRT_MAX - 1;
				return NULL;
			} 
			else if (pBD->capacity == SHRT_MAX - 1)
			{
				return pBD->capacity;
			}
			
			/*if ((unsigned short)pBD->capacity < SHRT_MAX - 1)
				newCapacity = (unsigned short)pBD->capacity + (unsigned char)pBD->inc_factor;
			else if (pBD->capacity > SHRT_MAX - 1)
				newCapacity = SHRT_MAX - 1;*/
		} /* end of the if function: mode 1 */
		/* if the mode -1, incearse the current capacity of the buffer to a new capacity*/
		else if (pBD->mode == O_MODE_MINUS_ONE)
		{
			/* if current capacity cannot be incremented anymore
			because it has reached the maximum capacity of the buffer,
			the function returns NULL */
			if (pBD->capacity == SHRT_MAX - 1)
				return NULL;

			available_space = SHRT_MAX - 1 - pBD->capacity;
			newInc = (available_space * (unsigned char)pBD->inc_factor / 100.0f);

			if (pBD->capacity + newInc < SHRT_MAX - 1 && newInc != 0)
				newCapacity = pBD->capacity + newInc;
			else if (pBD->capacity < SHRT_MAX - 1)
				pBD->capacity = SHRT_MAX - 1;
		} /* end of if function : mode -1 */
		else
			return NULL;
		/* If the capacity increment in mode 1 or -1 is successful, reallocate memory
		expand the character buffer calling realloc() with the new capacity */
		temp = (char *)realloc(pBD->cb_head, sizeof(char)*newCapacity);
		/* If reallocation fails, returns NULL */
		if (temp == NULL)
			return NULL;
		else /* if the reallocation is successful, */
		{
			if (temp != pBD->cb_head)
				pBD->flags |= SET_R_FLAG; /* sets r_flag bit to 1 */
		}
		pBD->cb_head = temp;
		pBD->capacity = newCapacity;
	}
	/* adds the char symbol to the bufer content*/
	pBD->cb_head[pBD->addc_offset++] = symbol; 
	return pBD;
}

/*
* Purpose: This function is to reset the offsets to clean the memory locations currently buffer struct 
		   such as addc, getc, and markc. 
* Author: Seongyeop Jeong
* Versions:
* Called Functions: return
* Parameters: Buffer* const pBD
* Return value: -1, 1
* Algorithm: 1. 
*/
int b_clear(Buffer * const pBD) {
	/* If run-time error ocurrs, returns -1 */
	if (pBD == NULL)
		return RT_FAIL_1;
	/* reinitialize the data members of buffer to 0 */
	pBD->addc_offset &= OFFSET_RESET;
	pBD->getc_offset &= OFFSET_RESET;
	pBD->markc_offset &= OFFSET_RESET;
	/* except for 0 (false), every numbers are TRUE, here's true is defined to 1 */
	return TRUE; 
}

/*
* Purpose: This function is to abort and release memory of the Buffer struct when this application is done. 
* Author: Seongyeop Jeong
* Versions:
* Called Functions: free
* Parameters: Buffer* const pBD
* Return value: void
* Algorithm: 1.
*/
void b_free(Buffer *const pBD)
{
	if (pBD != NULL) 
	{
		/* free the pointer of the buffer firstly, then free memory of the buffer.*/
		free(pBD->cb_head);
		free(pBD);
	}
}

/*
* Purpose: This function is to check if the character on the buffer is full. then it returns false, true or -1
* Author: Seongyeop Jeong
* Versions:
* Called Functions: return
* Parameters: Buffer* const pBD
* Return value: -1, 1, 0
* Algorithm: 1.
*/
int b_isfull(Buffer *const pBD)
{
	if (pBD == NULL)
		return RT_FAIL_1;
	if (pBD->addc_offset == pBD->capacity)
		return TRUE;
	else
		return FALSE;
}

/*
* Purpose: This function is to returns current limit size of buffer.
* Author: Seongyeop Jeong
* Versions:
* Called Functions: return
* Parameters: Buffer* const pBD
* Return value: addc_offset
* Algorithm: 1.
*/
short b_limit(Buffer *const pBD)
{
	if (pBD == NULL)
		return RT_FAIL_1;

	return pBD->addc_offset;
}

/*
* Purpose: This function is to return current capacity on the buffer
* Author: Seongyeop Jeong
* Versions:
* Called Functions: return 
* Parameters: Buffer* const pBD
* Return value: -1, capacity
* Algorithm: 1.
*/
short b_capacity(Buffer *const pBD)
{
	if (pBD == NULL)
		return RT_FAIL_1;

	return pBD->capacity;
}

/*
* Purpose: This function is to return the current limit size of the buffer
* Author: Seongyeop Jeong
* Versions:
* Called Functions:
* Parameters: Buffer const pBD, short mark
* Return value: -1, markc_offset
* Algorithm: 1.
*/
short b_mark(pBuffer const pBD, short mark)
{
	if (pBD == NULL)
		return RT_FAIL_1;

	if (mark < 0 || mark > pBD->addc_offset)
		return RT_FAIL_1;

	pBD->markc_offset = mark;

	return pBD->markc_offset;
}

/*
* Purpose: This function is to return what mode does this application run.
* Author: Seongyeop Jeong
* Versions:
* Called Functions: return
* Parameters: Buffer* const pBD
* Return value: -1, 1, 0
* Algorithm: 1.
*/
int b_mode(Buffer * const pBD)
{
	/* run-time error, this function notify the calling function about the failure */
	if (pBD == NULL)
		return LOAD_FAIL; 

	return pBD->mode;
}

/*
* Purpose: This function is to return the increment factor which is unsigned value.(non-negative value)
* Author: Seongyeop Jeong
* Versions:
* Called Functions: return
* Parameters: Buffer* const pBD
* Return value: 0x100, inc_factor in struct
* Algorithm: 1.
*/
size_t b_incfactor(Buffer * const pBD)
{
	if (pBD == NULL)
		return ERR_INC_FACTOR;

	return (unsigned char)pBD->inc_factor;
}

/*
* Purpose: This function is to return the number of characters on the buffer 
* Author: Seongyeop Jeong
* Versions:
* Called Functions:
* Parameters: FILE* const fi, Buffer* const pBD
* Return value: -1, -2, charNum
* Algorithm: 1. check if there is run-time error
			 2. get a character continuously until the end of the file
			 3. each character will add to the buffer through b_addc method
			 4. if the b_addc method's return is not null, 
			 count up the number of characters how many character does it work with the method 
*/
int b_load(FILE * const fi, Buffer * const pBD)
{
	char buffer;
	int charNum = 0;

	if (fi == NULL || pBD == NULL)
		return RT_FAIL_1;

	while (1) /* repeat until the standard macro feof(fi)detects EOF */
	{
		buffer = (char)fgetc(fi);

		/* feof function: tests the end-of-file indicator,
		it will be returned non-zero when end-of-file indicator
		associated with the stream is set */
		if (feof(fi))
			break;
		if (b_addc(pBD, buffer) == NULL)
		{
			ungetc(buffer, fi);
			printf("The last character read from the file is: %c %d\n", buffer, buffer);
			return LOAD_FAIL;
		}
		charNum++;
	}
	return (int)charNum;
}

/*
* Purpose: This function is to check if the buffer's addc_offset is 0 or not.
* Author: Seongyeop Jeong
* Versions:
* Called Functions: return
* Parameters: Buffer* const pBD
* Return value: -1, 1, 0
* Algorithm: 1. check if the run-time erorr is happened
			 2. check if the addc_offset is 0, then return 1.
			 3. check if any else situation will be return 0.
*/
int b_isempty(Buffer * const pBD)
{
	if (pBD == NULL)
		return RT_FAIL_1;
	/* if the addc_offset is 0, the function returns 1; otherwise it returns 0 */
	if (pBD->addc_offset == 0)
		return TRUE;
	else
		return FALSE;
}

/*
* Purpose: This function is to get character of the buffer. 
		   When the addc_offset is same with the getc_offset, 
		   set the eob to 1 bit to make sure that it has been reached current limit on the buffer 
* Author: Seongyeop Jeong
* Versions:
* Called Functions: return
* Parameters: Buffer* const pBD
* Return value: -2, 0, pointer of the buffer
* Algorithm: 1. in b_print method, b_getc will be checked until it will catch them if getc and addc are equal.
			 2. when the getc and addc are equal, set the EOB to 1 bit, and returns 0.
			 3. if not, reset the eob to 0 bit. Then next pointer of getc_offset of the buffer will be returned 
*/
char b_getc(Buffer * const pBD)
{
	if (pBD == NULL)
		return RT_FAIL_2;
	/* if the getc and addc are equal, the current buffer has been reached on the buffer limit */
	if (pBD->getc_offset == pBD->addc_offset)
	{
		pBD->flags |= SET_EOB;
		return 0;
	}
	/* reset the eob to 0 bit so that it has not been reached current limit on the buffer */
	else
		pBD->flags &= RESET_EOB;

	return pBD->cb_head[pBD->getc_offset++]; 
}

/*
* Purpose: This function is to return value to check in b_print method if it is end of the buffer
* Author: Seongyeop Jeong
* Versions:
* Called Functions: return
* Parameters: Buffer* const pBD
* Return value: -1, return flags which is checked 0x0001
* Algorithm: 1. make a new variable to check if it is end of buffer. 
			 2. then returns the variable to the b_print method
*/
int b_eob(Buffer * const pBD)
{
	/* to avoid run-time error */
	if (pBD == NULL)
		return RT_FAIL_1;

	short checkEob;
	checkEob = pBD->flags & CHECK_EOB;
	
	return checkEob;
}

/*
* Purpose:
* Author: Seongyeop Jeong
* Versions:
* Called Functions:
* Parameters: Buffer* const pBD
* Return value: -1, 0, getc_offset
* Algorithm: 1.
*/
int b_print(Buffer * const pBD)
{
	char buffer;

	if (pBD == NULL)
		return RT_FAIL_1;

	if (pBD->addc_offset == FALSE)
	{
		printf("Empty buffer!");
		printf("\n");
		return 0;
	}

	while (1)
	{
		buffer = b_getc(pBD);

		if (b_eob(pBD))
			break;
		printf("%c", buffer);
	}
	printf("\n");

	return pBD->getc_offset;
}

/*
* Purpose:
* Author: Seongyeop Jeong
* Versions:
* Called Functions:
* Parameters: Buffer* const pBD, char symbol
* Return value: -1, NULL, Buffer
* Algorithm: 1.
*/
Buffer * b_compact(Buffer * const pBD, char symbol)
{
	char* temp;

	if (pBD == NULL)
		return NULL;
	/* uses realloc to adjust the new capacity */
	temp = (char*)realloc(pBD->cb_head, pBD->addc_offset + 1);
	
	if (temp == NULL)
		return NULL;

	if (pBD->capacity <= SHRT_MAX - 1)
		pBD->capacity = pBD->addc_offset + 1;
	pBD->cb_head = temp;
	/* the function adds the symbol to the end of the char buffer */
	pBD->cb_head[pBD->addc_offset++] = symbol;
	/* it must set the r_flag bit */
	if (temp != pBD->cb_head)
		pBD->flags |= SET_R_FLAG;

	return pBD;
}

/*
* Purpose:
* Author: Seongyeop Jeong
* Versions:
* Called Functions:
* Parameters: Buffer* const pBD
* Return value: -1, flags which is set r_flag to 1
* Algorithm: 1.
*/
char b_rflag(Buffer* const pBD)
{
	if (pBD == NULL)
		return RT_FAIL_1;

	return pBD->flags &= SET_R_FLAG; // check 
}

/*
* Purpose:
* Author: Seongyeop Jeong
* Versions:
* Called Functions:
* Parameters: Buffer* const pBD
* Return value: 
* Algorithm: 1.
*/
short b_retract(Buffer * const pBD)
{
	if (pBD == NULL)
		return RT_FAIL_1;
	else
		return pBD->getc_offset;

	return pBD->getc_offset--;
}

/*
* Purpose:
* Author: Seongyeop Jeong
* Versions:
* Called Functions:
* Parameters: Buffer* const pBD
* Return value: -1, getc_offset from Buffer struct 
* Algorithm: 1.
*/
short b_reset(Buffer * const pBD)
{
	pBD->getc_offset |= pBD->markc_offset;

	if (pBD == NULL || pBD->markc_offset < OFFSET_RESET)
		return RT_FAIL_1;
	else
		return pBD->getc_offset;
}

/*
* Purpose:
* Author: Seongyeop Jeong
* Versions:
* Called Functions:
* Parameters: Buffer* const pBD
* Return value: -1, getc_offset from buffer struct
* Algorithm: 1.
*/
short b_getcoffset(Buffer * const pBD)
{
	if (pBD == NULL)
		return RT_FAIL_1;

	return pBD->getc_offset;
}

/*
* Purpose:
* Author: Seongyeop Jeong
* Versions:
* Called Functions:
* Parameters: Buffer* const pBD
* Return value: -1, 0
* Algorithm: 1.
*/
int b_rewind(Buffer * const pBD)
{
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;

	if (pBD == NULL)
		return RT_FAIL_1;
	else
		return 0;
}

/*
* Purpose:
* Author: Seongyeop Jeong
* Versions:
* Called Functions:
* Parameters: Buffer* const pBD
* Return value: NULL, pointer of buffer struct 
* Algorithm: 1.
*/
char * b_location(Buffer * const pBD, short loc_offset)
{
	if (pBD == NULL)
		return NULL;
	else if (pBD->addc_offset <= loc_offset || loc_offset >= 0)
		return NULL;

	return pBD->cb_head + loc_offset; /* QUESTION */
}