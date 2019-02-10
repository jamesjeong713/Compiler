/*
* File Name		: buffer.c
* Compiler		: MS Visual Studio 2015
* Author		: Seongyeop Jeong, ID# 040885882
* Course		: CST8152 - Compiler, Lab Section: 012
* Assignment	: 1
* Date			: 2 October 2018
* Professor		: Svillen Ranev
* Purpose		: This file is to implement source code to parse the bitwise on the buffer
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
* Versions: 1.0
* Called Functions: calloc(), malloc(), free()
* Parameters: short init_capacity, char inc_factor, char o_mode
* Return value: pBD, NULL
* Algorithm:	1. allocate memory for one Buffer;
*				2. allocates memory for one dynamic character buffer;
*				3. sets the buffer operational mode indicator mode
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
	/* to avoid mistake, assigned unsigned char inc_factor to the variable */
	unsigned char unsignedIncFactor = (unsigned char)inc_factor;
	/* reset the flags to the dafulat flags */
	pB->flags &= DEFAULT_FALGS;
	///* allocates memory for one dynamic character buffer */
	//pB->cb_head = (char *)malloc(sizeof(char) * init_capacity);
	///* check for NULL */
	//if (pB->cb_head == NULL)
	//	return NULL;
	/* if init_capacity is smaller then maximum value, and it is greater than 0, 
	application has to launch. if it is not, abort and release the memory by using free. 
	then return null */
	if (init_capacity < SHRT_MAX && init_capacity >= 0)
	{
		/* allocates memory for one dynamic character buffer */
		pB->cb_head = (char *)malloc(sizeof(char) * init_capacity);
		/* check for NULL */
		if (pB->cb_head == NULL)
			return NULL;
		/* sets the buffer operational mode indicator mode 'f' */
		if (o_mode == 'f' || (unsignedIncFactor == INC_FACTOR_ZERO) || o_mode == 'f' && (unsignedIncFactor != INC_FACTOR_ZERO))
		{
			/* if init_capacity is 0, return NULL to avoid run-time error */
			if (init_capacity == 0)
				return NULL;
			/* fixed mode is 0 */
			pB->mode = O_MODE_ZERO;
			/* increment factor is 0 in fixed mode only */
			pB->inc_factor = INC_FACTOR_ZERO;
		}
		/* in operational mode 'a', unsigned inc_factor is greater than 1, and smaller than 255 */
		else if (o_mode == 'a' && (unsignedIncFactor >= INC_FACTOR_MIN) && (unsignedIncFactor <= INC_FACTOR_MAX))
		{
			/* 'a' mode as in number 1 */
			pB->mode = O_MODE_ONE;
			/* assign the unsigned increment factor to inc_factor of the buffer */
			pB->inc_factor = unsignedIncFactor;
		}
		/* in operational mode 'm', unsigned inc_factor is greater than 1, and smaller then 100 */
		else if (o_mode == 'm' && (unsignedIncFactor >= INC_FACTOR_MIN) && (unsignedIncFactor <= INC_FACTOR_HUNDRED))
		{
			/* 'a' mode as in number -1 */
			pB->mode = O_MODE_MINUS_ONE;
			/* unsigned inc_factor has to be assigned in inc_factor of the buffer */
			pB->inc_factor = unsignedIncFactor;
		}
		/* abort the buffer by using free function and return NULL. */
		else {
			/* free a pointer of the memory of the buffer firstly */
			free(pB->cb_head);
			/* then free the struct of the buffer */
			free(pB);
			return NULL;
		}
	}
	/* otherwise, abort and release memory */
	else
	{
		free(pB->cb_head);
		free(pB);
		return NULL;
	}
	/* if allocation memory fails, abort and release it */
	if (pB->cb_head == NULL)
	{
		free(pB->cb_head);
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
* Versions: 1.0
* Called Functions: realloc()
* Parameters: pBuffer const pBD, char symbol
* Return value: pBuffer, NULL
*
* Algorithm: 1. if the buffer is operational and not full, add the character to buffer
*			 2. if the buffer is full, resize the buffer depending on the mode (a, m, f)
*			 3. if the operational mode, function returns different value
*				operational mode indicator mode
*/
pBuffer b_addc(pBuffer const pBD, char symbol)
{
	/* to assign the capacity because it depends on the mode */
	unsigned short newCapacity = 0;
	/* to get available space */
	short available_space = 0;
	/* this increment variable is for mode multiplicative */
	unsigned short newInc = 0;
	/* reallocation memory to this variable temporary. 
	it will be assigned to pBD->cb_head after some conditions are satisfied*/
	char* temp;
	/* to avoid run-time error */
	if (pBD == NULL)
		return NULL;
	/* flags field reset the allocation */
	pBD->flags &= RESET_R_FLAG;
	/* this 'if statement' is to expand the capacity. 
	if there is no any space, do progress depending on mode type */
	if (pBD->addc_offset >= pBD->capacity)
	{
		/* if mode is 0, it has to return null because fixed buffer doesn't expand the capacity */
		if (pBD->mode == O_MODE_ZERO)
			return NULL;
		/* mode is 1 (additive) */
		else if (pBD->mode == O_MODE_ONE)
		{
			/* if the result is positive and does not exceed minus 1, this function proceeds*/
			if (pBD->capacity < 0 || pBD->capacity >= MAXIMUM_ALLOWED_NUMBER)
				return NULL;

			/* assign the capacity adding inc_factor to newCapcity to expand buffer. 
			new capacity has to be unsgiend short so that it has to be casted to unsigned in calculation */
			newCapacity = (unsigned short)pBD->capacity + (unsigned char)pBD->inc_factor;
			/* if the newCapacity is greater than maximum number -1, it has to be stop */
			if (newCapacity >= MAXIMUM_ALLOWED_NUMBER)
			{
				/* so that maximum number-1 is assigned to new capacity */
				newCapacity = MAXIMUM_ALLOWED_NUMBER;
				/* then return NULL, to display and let main program know it is full */
				return NULL;
			}
		} /* end of the if function: mode 1 */
		/* if the mode -1, incearse the current capacity of the buffer to a new capacity*/
		else if (pBD->mode == O_MODE_MINUS_ONE)
		{
			/* if current capacity cannot be incremented anymore
			because it has reached the maximum capacity of the buffer,
			the function returns NULL */
			if (pBD->capacity == MAXIMUM_ALLOWED_NUMBER)
				return NULL;
			/* to get available space, current capacity has to be deducted from maximum number - 1 */
			available_space = MAXIMUM_ALLOWED_NUMBER - pBD->capacity;
			/* after calculating the increments, it has to be casted to unsigned to make them as positive number */
			newInc = (unsigned short)(available_space * (unsigned char)pBD->inc_factor / 100.0f);
			/* validation for new capacity: total value does not have to be greater than maximum number -1 
			and also new increment must not 0 */
			if (pBD->capacity + newInc < MAXIMUM_ALLOWED_NUMBER && newInc != 0)
				/* if so, assign to new capacity to expand the buffer */
				newCapacity = (unsigned short)pBD->capacity + newInc;
			/* it is to avoid over flow from SHRT_MAX -1 */
			else if (pBD->capacity < MAXIMUM_ALLOWED_NUMBER)
				newCapacity = MAXIMUM_ALLOWED_NUMBER;
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
			/* whenever program reallocation, the memory location is changed so that it has to be set flags */
			if (temp != pBD->cb_head)
				pBD->flags |= SET_R_FLAG; /* sets r_flag bit to 1 */
		}
		pBD->cb_head = temp;
		pBD->capacity = newCapacity;
	}
	/* if there is enough space, adds the char symbol to the bufer content*/
	if (pBD->addc_offset < pBD->capacity)
		pBD->cb_head[pBD->addc_offset++] = symbol;
	/* if not, return null to pass to main program */
	else
		return NULL;
	return pBD;
}

/*
* Purpose: This function is to reset the offsets to clean the memory locations currently buffer struct
such as addc, getc, and markc.
* Author: Seongyeop Jeong
* Versions: 1.0
* Called Functions: none
* Parameters: Buffer* const pBD
* Return value: -1, 1
* Algorithm: 1. when the code is reached to the b_clear method, this method is to reset the offsets 
*			 2. if there is no run-time error, it returns TRUE which is 1 (not 0)
*/
int b_clear(Buffer * const pBD) {
	/* If run-time error ocurrs, returns -1 */
	if (pBD == NULL)
		return RT_FAIL_1;
	/* reinitialize the data members of buffer to 0 */
	pBD->addc_offset = OFFSET_RESET;
	pBD->getc_offset = OFFSET_RESET;
	pBD->markc_offset = OFFSET_RESET;
	/* except for 0 (false), every numbers are TRUE, here's true is defined to 1 */
	return TRUE;
}

/*
* Purpose: This function is to abort and release memory of the Buffer struct when this application is done.
* Author: Seongyeop Jeong
* Versions: 1.0
* Called Functions: free()
* Parameters: Buffer* const pBD
* Return value: 
* Algorithm: 1. if pointer of the buffer is not null, memory locations have to be released. 
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
* Versions: 1.0
* Called Functions: none
* Parameters: Buffer* const pBD
* Return value: -1, 1, 0
* Algorithm: 1. check the run-time error. if there is, return -1.
*			 2. check if addc_offset and capacity are equal, return true(every number except for 0).
*			 3. if not, return false(0).
*/
#ifndef B_FULL
int b_isfull(Buffer *const pBD)
{
	if (pBD == NULL)
		return RT_FAIL_1;
	if (pBD->addc_offset == pBD->capacity)
		return TRUE;
	else
		return FALSE;
}
#endif

/*
* Purpose: This function is to returns current limit size of buffer to display it.
* Author: Seongyeop Jeong
* Versions: 1.0
* Called Functions: none
* Parameters: Buffer* const pBD
* Return value: addc_offset
* Algorithm: 1. If there is no run-time error, return addc_offset from buffer struct.
*/
short b_limit(Buffer *const pBD)
{
	if (pBD == NULL)
		return RT_FAIL_1;

	return pBD->addc_offset;
}

/*
* Purpose: This function is to return current capacity on the buffer to display it.
* Author: Seongyeop Jeong
* Versions: 1.0
* Called Functions: none
* Parameters: Buffer* const pBD
* Return value: -1, capacity
* Algorithm: 1. if there is no run-time error, it returns capacity.
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
* Versions: 1.0
* Called Functions: none
* Parameters: Buffer const pBD, short mark
* Return value: -1, markc_offset
* Algorithm: 1. if there is no run-time error, do process.
*			 2. if mark value is smaller than 0 then it is greater than addc_offset, it returns -1
*			 3. then mark assigns to markc_offset of buffer 
*/
short b_mark(pBuffer const pBD, short mark)
{
	if (pBD == NULL)
		return RT_FAIL_1;

	if (mark < 0 || mark > pBD->addc_offset)
		return RT_FAIL_1;

	return pBD->markc_offset = mark;
}

/*
* Purpose: This function is to set up what mode this application run with.
* Author: Seongyeop Jeong
* Versions: 1.0
* Called Functions: none
* Parameters: Buffer* const pBD
* Return value: -1, 1, 0
* Algorithm: 1. if there is no run-time error, it returns mode value. 
*/
int b_mode(Buffer * const pBD)
{
	/* run-time error, this function notify the calling function about the failure */
	if (pBD == NULL)
		return RT_FAIL_2;

	return pBD->mode;
}

/*
* Purpose: This function is to return the increment factor which is unsigned value.(non-negative value)
* Author: Seongyeop Jeong
* Versions: 1.0
* Called Functions: none
* Parameters: Buffer* const pBD
* Return value: 0x100, inc_factor
* Algorithm: 1. if there is no run-time error, do process.
*			 2. returns inc factor which is unsigned char.
*/
size_t b_incfactor(Buffer * const pBD)
{
	if (pBD == NULL)
		return ERR_INC_FACTOR;

	return (unsigned char)pBD->inc_factor;
}

/*
* Purpose: This function is to return the number of characters on the buffer by getting a character .
* Author: Seongyeop Jeong
* Versions: 1.0
* Called Functions: fgetc(), feof(), ungetc(), b_addc(), printf()
* Parameters: FILE* const fi, Buffer* const pBD
* Return value: -1, -2, charNum
* Algorithm: 1. check if there is run-time error
*			 2. each character will be added to the buffer through b_addc method until the end of the file
*			 3. if the b_addc method's return is not null,
*				count up the number of characters how many character does it work with the method
*			 4. if the b_addc method's return is null,
*				print it what the last character was.
*/
int b_load(FILE * const fi, Buffer * const pBD)
{
	/* actually this variable is not buffer. it is to get a character from fgetc */
	char buffer;
	int charNum = 0;

	if (fi == NULL || pBD == NULL)
		return RT_FAIL_1;

	while (1) /* repeat until the standard macro feof(fi)detects EOF */
	{
		/* fgetc function is to get a character */
		buffer = (char)fgetc(fi);

		/* feof function: tests the end-of-file indicator,
		it will be returned non-zero when end-of-file indicator
		associated with the stream is set */
		if (feof(fi))
			break;
		/* check the buffer by passing the buffer and a character */
		if (b_addc(pBD, buffer) == NULL)
		{
			/* the last character will be pushed on the buffer stream */
			ungetc(buffer, fi);
			/* print out the character which is last one on the stream */
			printf("The last character read from the file is: %c %d\n", buffer, buffer);
			return LOAD_FAIL;
		}
		/* if b_addc != null, count character on the buffer by 1 */
		charNum++;
	}
	return (int)charNum;
}

/*
* Purpose: This function is to check if the buffer's addc_offset is 0 or not.
* Author: Seongyeop Jeong
* Versions: 1.0
* Called Functions: none
* Parameters: Buffer* const pBD
* Return value: -1, 1, 0
* Algorithm: 1. check if the run-time erorr is happened
*			 2. check if the addc_offset is 0, then return 1.
*			 3. check if any else situation will be return 0.
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
* Versions: 1.0
* Called Functions: none
* Parameters: Buffer* const pBD
* Return value: -2, 0, pointer of the buffer
* Algorithm: 1. in b_print method, b_getc will be checked 
*				until it will catch them if getc and addc are equal.
*			 2. when the getc and addc are equal, 
*				set the EOB to 1 bit, and returns 0.
*			 3. if not, reset the eob to 0 bit. 
*				Then next pointer of getc_offset of the buffer will be returned
*/
char b_getc(Buffer * const pBD)
{
	if (pBD == NULL)
		return RT_FAIL_2;
	/* if the getc and addc are equal, the current buffer has been reached on the buffer limit */
	if (pBD->getc_offset == pBD->addc_offset)
	{
		/* set to 1 bit. End of the buffer is 1 because the buffer is full */
		pBD->flags |= SET_EOB;
		return 0;
	}
	/* reset the eob to 0 bit so that it has not been reached current limit on the buffer 
	then do progress to the next position of the buffer */
	else
		pBD->flags &= RESET_EOB;
	return pBD->cb_head[pBD->getc_offset++];
}

/*
* Purpose: This function is to return value to check in b_print method if it is end of the buffer
* Author: Seongyeop Jeong
* Versions: 1.0
* Called Functions: none
* Parameters: Buffer* const pBD
* Return value: -1, return flags which is checked 0x0001
* Algorithm: 1. make a new variable to check if it is end of buffer.
*			 2. then returns the variable to the b_print method
*/
int b_eob(Buffer * const pBD)
{
	/* to avoid run-time error */
	if (pBD == NULL)
		return RT_FAIL_1;
	/* to check if it is end of the buffer for method of b_print */
	short checkEob;
	checkEob = pBD->flags & CHECK_EOB;

	return checkEob;
}

/*
* Purpose: This function is to print out the characters on the buffer to the standard output. 
* Author: Seongyeop Jeong
* Versions: 1.0
* Called Functions: b_getc(), b_eob(), printf()
* Parameters: Buffer* const pBD
* Return value: -1, 0, getc_offset
* Algorithm: 
*			 1. if addc_offset of the buffer struct is 0, returns 0.
*			 2. if not, it gets a character by using b_getc method 
*				then prints out until it reachs to end of the buffer.
*			 3. it returns getc_offset of buffer struct which is from b_getc method. 
*/
int b_print(Buffer * const pBD)
{
	/* to get a character by using the b_getc method */
	char buffer;
	/* avoid the run-time error */
	if (pBD == NULL)
		return RT_FAIL_1;
	/* if the addc_offset of the buffer 0, print out empty buffer, then returns 0 */
	if (pBD->addc_offset == 0)
	{
		printf("Empty buffer!");
		printf("\n");
		return 0;
	}

	while (1)
	{
		/* get a character */
		buffer = b_getc(pBD);
		/* if it is end of the buffer, break. */
		if (b_eob(pBD))
			break;
		printf("%c", buffer);
	}
	printf("\n");

	return pBD->getc_offset;
}

/*
* Purpose: This function is to compact the space for one more character when it is full. 
* Author: Seongyeop Jeong
* Versions: 1.0
* Called Functions: realloc(), 
* Parameters: Buffer* const pBD, char symbol
* Return value: -1, NULL, Buffer
* Algorithm: 1. realloc memory location into next position on the buffer
*			 2. if the temp is not matched to head of the buffer, set the r_flag
*/
Buffer * b_compact(Buffer * const pBD, char symbol)
{
	/* declare the pointer temp to reallocation memory temporary */
	char* temp;
	/* avoid the run-time error */
	if (pBD == NULL)
		return NULL; 
	pBD->flags &= RESET_R_FLAG;
	/* uses realloc to adjust the new capacity */
	temp = (char*)realloc(pBD->cb_head, pBD->addc_offset + 1);
	/* avoid the run-time error if the temp fail the reallocation */
	if (temp == NULL)
		return NULL;
	/* after reallocation, it is possible to be changed the memory location
	so that it must set the r_flag bit 1 */
	if (temp != pBD->cb_head)
		pBD->flags |= SET_R_FLAG;
	/* to the pointer of the buffer, temp has to be assigned */
	pBD->cb_head = temp;
	/* if there is enough space in buffer, add 1 to offset */
	if (pBD->capacity <= SHRT_MAX - 1)
		pBD->capacity = pBD->addc_offset + 1;
	/* the function adds the symbol to the end of the char buffer */
	pBD->cb_head[pBD->addc_offset++] = symbol;
	

	return pBD;
}

/*
* Purpose: This function is to set up the r_flag with bitwise operator.
* Author: Seongyeop Jeong
* Versions: 1.0
* Called Functions: none
* Parameters: Buffer* const pBD
* Return value: -1, flags which is set r_flag to 1
* Algorithm: 1. return reseted r_flag to the flag of buffer
*/
char b_rflag(Buffer* const pBD)
{
	if (pBD == NULL)
		return RT_FAIL_1;

	return pBD->flags &= SET_R_FLAG;  
}

/*
* Purpose: This function is to decrements getc_offset by 1.
* Author: Seongyeop Jeong
* Versions: 1.0
* Called Functions: none
* Parameters: Buffer* const pBD
* Return value: decremented getc_offset of the buffer
* Algorithm: 1. return decremented getc_offset by 1
*/
short b_retract(Buffer * const pBD)
{
	if (pBD == NULL)
		return RT_FAIL_1;
	/* decrement getc_offset */
	return pBD->getc_offset--;
}

/*
* Purpose: This function is to reset the getc_offset to the current markc_offset.
* Author: Seongyeop Jeong
* Versions: 1.0
* Called Functions: none
* Parameters: Buffer* const pBD
* Return value: -1, getc_offset from Buffer struct
* Algorithm: 1. reset the getc_offset to markc_offset
*/
short b_reset(Buffer * const pBD)
{
	pBD->getc_offset = pBD->markc_offset;
	/* to avoid run-time error */
	if (pBD == NULL || pBD->markc_offset < OFFSET_RESET)
		return RT_FAIL_1;
	else
		return pBD->getc_offset;
}

/*
* Purpose: This function is to get the current getc_offsets
* Author: Seongyeop Jeong
* Versions: 1.0
* Called Functions: none
* Parameters: Buffer* const pBD
* Return value: -1, getc_offset from buffer struct
* Algorithm: 1. return the current getc_offset of the buffer
*/
short b_getcoffset(Buffer * const pBD)
{
	if (pBD == NULL)
		return RT_FAIL_1;
	/* return the getc_offset to get the current offsets */
	return pBD->getc_offset;
}

/*
* Purpose: This function is to set the offsets to 0 such as getc and markc.
* Author: Seongyeop Jeong
* Versions: 1.0
* Called Functions: none
* Parameters: Buffer* const pBD
* Return value: -1, 0
* Algorithm: 1. assign 0 to getc and markc offsets. 
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
* Purpose: This function is to return a pointer of the buffer by adding location of the character buffer
*		   to get the begining of the character array. However, it has not been used in this assignment.
* Author: Seongyeop Jeong
* Versions: 1.0
* Called Functions: none
* Parameters: Buffer* const pBD
* Return value: NULL, pointer of buffer struct
* Algorithm: 1. return a pointer to a location of the chacracter buffer
*/
char * b_location(Buffer * const pBD, short loc_offset)
{
	if (pBD == NULL)
		return NULL;
	/* error check if the range of addc_offset is less than loc_offset, and greater than 0 */
	else if (pBD->addc_offset <= loc_offset && loc_offset >= 0)
		return NULL;
	/* to get the location, add the loc_offset to the cb_head */
	return pBD->cb_head + loc_offset; 
}