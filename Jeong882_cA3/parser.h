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
#pragma once
#include "buffer.h"
#include "token.h"

#ifndef PARSER_H_
#define PARSER_H_
#endif

#define NO_ATTR -1
#define ELSE 0	
#define FALSE 1 
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE 9

extern int line;
extern Buffer * str_LTBL;
extern char* kw_table[];
static Token lookahead;
int synerrno;
/* functions */
extern Token malar_next_token(void);
void parser(void);
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe();
void gen_incode(char* prints);
void program(void);
void opt_statements(void);
void statements(void);
void statement(void);
void statements_prime(void);
void assignment_statement(void);
void assignment_expression(void);
void input_statement(void);
void variable_list(void);
void variable_identifier(void);
void variable_list_prime(void);
void output_statement(void);
void opt_variable_list(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_prime(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_prime(void);
void primary_arithmetic_expression(void);
void string_expression(void);
void string_expression_prime(void);
void primary_string_expression(void);
void iteration_statement(void);
void selection_statement(void);
void pre_condition(void);
void conditional_expression(void);
void logical_or_expression(void);
void logical_or_expression_prime(void);
void logical_and_expression(void);
void logical_and_expression_prime(void);
void relational_expression(void);
void primary_a_relational_expression(void);
void primary_s_relational_expression(void);
void primary_a_relational_expression_prime(void);
void primary_s_relational_expression_prime(void);
