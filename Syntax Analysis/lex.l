%{
#include <stdio.h>
#include<stdlib.h>
#include <string.h>
#include "parser.tab.h"
%}
numeric [0-9]
alphabet [a-zA-Z]
alphanumeric {numeric}|{alphabet}
underscore [_]
underscore_alpha {underscore}|{alphabet}
underscore_alpha_numeric {underscore}|{alphabet}|{numeric}
%%
[ \t]+ ;
\n ;
"(" {printf("%s\n",yytext); return yytext[0];}
")" {printf("%s\n",yytext); return yytext[0];}
"{" {printf("%s\n",yytext); return yytext[0];}
"}" {printf("%s\n",yytext); return yytext[0];}
{numeric}+ {printf("%s\n",yytext); return INTEGER_CONSTANT;}
{numeric}+"."{numeric}+ {printf("%s\n",yytext); return FLOAT_CONSTANT;}
"float" {printf("%s\n",yytext); return FLOAT;}
";" {printf("%s\n",yytext); return SEMICOLON;}
"," {printf("%s\n",yytext); return COMMA;}
"int" {printf("%s\n",yytext); return INT;}
"char" {printf("%s\n",yytext); return CHAR;}
"=" {printf("%s\n",yytext); return ASSIGN;}
"if" {printf("%s\n",yytext); return IF;}
"else" {printf("%s\n",yytext); return ELSE;}
"&&" {printf("%s\n",yytext); return AND;}
"||" {printf("%s\n",yytext); return OR;}
"!" {printf("%s\n",yytext); return NOT;}
"==" {printf("%s\n",yytext); return EQ;}
">=" {printf("%s\n",yytext); return GE;}
"<=" {printf("%s\n",yytext); return LE;}
"<" {printf("%s\n",yytext); return LT;}
">" {printf("%s\n",yytext); return GT;}
"!=" {printf("%s\n",yytext); return NE;}
"while" {printf("%s\n",yytext); return WHILE;}
"return" {printf("%s\n",yytext); return RETURN;}
{underscore_alpha}{underscore_alpha_numeric}* {printf("%s\n",yytext); return ID;}
"+" {printf("%s\n",yytext); return PLUS;}
"-" {printf("%s\n",yytext); return MINUS;}
"*" {printf("%s\n",yytext); return MULTIPLY;}
"/" {printf("%s\n",yytext); return DIVISION;}
