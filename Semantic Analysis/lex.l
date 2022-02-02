%{
#include <stdio.h>
#include<stdlib.h>
#include <string.h>
#include "parser.tab.h"
#include "head.h"
int addToSynTab(char *str);
int Typeit(char* t);
int i = 0;
int number = 0;
int present = 0;
char type[10];
int change = 0;
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
"(" {printf("%s\n",yytext); change = 1; return yytext[0];}
")" {printf("%s\n",yytext); change = 1; return yytext[0];}
"{" {printf("%s\n",yytext); change = 1; return yytext[0];}
"}" {printf("%s\n",yytext); change = 1; return yytext[0];}
{numeric}+ {printf("%s\n",yytext); change = 1; return INTEGER_CONSTANT;}
{numeric}+"."{numeric}+ {printf("%s\n",yytext); change = 1; return FLOAT_CONSTANT;}
"float" {printf("%s\n",yytext); Typeit(yytext); change = 0; return FLOAT; }
";" {printf("%s\n",yytext); change = 1; return SEMICOLON;}
"," {printf("%s\n",yytext); change = 0; return COMMA;}
"int" {printf("%s\n",yytext); Typeit(yytext); change = 0; return INT; }
"char" {printf("%s\n",yytext); Typeit(yytext); change = 0; return CHAR; }
"=" {printf("%s\n",yytext); change = 1; return ASSIGN;}
"if" {printf("%s\n",yytext); change = 1; return IF;}
"else" {printf("%s\n",yytext); change = 1; return ELSE;}
"&&" {printf("%s\n",yytext); change = 1; return AND;}
"||" {printf("%s\n",yytext); change = 1; return OR;}
"!" {printf("%s\n",yytext); change = 1; return NOT;}
"==" {printf("%s\n",yytext); change = 1; return EQ;}
">=" {printf("%s\n",yytext); change = 1; return GE;}
"<=" {printf("%s\n",yytext); change = 1; return LE;}
"<" {printf("%s\n",yytext); change = 1; return LT;}
">" {printf("%s\n",yytext); change = 1; return GT;}
"!=" {printf("%s\n",yytext); change = 1; return NE;}
"while" {printf("%s\n",yytext); change = 1; return WHILE;}
"return" {printf("%s\n",yytext); change = 1; return RETURN;}
"+" {printf("%s\n",yytext); change = 1; return PLUS;}
"-" {printf("%s\n",yytext); change = 1; return MINUS;}
"*" {printf("%s\n",yytext); change = 1; return MULTIPLY;}
"/" {printf("%s\n",yytext); change = 1; return DIVISION;}
[_|a-z|A-Z][_|a-z|A-Z|0-9]* {int index = addToSynTab(yytext); change = 0; return ID;};

%%
int addToSynTab(char* str){
  int length_of_str = 0;
  while(str[length_of_str] != '\0'){
    length_of_str += 1;
  }
  int count = 0;
  for(int k = 0;k<number;k++){
      int count_symbol_table_entry = 0;
      while(symbol_table[k].name[count_symbol_table_entry] != '\0'){
        count_symbol_table_entry += 1;
      }
      for(int p = 0; p < count_symbol_table_entry; p++){
        if(str[p] == symbol_table[k].name[p]){
          count += 1;
        }
      }
      if(count == length_of_str){
        printf("str = %s\n",str);
        present = 1;
        count = 0;
        break;
      }
      count = 0;
  }
  if (present == 0){
      int length_of_t = 0;
      while(type[length_of_t] != '\0'){
        length_of_t += 1;
      }
      count = 0;
      for(int j = 0; j < length_of_str; j++)
      {
      symbol_table[i].name[j] = str[j];
      }
      if(change == 0){
          for(int j = 0; j < length_of_t; j++)
          {
          symbol_table[i].type[j] = type[j];
          }
          }
      else{
          symbol_table[i].type[0] = 'N';
          symbol_table[i].type[1] = 'O';
          symbol_table[i].type[2] = 'T';
          symbol_table[i].type[3] = ' ';
          symbol_table[i].type[4] = 'D';
          symbol_table[i].type[5] = 'E';
          symbol_table[i].type[6] = 'F';
          symbol_table[i].type[7] = 'I';
          symbol_table[i].type[8] = 'N';
          symbol_table[i].type[9] = 'E';
          symbol_table[i].type[10] = 'D';
      }
      i = i + 1;
      number += 1;
      }
      present = 0;
  printf("Printing Symbol table \n");
  for(int l = 0;l<number;l++){
    printf("%s %s \n",symbol_table[l].name,symbol_table[l].type);
  }
  printf("Printed Symbol table \n");
};

int Typeit(char* t){
  for(int j = 0; j < 10; j++)
  {
    type[i]= '\0';
  }
  int length_of_t = 0;
  while(t[length_of_t] != '\0'){
    length_of_t += 1;
  }
  for(int j = 0; j < length_of_t; j++)
  {
    type[j] = t[j];
  }
  return 0;
}
