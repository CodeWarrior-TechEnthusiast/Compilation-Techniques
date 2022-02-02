%{
#include "advanced_parser.tab.h"
#include "head.h"
#include <stdio.h>
#include <string.h>
int  yylex(void);
int  yyerror(char* s);
void write_init_lines();
void write_closing_lines();
int  addToSymTab(char* t, char* s);
int  symTabLookUp(char* s);
int  addConditionalExp(double expVal1, char *expType1, int irName1, double expVal2, char *expType2, int irName2, char op);
void backpatch(int list[], int count, int label);
char curType[MAX];
char file_buffer[MAX*MAX*MAX]; 
int buffer_count = 0;
int count = 1;
int instruction_count = 100;
int line_number = 0;
%}

%union{
    int    ival;
    char   sval[10];
    double dval;
    struct expStruct
    {
        double expVal;;
        char   expType[30];
        int    irName; 
        struct bList
        {
            int ele[10];
            int count;
        } trueList ,falseList;
    }eval;
    struct statementStruct
    {
        struct slist
        {
            int ele[10];
            int count;
        } nextList;
    }stmtVal;

};


%token COMMA INT_CONST SEMICOLON LOGOP RELOP OR AND NOT IF WHILE EQ 
%token <sval> ID 
%type  <eval> EXP TERM FACTOR relExp logExp 
%type  <stmtVal> stmt assignStmt ifStmt whileStmt stmtList 
%type  <ival> M
%token <ival> INT_CONST 
%token <dval> DOUBLE_CONST
%token INT DOUBLE
%start prog
%%
prog : { 
           symbTabEntryCount = 0; 
           write_init_lines(); 
       } 
       funcDef 
       { 
           write_closing_lines(); 
       }
funcDef : TYPE_CONST ID '(' argList ')' '{' declList 
	{ } 
        stmtList '}' {}
argList : arg ',' arg {} | arg {} | ; 
arg	: TYPE_CONST ID {} 
declList : declList  decl {} | decl {}
decl : TYPE_CONST varList SEMICOLON 
        { 
            //printf("Detected declaration\n"); 
        }
TYPE_CONST : INT 
	   { 
               strcpy(curType, "int"); 
               //printf("Detected int\n"); 
           } 
	   | DOUBLE 
           { 
               strcpy(curType, "double"); 
               //printf("Detected double\n");
           }
varList : ID 
	   { 
               int sym_tab_loc = addToSymTab(curType, $1); 
               char c[2] = "id";              
         	if(curType[0] == c[0])
         	{
         		char temp[100];
 			sprintf(temp,"%s%d = alloca i32, align 4\n","%",count);
 			strcat(file_buffer,temp);
 			line_number += 1;
         	}
         	if(curType[0] == c[1]){
         		char temp[100];
         		sprintf(temp,"%s%d = alloca double, align 8\n","%",count);
         		strcat(file_buffer,temp);
         		line_number += 1;
         	}
         	count += 1;
           } 
           COMMA varList 
           { 
           }
	| ID 
           { 
             int sym_tab_loc = addToSymTab(curType, $1); 
               char c[2] = "id";              
         	if(curType[0] == c[0])
         	{
         		char temp[100];
 			sprintf(temp,"%s%d = alloca i32, align 4\n","%",count);
 			strcat(file_buffer,temp);
 			line_number += 1;
         	}
         	if(curType[0] == c[1]){
         		char temp[100];
         		sprintf(temp,"%s%d = alloca double, align 8\n","%",count);
         		strcat(file_buffer,temp);
         		line_number += 1;
         	}
         	count += 1;
           }
stmtList : stmtList M stmt 
	 {
             //backpatch($1.nextList.ele,1, $2);
             //$1.nextList.count += 1;
             /* Save the ele and count fields of $3 to $1 */
         }
	 | stmt;
stmt : assignStmt { $$ = $1; }
      | ifStmt { $$ = $1; 
      //printf("I am here - %d\n",$1.nextList.ele[0]);
             backpatch($1.nextList.ele,$1.nextList.count,count - 1);
      }
      | whileStmt { } ;
assignStmt : ID '=' EXP SEMICOLON 
	   {    
               if((!strcmp($3.expType, "int_const") || !strcmp($3.expType, "int")) )
               {
               	
                   	if(!strcmp($3.expType, "int_const")){
                   		int value = $3.expVal; 
                   		char temp[100];
                   		sprintf(temp,"store i32 %d, i32* %s%d, align 4\n",value,"%",symTabLookUp($1) + 1);
                   		strcat(file_buffer,temp);
                   		line_number += 1;
                   	}
                   	else{
                   		char temp[100];
                   		sprintf(temp,"store i32 %s%d, i32* %s%d, align 4\n","%",count -1,"%",symTabLookUp($1) + 1);
                   		strcat(file_buffer,temp);
                   		line_number += 1;
                   	}
                   	
               }
               if(!strcmp(symbol_table[symTabLookUp($1) + 1].type,"double"))
               {
               	
                   	if(!strcmp($3.expType, "double_const")){
                   		char temp[100];
                   		sprintf(temp,"store double %f, double* %s%d, align 8\n",$3.expVal,"%",symTabLookUp($1) + 1);
                   		strcat(file_buffer,temp);
                   		line_number += 1;
                   	}
                   	else if(!strcmp($3.expType, "int_const")){
                   		char temp[100];
                   		double value = $3.expVal;
                   		sprintf(temp,"store double %f, double* %s%d, align 8\n",value,"%",symTabLookUp($1) + 1);
                   		strcat(file_buffer,temp);
                   		line_number += 1;
                   	}
                   	else{
                   		char temp[100];
                   		sprintf(temp,"store double %s%d, double* %s%d, align 8\n","%",count -1,"%",symTabLookUp($1) + 1);
                   		strcat(file_buffer,temp);
                   		line_number += 1;
                   	}
                   	
               }
               $$.nextList.count = 0;
           }
EXP : EXP '+' TERM 
    { 
        strcpy($$.expType, $1.expType); 
        int value = $3.expVal;
        if(!strcmp($1.expType, "int"))
        {
            if(!strcmp($3.expType, "int_const")){
            	char temp[100];
		sprintf(temp,"%s%d = add nsw i32 %s%d , %d\n","%",count,"%",count-1,value);
		strcat(file_buffer,temp);
		count += 1;
		line_number += 1;
		}
	    else{
	    	char temp[100];
	    	sprintf(temp,"%s%d = add nsw i32 %s%d , %s%d\n","%",count,"%",count-2,"%",count-1);
	    	count += 1;
	    	line_number += 1;
	    	strcat(file_buffer,temp);
	    }
        }
        if(!strcmp($1.expType, "double"))
        {
            if(!strcmp($3.expType, "double")){
            char temp[100];
		    sprintf(temp,"%s%d = fadd double %s%d , %s%d\n","%",count,"%",count-2,"%",count-1);
		    strcat(file_buffer,temp);
		    count += 1;
		    line_number += 1;
		    
		    }
	    else{
	    char temp[100];
		    sprintf(temp,"%s%d = fadd double %s%d , %f\n","%",count,"%",count-1,$3.expVal);
		    strcat(file_buffer,temp);
		    count += 1;
		    line_number += 1;
	    }
        }
    }
    | EXP '-' TERM 
    { 
        strcpy($$.expType, $1.expType); 
    }
    | TERM 
     { 
         strcpy($$.expType, $1.expType); 
         $$.expVal = $1.expVal;
         $$.irName = $1.irName;
     } 
TERM : TERM '*' FACTOR 
     { 
         strcpy($$.expType, $1.expType); 
        int value = $3.expVal;
        if(!strcmp($1.expType, "int"))
        {
            if(!strcmp($3.expType, "int_const")){
            char temp[100];
		sprintf(temp,"%s%d = mul nsw i32 %s%d , %d\n","%",count,"%",count-1,value);
		count += 1;
		strcat(file_buffer,temp);
		line_number += 1;
		}
	    else{
	    char temp[100];
	    	sprintf(temp,"%s%d = mul nsw i32 %s%d , %s%d\n","%",count,"%",count-2,"%",count-1);
	    	count += 1;
	    	strcat(file_buffer,temp);
	    	line_number += 1;
	    }
        }
        if(!strcmp($1.expType, "double"))
        {
            if(!strcmp($3.expType, "double")){
            char temp[100];
		    sprintf(temp,"%s%d = fmul double %s%d , %s%d\n","%",count,"%",count-2,"%",count-1);
		    count += 1;
		    strcat(file_buffer,temp);
		    line_number += 1;
		    }
	    else{
	    	    char temp[100];
		    sprintf(temp,"%s%d = fmul double %s%d , %f\n","%",count,"%",count-1,$3.expVal);
		    count += 1;
		    strcat(file_buffer,temp);
		    line_number += 1;
	    }
        }
     }
     | TERM '/' FACTOR 
     { 
     }
     | FACTOR 
     { 
           strcpy($$.expType, $1.expType);
           $$.expVal = $1.expVal;
           $$.irName = $1.irName;
     }
FACTOR : ID 
       { 
           strcpy($$.expType, symbol_table[symTabLookUp($1)].type); 
           char c[2] = "id";              
         	if($$.expType[0] == c[0])
         	{
         	char temp[100];
 			sprintf(temp,"%s%d = load i32, i32* %s%d, align 4\n","%",count,"%",symTabLookUp($1)+1);
 			strcat(file_buffer,temp);
 			line_number += 1;
         	}
         	if($$.expType[0] == c[1]){
         	char temp[100];
         		sprintf(temp,"%s%d = load double, double* %s%d, align 8\n","%",count,"%",symTabLookUp($1)+1);
         		strcat(file_buffer,temp);
         		line_number += 1;
         	}
           count += 1;
       } 
       | INT_CONST 
         { 
             strcpy($$.expType, "int_const"); 
             $$.expVal = $1; 
         }
       | DOUBLE_CONST 
         { 
             strcpy($$.expType, "double_const"); 
             $$.expVal = $1; 
         }
ifStmt : IF'('logExp')' M '{'stmtList '}' 
       { 
           //printf("I am here - %d\n",$3.trueList.ele[0]);
           //printf("I am here - %d\n",$3.trueList.count);
           int a = $$.nextList.count;
           int b = $3.falseList.count;
           int c = $7.nextList.count;
           for(int i = 0; i <  b;i++){
           	$$.nextList.ele[a] = $3.falseList.ele[i];
           	a = a+1;
           }
           for(int i = 0; i <  c;i++){
           	$$.nextList.ele[a] = $7.nextList.ele[i];
           	a = a+1;
           }
           //printf("Value of a = %d",a);
           $$.nextList.count = a;
                      //printf("I am testing - %d",$3.trueList.count);
           backpatch($3.trueList.ele,$3.trueList.count, $5 - 1);
           /* Save logExp.falseList and $7.nextList to $$.nextList*/
           char temp[100];
           sprintf(temp,"br label %s%d\n","%",count);
	    strcat(file_buffer,temp);
           line_number += 1;
           sprintf(temp,"\n");
           strcat(file_buffer,temp);
           sprintf(temp,"%d:                                               ; preds = %s%d, %s0\n",count,"%",$5 - 1,"%");
	   strcat(file_buffer,temp);
           line_number += 1;
           count += 1;
       }
logExp : relExp { $$ = $1; };
relExp : EXP '>' EXP 
     { 
         strcpy($$.expType, "bool"); 
         addConditionalExp($1.expVal, $1.expType, $1.irName, 
                           $3.expVal, $3.expType, $3.irName, '>'); 
         $$.trueList.ele[$$.trueList.count] = line_number - 2;
         $$.falseList.ele[$$.falseList.count] = line_number - 2;
         $$.trueList.count += 1;
         $$.falseList.count += 1;
     };
relExp : EXP '<' EXP 
      { 
          strcpy($$.expType, "bool"); 
          printf("Not supported\n");
      };
relExp : EXP EQ EXP 
       { 
           strcpy($$.expType, "bool"); 
           printf("Not supported\n");
       };
relExp : EXP RELOP EXP 
       { 
           strcpy($$.expType, "bool"); 
           printf("Not supported\n");
       };
whileStmt : WHILE'('logExp')''{'stmtList'}' 
       { 
           printf("Not supported\n");
       }; 

M : { /* Save the next label */
	$$ = count;
	}
%%

void write_init_lines()
{	
	char temp[200];
	sprintf(temp,"; ModuleID = 'prog.c'\n");
	strcat(file_buffer,temp);
	sprintf(temp,"source_filename = \"prog.c\"\n");
	strcat(file_buffer,temp);
	sprintf(temp,"target datalayout = \"e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128\"\n");
	strcat(file_buffer,temp);
	sprintf(temp,"target triple = \"x86_64-pc-linux-gnu\"\n");
	strcat(file_buffer,temp);
	sprintf(temp,"; Function Attrs: noinline nounwind optnone uwtable\n");
	strcat(file_buffer,temp);
	sprintf(temp,"define dso_local i32 @main() #0 {\n");
	strcat(file_buffer,temp);
}

void write_closing_lines()
{
	char temp[200];
	sprintf(temp,"ret i32 0\n}\n");
	strcat(file_buffer,temp);
	sprintf(temp,"attributes #0 = { noinline nounwind optnone uwtable \"correctly-rounded-divide-sqrt-fp-math\"=\"false\" \"disable-tail-calls\"");
	strcat(file_buffer,temp);
	sprintf(temp,"=\"false\" \"frame-pointer\"=\"all\" \"less-precise-fpmad\"=\"false\" \"min-legal-vector-width\"=\"0\" \"no-infs-fp-math\"");
	strcat(file_buffer,temp);
	sprintf(temp,"=\"false\" \"no-jump-tables\"=\"false\" \"no-nans-fp-math\"=\"false\" \"no-signed-zeros-fp-math\"=\"false\" \"no-trapping-math\"=\"false\""); 
	strcat(file_buffer,temp);
	sprintf(temp,"\"stack-protector-buffer-size\"=\"8\" \"target-cpu\"=\"x86-64\" \"target-features\"=\"+cx8,+fxsr,+mmx,+sse,+sse2,+x87\" \"unsafe-fp-math\"");
	strcat(file_buffer,temp);
	sprintf(temp,"=\"false\" \"use-soft-float\"=\"false\"}");
	strcat(file_buffer,temp);
	sprintf(temp,"!llvm.module.flags = !{!0}");
	strcat(file_buffer,temp);
	sprintf(temp,"!llvm.ident = !{!1}");
	strcat(file_buffer,temp);
	sprintf(temp,"!0 = !{i32 1, !\"wchar_size\", i32 4}");
	strcat(file_buffer,temp);
	sprintf(temp,"!1 = !{!\"clang version 10.0.0-4ubuntu1\"}");
	strcat(file_buffer,temp);
    printf("%s",file_buffer);
}

int addToSymTab(char *cur_type, char *s)
{
int length_of_cur_type = 0;
	int length_of_s = 0; 
	while(cur_type[length_of_cur_type] != '\0'){
		length_of_cur_type += 1;
	}
	while(s[length_of_s] != '\0'){
		length_of_s += 1;
	}
	for(int j = 0; j < length_of_cur_type; j++){
		symbol_table[symbTabEntryCount].type[j] = cur_type[j];
	} 
	for(int j = 0; j < length_of_s; j++){
		symbol_table[symbTabEntryCount].name[j] = s[j];
	} 
	symbTabEntryCount += 1;
	return symbTabEntryCount-1;
}

int symTabLookUp(char *s)
{
	for(int i = 0;i < symbTabEntryCount;i++){
    	int counter = 0;
    	int length_of_s = 0; 
    	while(s[length_of_s] != '\0'){
		length_of_s += 1;
	}
	for(int j = 0;j < length_of_s;j++){
		if(s[j] == symbol_table[i].name[j]){
			counter += 1;
		}
	}
	if(counter == length_of_s){
		return i;
	}
    }
    return 0;
}


int addConditionalExp(double expVal1, char *expType1, int irName1, double expVal2, char *expType2, int irName2, char op)
{
    if(op == '>')
    {
        if((!strcmp(expType1, "int")) && (!strcmp(expType2, "int")))
        {	
        	  int count1 = 0;
        	  char temp[100];
        	  char temp2[100];
		  sprintf(temp,"%s%d = icmp sgt i32 %s%d, %s%d\n","%",count,"%",count-2,"%",count-1);
		  strcat(file_buffer,temp);
		  line_number += 1;
		  sprintf(temp2,"br i1 %s%d, label %s%s, label %s%s\n","%",count,"%","??","%","??");
		  strcat(file_buffer,temp2);
		  line_number += 1;
		  count1 = count1 + 1;
		  sprintf(temp,"\n");
           strcat(file_buffer,temp);
		  sprintf(temp,"%d:                                               ; preds = %s0\n",count + 1,"%");
		  strcat(file_buffer,temp);
		  line_number += 1;
		  count = count + 2;
			}
		
        else if((!strcmp(expType1, "int")) && (!strcmp(expType2, "int_const")))
        {		
        	  int count1 = 0;
        	  char temp[100];
        	  char temp2[100];
        	  int d = expVal2;
		  sprintf(temp,"%s%d = icmp sgt i32 %s%d, %d\n","%",count,"%",count-1,d);
		  strcat(file_buffer,temp);
		  line_number += 1;
		  sprintf(temp2,"br i1 %s%d, label %s%s, label %s%s\n","%",count,"%","??","%","??");
		  strcat(file_buffer,temp2);
		  line_number += 1;
		  count1 = count1 + 1;
		  sprintf(temp,"\n");
           strcat(file_buffer,temp);
		  sprintf(temp,"%d:                                               ; preds = %s0\n",count + 1,"%");
		  strcat(file_buffer,temp);
		  line_number += 1;
		  count = count + 2;
			}
        if((!strcmp(expType1, "double")) && (!strcmp(expType2, "double")))
        {
        	  int count1 = 0;
        	  char temp[100];
        	  char temp2[100];
		  sprintf(temp,"%s%d = fcmp ogt double %s%d, %s%d\n","%",count,"%",count-2,"%",count-1);
		  strcat(file_buffer,temp);
		  line_number += 1;
		  sprintf(temp2,"br i1 %s%d, label %s%s, label %s%s\n","%",count,"%","??","%","??");
		  strcat(file_buffer,temp2);
		  count1 = count1 + 1;
		  line_number += 1;
		  sprintf(temp,"\n");
           strcat(file_buffer,temp);
		  sprintf(temp,"%d:                                               ; preds = %s0\n",count + 1,"%");
		  strcat(file_buffer,temp);
		  line_number += 1;
		   count = count + 2;
        }
        else if((!strcmp(expType1, "double")&& (!strcmp(expType2, "double_const")) ))
        {
        
        	  int count1 = 0;
        	  char temp[100];
        	  char temp2[100];
        	  int d = expVal2;
		  sprintf(temp,"%s%d = fcmp ogt double %s%d, %d\n","%",count,"%",count-1,d);
		  strcat(file_buffer,temp);
		  line_number += 1;
		  sprintf(temp2,"br i1 %s%d, label %s%s, label %s%s\n","%",count,"%","??","%","??");
		  strcat(file_buffer,temp2);
		  count1 = count1 + 1;
		  line_number += 1;
		  sprintf(temp,"\n");
           strcat(file_buffer,temp);
		  sprintf(temp,"%d:                                               ; preds = %s0\n",count + 1,"%");
		  strcat(file_buffer,temp);
		  line_number += 1;
		   count = count + 2;
        }
        else if((!strcmp(expType1, "double")&& (!strcmp(expType2, "int_const")) ))
        {
        
        	  int count1 = 0;
        	  char temp[100];
        	  char temp2[100];
        	  int d = expVal2;
		  sprintf(temp,"%s%d = fcmp ogt double %s%d, %d\n","%",count,"%",count-1,d);
		  strcat(file_buffer,temp);
		  line_number += 1;
		  sprintf(temp2,"br i1 %s%d, label %s%s, label %s%s\n","%",count,"%","??","%","??");
		  strcat(file_buffer,temp2);
		  count1 = count1 + 1;
		  line_number += 1;
		  sprintf(temp,"\n");
           strcat(file_buffer,temp);
		  sprintf(temp,"%d:                                               ; preds = %s0\n",count + 1,"%");
		  strcat(file_buffer,temp);
		  line_number += 1;
		   count = count + 2;
        }
    }
    else
    {
        /* ignore */
    }
}

void backpatch(int list[], int count1, int label)
{	
	for(int i =0;i<count1;i++){
		int a = list[i];
		int line = 0;
		int temp = 0;
		while(line != a){
			if(file_buffer[temp] == '\n'){
				line = line + 1;
			}
			temp += 1;
		}
		while(file_buffer[temp] != '?'){
			temp += 1;
		}
		if(label < 10){
		char label1 = 48 + label;
		char label2 = 32;
		file_buffer[temp] = label1;
		file_buffer[temp + 1] = label2;
		}
		if(label >= 10){
		int f = label%10;
		int s = (label - f)/10;
		//printf("Testing1 - %d\n",f);
		//printf("Testing 2 - %d\n",s);
		char label1 = 48 + s;
		char label2 = 48 + f;
		file_buffer[temp] = label1;
		file_buffer[temp + 1] = label2;
		}
		
	}
}


int main()
{
    yyparse();
}
int yyerror(char *s)
{
    fprintf(stderr, "%s\n",s);
}
