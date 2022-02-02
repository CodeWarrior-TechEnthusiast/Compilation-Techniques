%{
#include "advanced_parser.tab.h"
#include "head.h"
int  yylex(void);
int  yyerror(char* s);
void write_init_lines();
void write_closing_lines();
int  addToSymTab(char* t, char* s);
int  symTabLookUp(char* s);
char curType[MAX];
int count = 1;
%}

%union{
    int    ival;
    double dval;
    char   sval[10];
    struct expStruct
    {
        double expVal;
        char   expType[30];
        int    irName;     }eval;
};


%token COMMA INT_CONST SEMICOLON RELOP LOGOP OR AND NOT IF WHILE BEG END
%token <sval> ID 
%type <eval> EXP TERM FACTOR bExp 
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
funcDef : TYPE_CONST ID '(' argList ')' BEG declList 
	{ } 
        stmtList END {}
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
 			printf("%s%d = alloca i32, align 4\n","%",count);
         	}
         	if(curType[0] == c[1]){
         		printf("%s%d = alloca DOUBLE, align 8\n","%",count);
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
 			printf("%s%d = alloca i32, align 4\n","%",count);
         	}
         	if(curType[0] == c[1]){
         		printf("%s%d = alloca DOUBLE, align 8\n","%",count);
         	}
         	count += 1;
           }
stmtList : stmtList stmt | stmt;
stmt : assignStmt | ifStmt | whileStmt ;
assignStmt : ID '=' EXP SEMICOLON 
	   {    
               if(!strcmp($3.expType, "int_const") || !strcmp($3.expType, "int"))
               {
                   	if(!strcmp($3.expType, "int_const")){
                   		int value = $3.expVal; 
                   		printf("store i32 %d, i32* %s%d, align 4\n",value,"%",symTabLookUp($1) + 1);
                   	}
                   	else{
                   		printf("store i32 %s%d, i32* %s%d, align 4\n","%",count -1,"%",symTabLookUp($1) + 1);
                   	}
                   	
               }
               if(!strcmp($3.expType, "double_const") || !strcmp($3.expType, "double"))
               {
                   	if(!strcmp($3.expType, "double_const")){
                   		printf("store double %f, double* %s%d, align 8\n",$3.expVal,"%",symTabLookUp($1) + 1);
                   	}
                   	else{
                   		printf("store double %s%d, double* %s%d, align 8\n","%",count -1,"%",symTabLookUp($1) + 1);
                   	}
                   	
               }
           }
EXP : EXP '+' TERM 
    { 
        strcpy($$.expType, $1.expType); 
        int value = $3.expVal;
        if(!strcmp($1.expType, "int"))
        {
            if(!strcmp($3.expType, "int_const")){
		printf("%s%d = add nsw i32 %s%d , %d\n","%",count,"%",count-1,value);
		count += 1;
		}
	    else{
	    	printf("%s%d = add nsw i32 %s%d , %s%d\n","%",count,"%",count-2,"%",count-1);
	    	count += 1;
	    }
        }
        if(!strcmp($1.expType, "double"))
        {
            if(!strcmp($1.expType, "double")){
		    printf("%s%d = fadd double %s%d , %f\n","%",count,"%",count-1,$3.expVal);
		    count += 1;
		    }
	    else{
	    	    printf("%s%d = fadd double %s%d , %s%d\n","%",count,"%",count-2,"%",count-1);
		    count += 1;
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
		printf("%s%d = mul nsw i32 %s%d , %d\n","%",count,"%",count-1,value);
		count += 1;
		}
	    else{
	    	printf("%s%d = mul nsw i32 %s%d , %s%d\n","%",count,"%",count-2,"%",count-1);
	    	count += 1;
	    }
        }
        if(!strcmp($1.expType, "double"))
        {
            if(!strcmp($1.expType, "double_const")){
		    printf("%s%d = fmul double %s%d , %f\n","%",count,"%",count-1,$3.expVal);
		    count += 1;
		    }
	    else{
	    	    printf("%s%d = fmul double %s%d , %s%d\n","%",count,"%",count-2,"%",count-1);
		    count += 1;
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
 			printf("%s%d = load i32, i32* %s%d, align 4\n","%",count,"%",symTabLookUp($1)+1);
         	}
         	if($$.expType[0] == c[1]){
         		printf("%s%d = load double, double* %s%d, align 8\n","%",count,"%",symTabLookUp($1)+1);
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
ifStmt : IF'('bExp')'BEG stmtList END { }
bExp : EXP RELOP EXP { }
whileStmt : WHILE'('bExp')' BEG stmtList END { } 
%%

void write_init_lines()
{
	printf("; ModuleID = 'prog.c'\n");
	printf("source_filename = \"prog.c\"\n");
	printf("target datalayout = \"e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128\"\n");
	printf("target triple = \"x86_64-pc-linux-gnu\"\n");
	printf("; Function Attrs: noinline nounwind optnone uwtable\n");
	printf("define dso_local i32 @main() #0 {\n");
}

void write_closing_lines()
{
	printf("ret i32 0\n}\n");
	printf("attributes #0 = { noinline nounwind optnone uwtable \"correctly-rounded-divide-sqrt-fp-math\"=\"false\" \"disable-tail-calls\"");
	printf("=\"false\" \"frame-pointer\"=\"all\" \"less-precise-fpmad\"=\"false\" \"min-legal-vector-width\"=\"0\" \"no-infs-fp-math\"");
	printf("=\"false\" \"no-jump-tables\"=\"false\" \"no-nans-fp-math\"=\"false\" \"no-signed-zeros-fp-math\"=\"false\" \"no-trapping-math\"=\"false\""); 
	printf("\"stack-protector-buffer-size\"=\"8\" \"target-cpu\"=\"x86-64\" \"target-features\"=\"+cx8,+fxsr,+mmx,+sse,+sse2,+x87\" \"unsafe-fp-math\"");
	printf("=\"false\" \"use-soft-float\"=\"false\"}");
	printf("!llvm.module.flags = !{!0}");
	printf("!llvm.ident = !{!1}");
	printf("!0 = !{i32 1, !\"wchar_size\", i32 4}");
	printf("!1 = !{!\"clang version 10.0.0-4ubuntu1\"}");
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

int main()
{
    yyparse();
}
int yyerror(char *s)
{
    fprintf(stderr, "%s\n",s);
}
