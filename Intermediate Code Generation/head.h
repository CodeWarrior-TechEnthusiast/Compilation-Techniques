#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define MAX 15
typedef struct symb_tab_entry{
	char name[MAX];
	char type[MAX];
}symb_tab_entry;
int symbTabEntryCount;
symb_tab_entry symbol_table[10];
