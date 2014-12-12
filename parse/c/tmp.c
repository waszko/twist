#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

int isInt(char *str){ 
	//rtn 1 if int, 0 if not
	int i, isInt=1, len = strlen(str);
	for (i=0; i<len; i++){
		if (isdigit(str[i])==0){isInt = 0;} //not an int
	}
	return isInt;
}	

int main(int argc, char *argv[]) {
	
	char *arg;
	int i;
	int s, stack[argc]; //argc??

	//first 2 args must be numbers, so put them on stack
	stack[0] = atoi(argv[1]);
	stack[1] = atoi(argv[2]);
	s=2;

	for (i=3; i<argc; i++){
		arg = argv[i];
		if (isInt(arg)==1){ //number
			stack[s] = atoi(arg);	
			s++;
		} else { //not number
			if (strcmp(arg,"+")== 0) {
				stack[s-2] = stack[s-2] + stack[s-1];
			} else if (strcmp(arg,"-")==0){
				stack[s-2] = stack[s-2] - stack[s-1];
			} else if (strcmp(arg,"x")==0){
				stack[s-2] = stack[s-2] * stack[s-1];
			} else if (strcmp(arg,"/")==0){
				stack[s-2] = stack[s-2] / stack[s-1];
			} else {printf("ERROR\n");}		
			s--;
		}		
	}
	printf("output = %d\n", stack[0]);

	return 0;
}
