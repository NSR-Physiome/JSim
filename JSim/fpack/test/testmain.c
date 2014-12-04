/* main program test calls simini, simlop, simend */

#include <stdio.h>

extern void simini_();

int main(int argc, char **argv) {
	printf("simini_\n");
	simini_();
	printf("simlop_\n");
	simlop_();
	printf("simend_\n");
	simend_();
	printf("exiting\n");
	return 0;
}

