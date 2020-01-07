#include <string.h>
#include <stdio.h>
#include <time.h>
#include <stdlib.h>


int main() {
//	int i, j;
//	printf("init...\n");
//	srand(time(NULL));
//	int sum = 0;
//
//	char * s1 = "abcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghje";
//	char * s2 = "abcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghj1";
//
//	printf("begin...\n");
//	clock_t t0 = clock();
//
//	for (j = 0; j < 10000000; j++) {
//		char * s1 = "abcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghje";
//		char * s2 = "abcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghjeabcdefghj1";
//		if (strcmp(s1, s2)) sum++;
//	}
//
//	clock_t t1 = clock();
//
//
//	printf("time: %-2.3f\n", (double)(t1 - t0) / CLOCKS_PER_SEC);
//	printf("clocks: %d\n", t1-t0);
//
//
//	printf("end! sum = %d \n", sum);

	char buf[100];
	char *p;
	p = buf;
	memset(buf, 0, 100);
	strcpy(p, "some test");
	printf("буфер после копирования:  \"%s\"\n", p);

	return 0;
}



