#include <string.h>
#include <stdio.h>
#include <time.h>
#include <stdlib.h>

#define MAX_ITEMS 1000
#define MAX_COORD 300
#define RETRY 100000

int x11[MAX_ITEMS];
int x12[MAX_ITEMS];
int x21[MAX_ITEMS];
int x22[MAX_ITEMS];
int y11[MAX_ITEMS];
int y12[MAX_ITEMS];
int y21[MAX_ITEMS];
int y22[MAX_ITEMS];

int main() {
	int i, j;
	printf("init...\n");
	srand(time(NULL));
	int sum = 0;

	for (i=0; i<MAX_ITEMS; i++) {
		x11[i] = rand() % MAX_COORD;
		x12[i] = rand() % MAX_COORD;
		x21[i] = rand() % MAX_COORD;
		x22[i] = rand() % MAX_COORD;
		y11[i] = rand() % MAX_COORD;
		y12[i] = rand() % MAX_COORD;
		y21[i] = rand() % MAX_COORD;
		y22[i] = rand() % MAX_COORD;
	}

	printf("begin...\n");
	clock_t t0 = clock();
	int rcount = 0;
	for (j = 0; j < RETRY; j++) {
		for (i = 0; i < MAX_ITEMS; i++) {
			sum += intersection_ark(x11[i], y11[i], x12[i], y12[i], x21[i], y21[i],
					x22[i], y22[i]);
			rcount++;
		}

	}

	clock_t t1 = clock();
//	for (j = 0; j < RETRY; j++) {
//		for (i = 0; i < MAX_ITEMS; i++) {
//			sum += intersection_vil(x11[i], y11[i], x12[i], y12[i], x21[i], y21[i],
//					x22[i], y22[i]);
//		}
//	}
//
//	clock_t t2 = clock();

	printf("ark----------------------------\n");
	printf("time: %-2.3f\n", (double)(t1 - t0) / CLOCKS_PER_SEC);
	printf("clocks: %d\n", t1-t0);

//	printf("vil----------------------------\n");
//	printf("time: %-2.3f\n", (double)(t2 - t1) / CLOCKS_PER_SEC);
//	printf("clocks: %d\n", t2-t1);

	printf("end! sum = %d rcount=%d \n", sum, rcount);
}


int intersection_ark(int x11, int y11, int x12, int y12, int x21, int y21,
		int x22, int y22) {
	int dir1x = x12 - x11;
	int dir1y = y12 - y11;

	int dir2x = x22 - x21;
	int dir2y = y22 - y21;

	//считаем уравнения прямых проходящих через отрезки
	float a1 = -dir1y;
	float b1 = +dir1x;
	float d1 = -(a1 * x11 + b1 * y11);

	float a2 = -dir2y;
	float b2 = +dir2x;
	float d2 = -(a2 * x21 + b2 * y21);

	//подставляем концы отрезков, для выяснения в каких полуплоскотях они
	float seg1_line2_start = a2 * x11 + b2 * y11 + d2;
	float seg1_line2_end = a2 * x12 + b2 * y12 + d2;

	float seg2_line1_start = a1 * x21 + b1 * y21 + d1;
	float seg2_line1_end = a1 * x22 + b1 * y22 + d1;

	//если концы одного отрезка имеют один знак, значит он в одной полуплоскости и пересечения нет.
	if (seg1_line2_start * seg1_line2_end >= 0 || seg2_line1_start
			* seg2_line1_end >= 0)
		return 0;

	return 1;
}

int intersection_vil(int x11, int y11, int x12, int y12, int x21, int y21,
		int x22, int y22) {

	float t2 = ((y12 - y11) * (x21 - x11) - (x12 - x11) * (y21
			- y11)) / ((x12 - x11) * (y22 - y21) - (y12 - y11)
			* (x22 - x21));
	float t1 = (x21 + t2 * (x22 - x21) - x11) / (x12 - x11);
	if (t2 >= 0 && t2 <= 1 && t1 >= 0) {
		//Точка пересечения найдена, осталось вычислить
		return 1;
	} else {
		//точки пересечения нет
		return 0;
	}
}
