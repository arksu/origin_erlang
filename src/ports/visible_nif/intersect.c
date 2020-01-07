/*
 * intersect.c
 *
 *  Created on: 05.01.2012
 *      Author: arksu
 */

inline int intersection(int x11, int y11, int x12, int y12,
		int x21, int y21, int x22, int y22) {
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
