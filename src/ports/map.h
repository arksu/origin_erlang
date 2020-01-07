/*
 * map.h
 *
 *  Created on: 01.01.2012
 *      Author: arksu
 */

#ifndef MAP_H_
#define MAP_H_

Coord get_map_coord(int Sg, int Grid);
int get_sg(int x, int y, int lv);
void get_sg_grid(int x, int y, int lv, int * sg, int * grid);
void get_grid_coord(int x, int y, int lv, int * sg, int * grid, int * index);

#endif /* MAP_H_ */
