/*
 * map.c
 *
 *  Created on: 10.10.2011
 *      Author: arksu
 */

#include "defines.h"

inline Coord get_map_coord(int Sg, int Grid) {
    Coord res;

	res.lv = Grid / (SUPERGRID_SIZE * SUPERGRID_SIZE) ;
    int ModGrid = Grid % (SUPERGRID_SIZE * SUPERGRID_SIZE);

    res.x = (Sg % (LEFT_SUPERGRID + RIGHT_SUPERGRID)) * (SUPERGRID_FULL_SIZE) + (ModGrid % (SUPERGRID_SIZE)) * (TILE_SIZE*GRID_SIZE);
    res.y = (Sg / (TOP_SUPERGRID + BOTTOM_SUPERGRID)) * (SUPERGRID_FULL_SIZE) + (ModGrid / (SUPERGRID_SIZE)) * (TILE_SIZE*GRID_SIZE);
    return res;
}

inline int get_sg(int x, int y, int lv) {
    int SgX = x / (SUPERGRID_FULL_SIZE);
    int SgY = y / (SUPERGRID_FULL_SIZE);
    return LEFT_SUPERGRID + SgX + (SgY + TOP_SUPERGRID) * (LEFT_SUPERGRID + RIGHT_SUPERGRID);
}

inline void get_sg_grid(int x, int y, int lv, int * sg, int * grid) {
    int SgX = x / (SUPERGRID_FULL_SIZE);
    int SgY = y / (SUPERGRID_FULL_SIZE);
    *sg = LEFT_SUPERGRID + SgX + (SgY + TOP_SUPERGRID) * (LEFT_SUPERGRID + RIGHT_SUPERGRID);

    int GridX = (x % (SUPERGRID_FULL_SIZE)) / (GRID_FULL_SIZE);
    int GridY = (y % (SUPERGRID_FULL_SIZE)) / (GRID_FULL_SIZE);
    *grid = GridX + GridY * SUPERGRID_SIZE + lv*SUPERGRID_SIZE*SUPERGRID_SIZE;
}

inline void get_grid_coord(int x, int y, int lv, int * sg, int * grid, int * index) {
    int SgX = x / (SUPERGRID_FULL_SIZE);
    int SgY = y / (SUPERGRID_FULL_SIZE);
    *sg = LEFT_SUPERGRID + SgX + (SgY + TOP_SUPERGRID) * (LEFT_SUPERGRID + RIGHT_SUPERGRID);

    int GridX = (x % (SUPERGRID_FULL_SIZE)) / (GRID_FULL_SIZE);
    int GridY = (y % (SUPERGRID_FULL_SIZE)) / (GRID_FULL_SIZE);
    *grid = GridX + GridY * SUPERGRID_SIZE + lv*SUPERGRID_SIZE*SUPERGRID_SIZE;
    int IndexX = (x % (GRID_FULL_SIZE)) / (TILE_SIZE);
    int IndexY = (y % (GRID_FULL_SIZE)) / (TILE_SIZE);
    *index = IndexX + IndexY * GRID_SIZE;
}



