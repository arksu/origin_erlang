/*
 * process_tile.c
 *
 *  Created on: 	10.10.2011
 *  Refactor on: 	08.01.2012
 *  Author: arksu
 */
#include <stdio.h>
#include "erl_nif.h"
#include "../defines.h"

#define GET_TILE(TX, TY)		src_tile[3*GRID_SIZE*GRID_SIZE + GRID_SIZE + (TY)*3*GRID_SIZE + (TX)]
#define GET_FLAG(TX, TY)		src_flag[3*GRID_SIZE*GRID_SIZE + GRID_SIZE + (TY)*3*GRID_SIZE + (TX)]
#define SET_TILE(TX, TY, VAL)	out_tiles[(TX)+(TY)*GRID_SIZE] = VAL; *tiles_changed = 1
#define SET_FLAG(TX, TY, VAL)	out_tiles[(TX)+(TY)*GRID_SIZE + GRID_SIZE*GRID_SIZE] = VAL; *tiles_changed = 1

//-------------------------------------------------------------------------------------
inline void process_tiles(ErlNifEnv* env,
#ifdef DEBUG_BUILD
	FILE * pFile,
#endif
	byte* src_tile, byte* src_flag, byte* out_tiles,
	int* tiles_changed
	) {
//-------------------------------------------------------------------------------------
	// текущий тайл
	int x, y;
	 *tiles_changed = 0;

	for (x = 0; x < GRID_SIZE; ++x) {
		for (y = 0; y < GRID_SIZE; ++y) {

			byte tile = GET_TILE(x, y);
			out_tiles[(x)+(y)*GRID_SIZE] = tile; // SET_TILE

			byte f = GET_FLAG(x, y);
			out_tiles[(x)+(y)*GRID_SIZE + GRID_SIZE*GRID_SIZE] = f; // SET_FLAG

//			if (x < 4 && y < 4)
//			DEBUGPRINTF("tile x=%i y=%i tile=%i flag=%i\n", x, y, tile, f);

			// если в тайле еще нет воды
			if (tile == TILE_HOLE) { // тут поставить тайл выкопанной ямы. куда должна затекать вода
				// смотрим окружающие 8
				if (GET_TILE(x-1,y-1) == TILE_WATER_LOW || GET_TILE(x-1,y-1) == TILE_WATER_DEEP) {
					// спавним воду
					SET_TILE(x, y, TILE_WATER_LOW);
					DEBUGPRINTF("spawn water! x=%i y=%i\n", x, y);
				} else if (GET_TILE(x-1,y) == TILE_WATER_LOW || GET_TILE(x-1,y) == TILE_WATER_DEEP) {
					// спавним воду
					SET_TILE(x, y, TILE_WATER_LOW);
					DEBUGPRINTF("spawn water! x=%i y=%i\n", x, y);
				} else if (GET_TILE(x-1,y+1) == TILE_WATER_LOW || GET_TILE(x-1,y+1) == TILE_WATER_DEEP) {
					// спавним воду
					SET_TILE(x, y, TILE_WATER_LOW);
					DEBUGPRINTF("spawn water! x=%i y=%i\n", x, y);
				} else if (GET_TILE(x,y-1) == TILE_WATER_LOW || GET_TILE(x,y-1) == TILE_WATER_DEEP) {
					// спавним воду
					SET_TILE(x, y, TILE_WATER_LOW);
					DEBUGPRINTF("spawn water! x=%i y=%i\n", x, y);
				} else if (GET_TILE(x,y+1) == TILE_WATER_LOW || GET_TILE(x,y+1) == TILE_WATER_DEEP) {
					// спавним воду
					SET_TILE(x, y, TILE_WATER_LOW);
					DEBUGPRINTF("spawn water! x=%i y=%i\n", x, y);
				} else if (GET_TILE(x+1,y-1) == TILE_WATER_LOW || GET_TILE(x+1,y-1) == TILE_WATER_DEEP) {
					// спавним воду
					SET_TILE(x, y, TILE_WATER_LOW);
					DEBUGPRINTF("spawn water! x=%i y=%i\n", x, y);
				} else if (GET_TILE(x+1,y) == TILE_WATER_LOW || GET_TILE(x+1,y) == TILE_WATER_DEEP) {
					// спавним воду
					SET_TILE(x, y, TILE_WATER_LOW);
					DEBUGPRINTF("spawn water! x=%i y=%i\n", x, y);
				} else if (GET_TILE(x+1,y+1) == TILE_WATER_LOW || GET_TILE(x+1,y+1) == TILE_WATER_DEEP) {
					// спавним воду
					SET_TILE(x, y, TILE_WATER_LOW);
					DEBUGPRINTF("spawn water! x=%i y=%i\n", x, y);
				}

			}

			// вспашка может исчезать с течением времени
			if (tile == TILE_PLOWED && (rand() % 40) == 1) {
				SET_TILE(x, y, TILE_GRASS);
			}

		}
	}
}
