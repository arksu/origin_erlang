/*
 * tiles.c
 *
 *  Created on: Aug 22, 2012
 *      Author: arksu
 */

#include <stdio.h>
#include <string.h>

#include "defines.h"
#include "tiles.h"

inline int get_tiles_collision(int move_type, int type, byte tile) {

	switch (type) {
		case TYPE_RABBIT:
			if (move_type == MOVE_LAND) {
				if (tile == TILE_WATER_DEEP || tile == TILE_WATER_LOW) {
					return tile;
				}
				return 0;
			}
			return 0;
			break;

		default:
			// если двигаемся по земле - коллизии с тайлами воды
			if (move_type == MOVE_LAND) {
				if (tile == TILE_WATER_DEEP) {
					return tile;
				}
				return 0;
			}

			// если плывем - коллизий нет вообще
			if (move_type == MOVE_SWIMMING) {
				return 0;
			}

			// если движемся по воде - столкновение с любым не водяным тайлом
			if (move_type == MOVE_WATER) {
				if (tile != TILE_WATER_DEEP && tile != TILE_WATER_LOW) {
					return tile;
				}
				return 0;
			}
			break;
	}
	return 0;
}

//-----------------------------------------------------------------------------------------------
inline void get_tile_type(byte type, char* atom) {
	TILES(GET_TILE_TYPE);
	strcpy(atom, "none");
}
//-----------------------------------------------------------------------------------------------
inline byte get_tile_typeid(char* type) {
	TILES(GET_TILE_TYPEID);
	return 0;
}
