/*
 * tiles.h
 *
 *  Created on: 18.01.2012
 *      Author: arksu
 */

#ifndef TILES_H_
#define TILES_H_

#include "defines.h"

#define GET_TILE_TYPE(TYPEID, TYPE_NAME) 		if (type == TYPEID) { strcpy(atom, TYPE_NAME); return; }
#define GET_TILE_TYPEID(TYPEID, TYPE_NAME) 		if (strcmp(TYPE_NAME, type) == 0) return TYPEID


#define TILES(FUNC) \
	FUNC(TILE_WATER_DEEP, 		"water_deep"); \
	FUNC(TILE_WATER_LOW,	 	"water_low"); \
	FUNC(TILE_HOLE, 			"hole"); \
	FUNC(TILE_CAVE, 			"cave"); \
	FUNC(TILE_CELLAR, 			"cellar"); \
	FUNC(TILE_DIRT, 			"dirt"); \
	FUNC(TILE_FOREST_FIR, 		"forest_fir"); \
	FUNC(TILE_FOREST_LEAF, 		"forest_leaf"); \
	FUNC(TILE_GRASS, 			"grass"); \
	FUNC(TILE_HOUSE, 			"house"); \
	FUNC(TILE_PLOWED, 			"plowed"); \
	FUNC(TILE_SAND, 			"sand"); \
	FUNC(TILE_SETT, 			"sett"); \
	FUNC(TILE_SWAMP, 			"swamp")


//-----------------------------------------------------------------------------------------------
inline void get_tile_type(byte type, char* atom);
//-----------------------------------------------------------------------------------------------
inline byte get_tile_typeid(char* type);
//-----------------------------------------------------------------------------------------------
inline int get_tiles_collision(int move_type, int type, byte tile);

#endif /* TILES_H_ */
