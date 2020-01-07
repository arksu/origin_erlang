/*
 * defines.h
 *
 *  Created on: 10.10.2011
 *      Author: arksu
 */

#ifndef DEFINES_H_
#define DEFINES_H_

#include "os_def.h"

typedef unsigned char byte;

//--------------------------------------------------------------
#ifdef DEBUG_BUILD
	#define WHERESTR  "[%s: %d]: "
	#define WHEREARG  __FILE__, __LINE__
	#define DEBUGPRINT2(...)       fprintf(pFile, __VA_ARGS__); fflush(pFile)
	#define DEBUGPRINTF(_fmt, ...)  DEBUGPRINT2(WHERESTR _fmt, WHEREARG, __VA_ARGS__)
	#define DEBUGPRINT(_msg)  DEBUGPRINT2(WHERESTR _msg, WHEREARG)
#else
	#define DEBUGPRINTF(_fmt, ...)  //empty
	#define DEBUGPRINT(_msg)  //empty
#endif
//--------------------------------------------------------------

#define LOG1(msg, arg1) fprintf(stderr, msg, arg1)
#define LOG2(msg, arg1, arg2) fprintf(stderr, msg, arg1, arg2)
#define LOG3(msg, arg1, arg2, arg3) fprintf(stderr, msg, arg1, arg2, arg3)
#define LOG4(msg, arg1, arg2, arg3, arg4) fprintf(stderr, msg, arg1, arg2, arg3, arg4)

#define SUPERGRID_SIZE 50
#define GRID_SIZE 100
#define TILE_SIZE 12
#define LEFT_SUPERGRID 0
#define TOP_SUPERGRID 0
#define RIGHT_SUPERGRID 1
#define BOTTOM_SUPERGRID 1

#define SUPERGRID_COUNT ((LEFT_SUPERGRID + RIGHT_SUPERGRID) * (TOP_SUPERGRID + BOTTOM_SUPERGRID))
#define GRID_FULL_SIZE (TILE_SIZE * GRID_SIZE)
#define SUPERGRID_FULL_SIZE (TILE_SIZE * GRID_SIZE * SUPERGRID_SIZE)
#define MIN_X_COORD ((-LEFT_SUPERGRID) * SUPERGRID_FULL_SIZE)
#define MIN_Y_COORD ((-TOP_SUPERGRID) * SUPERGRID_FULL_SIZE)
#define MAX_X_COORD (RIGHT_SUPERGRID * SUPERGRID_FULL_SIZE)
#define MAX_Y_COORD (BOTTOM_SUPERGRID * SUPERGRID_FULL_SIZE)
#define WORLD_BUFFER_SIZE 20

// расстояние для первичной фильтрации объектов для обсчета коллизий
#define COLLISION_DISTANCE 100
// расстояние в единицах игровых координат между итерациями
#define COLLISION_ITERATION_LENGTH 3

// move types
#define MOVE_LAND 0
#define MOVE_WATER 1
#define MOVE_SWIMMING 2

#define SECOND 		1
#define MINUTE 		SECOND*60
#define HOUR 		MINUTE*60
#define DAY 		HOUR*24

// #object record
#define OBJ_FIELDS_COUNT 9
#define OBJ_ID 1
#define OBJ_TYPE 2
#define OBJ_COORD 3
#define OBJ_PARAMS 4
#define OBJ_STATE 5
#define OBJ_TIME 6
#define OBJ_HP 7
#define OBJ_Q 8

// #grid record
#define GRID_COORD 	1
#define GRID_TILES 	2
#define GRID_STATIC 3
#define GRID_TICK 	4
#define GRID_DYN 	5
#define GRID_CLAIMS 8

// #claim_personal
#define CLAIM_PERSONAL_OWNER_ID 	1
#define CLAIM_PERSONAL_OBJECT_ID 	2
#define CLAIM_PERSONAL_RECT 		3


// #neighbor_rec record
#define NEIGHBOR_REC_STATE	4

// time types
#define TT_STATIC 0
#define TT_TICK 1
#define TT_DYNAMIC 2

// tile types
#define TILE_WATER_DEEP      	1
#define TILE_WATER_LOW    		2
#define TILE_HOLE				5
#define TILE_SETT     			10
#define TILE_PLOWED     		20
#define TILE_FOREST_LEAF     	25
#define TILE_FOREST_FIR    		30
#define TILE_GRASS     			35
#define TILE_SWAMP     			40
#define TILE_DIRT     			45
#define TILE_SAND     			50
#define TILE_HOUSE     			70
#define TILE_CELLAR    			75
#define TILE_CAVE     			80
// implement in tiles.h

//----------------------------------------------------------------------------------------------
// object types
#define TYPE_PLAYER		 				1
#define TYPE_SMALL_STONE 				10
#define TYPE_BUILD_BOX 					101
#define TYPE_BOX 						1000
#define TYPE_FIR_TREE 					4
#define TYPE_OAK_TREE 					5
#define TYPE_PINE_TREE 					6
#define TYPE_PEAR_TREE 					7
#define TYPE_APPLE_TREE 				8
#define TYPE_STUMP 						50
#define TYPE_LOG 						51
#define TYPE_LOGY 						57
#define TYPE_TILE_HILL 					55
#define TYPE_TILE_PIECE 				56
#define TYPE_STONE 						59
#define TYPE_STONE_AXE 					1002
#define TYPE_BILLET 					1001
#define TYPE_BASKET 					1003
#define TYPE_FABRIC_BAG 				1004
#define TYPE_DRAWING_BOX 				1005
#define TYPE_SOIL 						1006
#define TYPE_BRANCH_LEAF 				1010
#define TYPE_BOARD 						1009
#define TYPE_STONE_PIECE 				1011
#define TYPE_BRANCH 					1012
#define TYPE_CARROT 					1007
#define TYPE_SEED_CARROT 				1008
#define TYPE_PLANT_CARROT 				20
#define TYPE_BUILD1_CORNER_HOME_WOOD 	60
#define TYPE_CORNER_HOME_WOOD 			61
#define TYPE_BUILD1_WALLY_HOME_WOOD 	63
#define TYPE_WALLY_HOME_WOOD 			64
#define TYPE_BUILD1_WALLX_HOME_WOOD 	65
#define TYPE_WALLX_HOME_WOOD 			66
#define TYPE_PLANT_WILD					67
#define TYPE_PICKED_WILD				1013

#define TYPE_BUILD1_WALLWINDOWY_HOME_WOOD 	69
#define TYPE_WALLWINDOWY_HOME_WOOD 			70
#define TYPE_BUILD1_WALLDOORY_HOME_WOOD 	71
#define TYPE_WALLDOORY_HOME_WOOD 			72

#define TYPE_BUILD1_WALLWINDOWX_HOME_WOOD 	73
#define TYPE_WALLWINDOWX_HOME_WOOD 			74
#define TYPE_BUILD1_WALLDOORX_HOME_WOOD 	75
#define TYPE_WALLDOORX_HOME_WOOD 			76

#define TYPE_PLANT_WHEAT					77
#define TYPE_SEED_WHEAT						1014
#define TYPE_SEED_WILD						1015

#define TYPE_BUILD_CORNER_FENCE			78
#define TYPE_BUILD_WALLX_FENCE			79
#define TYPE_BUILD_WALLY_FENCE			80
#define TYPE_BUILD_GATEX_FENCE			81
#define TYPE_BUILD_GATEY_FENCE			82
#define TYPE_CORNER_FENCE				83
#define TYPE_WALLX_FENCE				84
#define TYPE_WALLY_FENCE				85
#define TYPE_GATEX_FENCE				86
#define TYPE_GATEY_FENCE				87
#define	TYPE_RUNESTONE					88
#define	TYPE_RABBIT						89
#define	TYPE_CHAIR						90
#define	TYPE_BUILD_CHAIR				91
#define	TYPE_RABBIT_CATCHED				1016
#define	TYPE_RABBIT_DEAD				1017
#define TYPE_CLAIM						92
#define TYPE_BUILD_CLAIM				93
#define TYPE_CLAY						1018
#define TYPE_SAND						1019
#define TYPE_BONFIRE					94
#define TYPE_BUILD_BONFIRE				95
#define TYPE_ASH						1020
#define TYPE_BRICK_CLAY					1021
#define TYPE_STRING						1022
#define TYPE_STRING_WILD				96
#define TYPE_JAR						97
#define TYPE_BUILD_JAR					98

#define TYPE_BRICK_CLAY_RAW				1023
#define TYPE_STRAW						1024
#define TYPE_FLOUR						1025
#define TYPE_TEAPOT						1026
#define TYPE_WATERFLASK					1027
#define TYPE_WOOD_SHOES					1028
#define TYPE_APPLE						1029
#define TYPE_APPLE_STUB					1030
#define TYPE_SEED_APPLE					1031
#define TYPE_PANTS_SKIN					1032
#define TYPE_FRIC_KEPKA					1033

// last: 97, 1032

//----------------------------------------------------------------------------------------------
#define TYPES(FUNC)	FUNC("player", TYPE_PLAYER); \
	FUNC("small_stone", TYPE_SMALL_STONE); \
	FUNC("build_box", TYPE_BUILD_BOX); \
	FUNC("box", TYPE_BOX); \
	FUNC("fir_tree", TYPE_FIR_TREE); \
	FUNC("oak_tree", TYPE_OAK_TREE); \
	FUNC("pine_tree", TYPE_PINE_TREE); \
	FUNC("pear_tree", TYPE_PEAR_TREE); \
	FUNC("apple_tree", TYPE_APPLE_TREE); \
	\
	FUNC("stump", TYPE_STUMP); \
	FUNC("log", TYPE_LOG); \
	FUNC("logy", TYPE_LOGY); \
	\
	FUNC("tile_hill", TYPE_TILE_HILL); \
	FUNC("tile_piece", TYPE_TILE_PIECE); \
	FUNC("stone", TYPE_STONE); \
	FUNC("build1_corner_home_wood", TYPE_BUILD1_CORNER_HOME_WOOD); \
	FUNC("corner_home_wood", TYPE_CORNER_HOME_WOOD); \
	\
	FUNC("stone_axe", TYPE_STONE_AXE); \
	FUNC("billet", TYPE_BILLET); \
	FUNC("basket", TYPE_BASKET); \
	FUNC("fabric_bag", TYPE_FABRIC_BAG); \
	FUNC("drawing_box", TYPE_DRAWING_BOX); \
	FUNC("soil", TYPE_SOIL); \
	FUNC("branch_leaf", TYPE_BRANCH_LEAF); \
	FUNC("board", TYPE_BOARD); \
	FUNC("stone_piece", TYPE_STONE_PIECE); \
	FUNC("branch", TYPE_BRANCH); \
	FUNC("carrot", TYPE_CARROT); \
	FUNC("seed_carrot", TYPE_SEED_CARROT); \
	FUNC("plant_carrot", TYPE_PLANT_CARROT); \
	\
	FUNC("plant_wild", TYPE_PLANT_WILD); \
	FUNC("picked_wild", TYPE_PICKED_WILD); \
	\
	FUNC("build1_wally_home_wood", TYPE_BUILD1_WALLY_HOME_WOOD); \
	FUNC("wally_home_wood", TYPE_WALLY_HOME_WOOD); \
	FUNC("build1_wallx_home_wood", TYPE_BUILD1_WALLX_HOME_WOOD); \
	FUNC("wallx_home_wood", TYPE_WALLX_HOME_WOOD); \
	\
	FUNC("build1_wallwindowy_home_wood", TYPE_BUILD1_WALLWINDOWY_HOME_WOOD); \
	FUNC("wallwindowy_home_wood", TYPE_WALLWINDOWY_HOME_WOOD); \
	FUNC("build1_wallwindowx_home_wood", TYPE_BUILD1_WALLWINDOWX_HOME_WOOD); \
	FUNC("wallwindowx_home_wood", TYPE_WALLWINDOWX_HOME_WOOD); \
	\
	FUNC("build1_walldoory_home_wood", TYPE_BUILD1_WALLDOORY_HOME_WOOD); \
	FUNC("walldoory_home_wood", TYPE_WALLDOORY_HOME_WOOD); \
	FUNC("build1_walldoorx_home_wood", TYPE_BUILD1_WALLDOORX_HOME_WOOD); \
	FUNC("walldoorx_home_wood", TYPE_WALLDOORX_HOME_WOOD); \
	FUNC("seed_wheat", TYPE_SEED_WHEAT); \
	FUNC("plant_wheat", TYPE_PLANT_WHEAT); \
	FUNC("seed_wild", TYPE_SEED_WILD); \
	\
	FUNC("build_corner_fence", TYPE_BUILD_CORNER_FENCE); \
	FUNC("build_wallx_fence", TYPE_BUILD_WALLX_FENCE); \
	FUNC("build_wally_fence", TYPE_BUILD_WALLY_FENCE); \
	FUNC("build_gatex_fence", TYPE_BUILD_GATEX_FENCE); \
	FUNC("build_gatey_fence", TYPE_BUILD_GATEY_FENCE); \
	FUNC("corner_fence", TYPE_CORNER_FENCE); \
	FUNC("wallx_fence", TYPE_WALLX_FENCE); \
	FUNC("wally_fence", TYPE_WALLY_FENCE); \
	FUNC("gatex_fence", TYPE_GATEX_FENCE); \
	FUNC("gatey_fence", TYPE_GATEY_FENCE); \
	FUNC("runestone", TYPE_RUNESTONE); \
	FUNC("rabbit", TYPE_RABBIT); \
	FUNC("chair", TYPE_CHAIR); \
	FUNC("build_chair", TYPE_BUILD_CHAIR); \
	FUNC("rabbit_catched", TYPE_RABBIT_CATCHED); \
	FUNC("rabbit_dead", TYPE_RABBIT_DEAD); \
	FUNC("claim", TYPE_CLAIM); \
	FUNC("build_claim", TYPE_BUILD_CLAIM); \
	FUNC("clay", TYPE_CLAY); \
	FUNC("sand", TYPE_SAND); \
	FUNC("bonfire", TYPE_BONFIRE); \
	FUNC("build_bonfire", TYPE_BUILD_BONFIRE); \
	FUNC("ash", TYPE_ASH); \
	FUNC("brick_clay", TYPE_BRICK_CLAY); \
	FUNC("string", TYPE_STRING); \
	FUNC("string_wild", TYPE_STRING_WILD); \
	FUNC("jar", TYPE_JAR); \
	FUNC("build_jar", TYPE_BUILD_JAR); \
	\
	FUNC("brick_clay_raw", TYPE_BRICK_CLAY_RAW); \
	FUNC("straw", TYPE_STRAW); \
	FUNC("flour", TYPE_FLOUR); \
	FUNC("teapot", TYPE_TEAPOT); \
	FUNC("waterflask", TYPE_WATERFLASK); \
	FUNC("wood_shoes", TYPE_WOOD_SHOES); \
	FUNC("apple", TYPE_APPLE); \
	FUNC("apple_stub", TYPE_APPLE_STUB); \
	FUNC("seed_apple", TYPE_SEED_APPLE); \
	FUNC("pants_skin", TYPE_PANTS_SKIN); \
	FUNC("fric_kepka", TYPE_FRIC_KEPKA); \


// object buffer size
#define OBJ_ARRAY_LEN 20000
#define ATOM_BUFFER_SIZE 100

typedef struct {
	int x;
	int y;
	int lv;
} Coord;

typedef struct {
	int left;
	int top;
	int right;
	int bottom;
} RECT;

Coord get_map_coord(int Sg, int Grid);
int get_sg(int x, int y, int lv);


#endif /* DEFINES_H_ */
