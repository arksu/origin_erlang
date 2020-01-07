/*
 * types.c
 *
 *  Created on: 31.12.2011
 *      Author: arksu
 */

#include "defines.h"
#include <string.h>
#include "erl_nif.h"

#include "nif_utils.h"

#define CHECK_TYPE(CTYPE) 			if (strcmp(type, CTYPE) == 0) return 1
#define CHECK_TYPEID(CTYPE) 		if (typeid == CTYPE) return 1
#define GET_TYPEID(CTYPE, TYPEID)	if (strcmp(type, CTYPE) == 0) return TYPEID
#define GET_TYPE(CTYPE, TYPEID)		if (typeid == TYPEID) { strcpy(atom, CTYPE); return; }


//----------------------------------------------------------------------------------------------
inline int is_item(int typeid) {
	CHECK_TYPEID(TYPE_STONE_AXE);
	CHECK_TYPEID(TYPE_BILLET);
	CHECK_TYPEID(TYPE_DRAWING_BOX);
	CHECK_TYPEID(TYPE_BASKET);
	CHECK_TYPEID(TYPE_FABRIC_BAG);
	CHECK_TYPEID(TYPE_SOIL);
	CHECK_TYPEID(TYPE_CARROT);
	CHECK_TYPEID(TYPE_SEED_CARROT);
	CHECK_TYPEID(TYPE_SEED_WHEAT);
	CHECK_TYPEID(TYPE_SEED_WILD);
	CHECK_TYPEID(TYPE_BRANCH_LEAF);
	CHECK_TYPEID(TYPE_STONE_PIECE);
	CHECK_TYPEID(TYPE_BRANCH);
	CHECK_TYPEID(TYPE_BOARD);
	CHECK_TYPEID(TYPE_PICKED_WILD);
	CHECK_TYPEID(TYPE_RABBIT_CATCHED);
	CHECK_TYPEID(TYPE_RABBIT_DEAD);
	CHECK_TYPEID(TYPE_CLAY);
	CHECK_TYPEID(TYPE_SAND);
	CHECK_TYPEID(TYPE_ASH);
	CHECK_TYPEID(TYPE_BRICK_CLAY);
	CHECK_TYPEID(TYPE_STRING);

	CHECK_TYPEID(TYPE_BRICK_CLAY_RAW);
	CHECK_TYPEID(TYPE_STRAW);
	CHECK_TYPEID(TYPE_FLOUR);
	CHECK_TYPEID(TYPE_TEAPOT);
	CHECK_TYPEID(TYPE_WATERFLASK);
	CHECK_TYPEID(TYPE_WOOD_SHOES);
	CHECK_TYPEID(TYPE_APPLE);
	CHECK_TYPEID(TYPE_APPLE_STUB);
	CHECK_TYPEID(TYPE_SEED_APPLE);
	CHECK_TYPEID(TYPE_PANTS_SKIN);
	CHECK_TYPEID(TYPE_FRIC_KEPKA);

	return 0;
}

//----------------------------------------------------------------------------------------------
inline int is_plant(int typeid) {
	CHECK_TYPEID(TYPE_PLANT_CARROT);
	CHECK_TYPEID(TYPE_PLANT_WHEAT);

	return 0;
}

//----------------------------------------------------------------------------------------------
#define DEFAULT_SIZE 			5
#define BOUNDS(TYPEID, SIZE) 		if (!found && typeid==TYPEID) {found = 1; *l=-SIZE; *t=-SIZE; *r=SIZE; *b=SIZE; }
#define BOUNDS_EX(TYPEID, SL, ST, SR, SB) 		if (!found && typeid==TYPEID) {found = 1; *l=-SL; *t=-ST; *r=SR; *b=SB; }

inline void get_bounds(int typeid, int * l, int * t, int * r, int * b) {
	int found = 0;

	BOUNDS(TYPE_PLAYER, 		4);
	BOUNDS(TYPE_SMALL_STONE, 	8);
	BOUNDS(TYPE_STONE, 			15);

	BOUNDS_EX(TYPE_GATEX_FENCE, 10, 5, 10, 5);
	BOUNDS_EX(TYPE_BUILD_GATEX_FENCE, 10, 5, 10, 5);
	BOUNDS_EX(TYPE_GATEY_FENCE, 5, 10, 5, 10);
	BOUNDS_EX(TYPE_BUILD_GATEY_FENCE, 5, 10, 5, 10);

	BOUNDS(TYPE_CHAIR, 			3);
	BOUNDS(TYPE_JAR, 			3);

	if (!found) {
		*l=-DEFAULT_SIZE;
		*t=-DEFAULT_SIZE;
		*r=DEFAULT_SIZE;
		*b=DEFAULT_SIZE;
	}
}

//----------------------------------------------------------------------------------------------
// объекты - всегда видимые вне зависимости от времени дня, зрения, скрытности
inline int is_always_visible(int typeid) {
	CHECK_TYPEID(TYPE_FIR_TREE);
	CHECK_TYPEID(TYPE_STONE);
	CHECK_TYPEID(TYPE_CORNER_HOME_WOOD);
	CHECK_TYPEID(TYPE_WALLY_HOME_WOOD);
	CHECK_TYPEID(TYPE_WALLX_HOME_WOOD);


	return 0;
}

//----------------------------------------------------------------------------------------------
// является ли объект прозрачным
inline int is_transparent(int typeid) {
	// все вещи прозрачные
	if (typeid > 1000) return 1;
	// возвращают 1
	CHECK_TYPEID(TYPE_LOG);
	CHECK_TYPEID(TYPE_LOGY);
	CHECK_TYPEID(TYPE_STUMP);
	CHECK_TYPEID(TYPE_PLANT_CARROT);
	CHECK_TYPEID(TYPE_PLANT_WHEAT);
	CHECK_TYPEID(TYPE_PLANT_WILD);

	return 0;
}

//----------------------------------------------------------------------------------------------
// дают ли объекты коллизию между собой
// for_type=player  type=box, log, etc...
#define CHECK_COLLISION_FALSE(CTYPE) if (type == CTYPE || for_type == CTYPE) return 0;
#define CHECK_COLLISION_PLAYER(CTYPE) if ((type == CTYPE && for_type == TYPE_PLAYER) || (for_type == CTYPE && type == TYPE_PLAYER)) return 0;
inline int get_collision(int type, int for_type) {
	if (is_item(type) || is_item(for_type)) return 0;
	if (is_plant(type) || is_plant(for_type)) return 0;

	if (for_type == TYPE_RABBIT && type==TYPE_PLAYER) return 0;
	if (type == TYPE_RABBIT && for_type==TYPE_PLAYER) return 0;
	if (for_type==TYPE_RABBIT) return 0;

	CHECK_COLLISION_FALSE(TYPE_LOG);
	CHECK_COLLISION_FALSE(TYPE_LOGY);
	CHECK_COLLISION_FALSE(TYPE_PLANT_WILD);
	CHECK_COLLISION_FALSE(TYPE_STRING_WILD);

//	CHECK_COLLISION_PLAYER(TYPE_WALLDOORX_HOME_WOOD);
//	CHECK_COLLISION_PLAYER(TYPE_WALLDOORY_HOME_WOOD);
//	CHECK_COLLISION_PLAYER(TYPE_GATEX_FENCE);
//	CHECK_COLLISION_PLAYER(TYPE_GATEY_FENCE);

	return 1;
}

//----------------------------------------------------------------------------------------------
// дает ли стейт объекта коллизию. проверяется только если тип объекта дает коллизию (get_collision == 1)
inline int get_collision_object_state(ErlNifEnv* env, int for_type, int type, ERL_NIF_TERM object) {
	char atom[ATOM_BUFFER_SIZE];
	int arity;
	ERL_NIF_TERM state;

	// если это ворота заборчика и сравниваем с игроком смотрим в стейт
	if ( (type == TYPE_GATEX_FENCE || type == TYPE_GATEY_FENCE) && for_type == TYPE_PLAYER) {
		get_obj_state(env, object, &state);
		// #obj_gate
		if (enif_is_tuple(env, state)) {
			const ERL_NIF_TERM* gate_tuple;
			if (enif_get_tuple(env, state, &arity, &gate_tuple)) {
				enif_get_atom(env, gate_tuple[1], atom, ATOM_BUFFER_SIZE, ERL_NIF_LATIN1);
				// is opened?
				if (strcmp(atom, "true") == 0) return 0;
				return 1;
			} else return 1;
		} else return 1;
	}

	if ( (type == TYPE_WALLDOORX_HOME_WOOD || type == TYPE_WALLDOORY_HOME_WOOD) && for_type == TYPE_PLAYER) {
		return 0;
	}

	return 1;
}

//----------------------------------------------------------------------------------------------
// дает ли коллизию объект если мы к нему движемся (игрок идет к воротам для какого либо действия)
#define CHECK_COLLISION_MOVE_PLAYER(CTYPE) if ((type == CTYPE && for_type == TYPE_PLAYER) || (for_type == CTYPE && type == TYPE_PLAYER)) return 1;
inline int get_collision_object_to_move(int type, int for_type) {
	CHECK_COLLISION_MOVE_PLAYER(TYPE_GATEX_FENCE);
	CHECK_COLLISION_MOVE_PLAYER(TYPE_GATEY_FENCE);

	CHECK_COLLISION_MOVE_PLAYER(TYPE_WALLDOORX_HOME_WOOD);
	CHECK_COLLISION_MOVE_PLAYER(TYPE_WALLDOORY_HOME_WOOD);

	return 0;
}

//----------------------------------------------------------------------------------------------
inline int get_typeid(char * type) {
	TYPES(GET_TYPEID)
	return 0;
}

inline void get_type(int typeid, char * atom) {
	TYPES(GET_TYPE)
	strcpy(atom, "unknown");
}
