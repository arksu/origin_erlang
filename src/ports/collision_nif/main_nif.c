/*
 * main.c
 *
 *  Created on: 30.12.2011
 *      Author: arksu
 */

#include <stdio.h>
#include <string.h>
#include "erl_nif.h"

#include "collision.h"
#include "../defines.h"
#include "../nif_utils.h"
#include <math.h>

#ifdef DEBUG_BUILD
static	FILE * pFile;
#endif


//-------------------------------------------------------------------------------------------
// заполнить список объектов из листа
inline void fill_obj_list(
#ifdef DEBUG_BUILD
		FILE * pFile,
#endif
		ErlNifEnv* env, ERL_NIF_TERM term, GAME_OBJECT* obj_list, int offset, int* count, int time_type) {
	ERL_NIF_TERM head, tail;
	ERL_NIF_TERM list = term;


	int arity;
	const ERL_NIF_TERM* tuple;
	Coord c;
	char type_atom[ATOM_BUFFER_SIZE];


	(*count) = 0;
	int id;

	while (enif_get_list_cell(env, list, &head, &tail)) {

		enif_get_tuple(env, head, &arity, &tuple);

		get_obj_id(env, head, &id);


		DEBUGPRINTF("add object %i\n", id);
		obj_list[offset + (*count)].id = id;

		obj_list[offset + (*count)].obj = head;
		obj_list[offset + (*count)].time_type = time_type;

		c = get_obj_coord(env, head);
		obj_list[offset + (*count)].x = c.x;
		obj_list[offset + (*count)].y = c.y;

		get_obj_type(env, head, type_atom);
		obj_list[offset + (*count)].type = get_typeid(type_atom);

		(*count)++;
		list = tail;
	}
}

//-------------------------------------------------------------------------------------------
// получить количество объектов в соседях
inline void get_count_obj_grids(
#ifdef DEBUG_BUILD
		FILE * pFile,
#endif
		ErlNifEnv* env, ERL_NIF_TERM term, int* obj_count) {
	ERL_NIF_TERM head, tail;
	ERL_NIF_TERM list = term;
	const ERL_NIF_TERM* grid_state;

	(*obj_count) = 0;

	int sg, grid, arity, count;
	while (enif_get_list_cell(env, list, &head, &tail)) {

		// каждый кортеж это грид стейт
		enif_get_tuple(env, head, &arity, &grid_state);

		enif_get_list_length(env, grid_state[GRID_STATIC], &count);
		(*obj_count) += count;
		DEBUGPRINTF("count static %i\n", count);
		enif_get_list_length(env, grid_state[GRID_TICK], &count);
		(*obj_count) += count;
		DEBUGPRINTF("count tick %i\n", count);
		enif_get_list_length(env, grid_state[GRID_DYN], &count);
		(*obj_count) += count;
		DEBUGPRINTF("count dyn %i\n", count);

		list = tail;
	}
}

//-----------------------------------------------------------------------
// добавить клайм
inline void add_claim(CLAIM** list, int* count, int* capacity, CLAIM c) {
	(*count)++;
	if ((*count) >= (*capacity)) {
		(*capacity) += CLAIMS_BUFFER_CAPACITY;
		*list = enif_realloc(*list, (*capacity)*sizeof(CLAIM));
	}
	(*list)[(*count)-1] = c;
}

//-------------------------------------------------------------------------------------------
inline void fill_grids(
#ifdef DEBUG_BUILD
		FILE * pFile,
#endif
		ErlNifEnv* env, GRID * grids,
		CLAIM ** claims, int * claims_count, int * claims_capacity,
		GAME_OBJECT * objects, ERL_NIF_TERM list_term) {
	ERL_NIF_TERM head, tail;
	ERL_NIF_TERM list = list_term;
	const ERL_NIF_TERM* grid_state;
	const ERL_NIF_TERM* tuple2;
	ErlNifBinary bin;
	int sg, grid, arity;
	int i = 0;
	int obj_count = 0;
	int count;

	ERL_NIF_TERM grid_claims, head_claims, tail_claims;
	const ERL_NIF_TERM* claim_tuple;
	ERL_NIF_TERM claim_rect;
	const ERL_NIF_TERM* claim_tuple_rect;

	while (enif_get_list_cell(env, list, &head, &tail)) {
		// каждый кортеж это стейт
		enif_get_tuple(env, head, &arity, &grid_state);
		enif_get_tuple(env, grid_state[GRID_COORD], &arity, &tuple2);

		if (enif_inspect_binary(env, grid_state[GRID_TILES], &bin)) {
			memcpy(&grids[i].tile, &bin.data[0], GRID_SIZE*GRID_SIZE);
			memcpy(&grids[i].flag, &bin.data[GRID_SIZE*GRID_SIZE], GRID_SIZE*GRID_SIZE);
		} else {
			memset(grids[i].tile, 0, GRID_SIZE*GRID_SIZE);
			memset(grids[i].flag, 0, GRID_SIZE*GRID_SIZE);
		}

		if (enif_get_int(env, tuple2[0], &sg))
			grids[i].sg = sg;
		if (enif_get_int(env, tuple2[1], &grid))
			grids[i].grid = grid;

		grid_claims = grid_state[GRID_CLAIMS];
		DEBUGPRINT("parse claims...\n");
		while (enif_get_list_cell(env, grid_claims, &head_claims, &tail_claims)) {
			enif_get_tuple(env, head_claims, &arity, &claim_tuple);
			CLAIM claim;
			enif_get_int(env, claim_tuple[CLAIM_PERSONAL_OWNER_ID], &claim.owner_id);
			DEBUGPRINTF("claim owner id %i \n", claim.owner_id);
			claim_rect = claim_tuple[CLAIM_PERSONAL_RECT];
			enif_get_tuple(env, claim_rect, &arity, &claim_tuple_rect);
			enif_get_int(env, claim_tuple_rect[0], &claim.l);
			enif_get_int(env, claim_tuple_rect[1], &claim.t);
			enif_get_int(env, claim_tuple_rect[2], &claim.r);
			enif_get_int(env, claim_tuple_rect[3], &claim.b);
			claim.rect = rect( claim.l * TILE_SIZE, claim.t * TILE_SIZE, (claim.r+1) * TILE_SIZE, (claim.b+1) * TILE_SIZE );

			DEBUGPRINTF("claim rect %i %i %i %i \n", claim.l, claim.t, claim.r, claim.b);
			add_claim(claims, claims_count, claims_capacity, claim);
			grid_claims = tail_claims;
		}


		fill_obj_list(
#ifdef DEBUG_BUILD
			pFile,
#endif
				env, grid_state[GRID_STATIC], objects, obj_count, &count, TT_STATIC);
		obj_count += count;
		fill_obj_list(
#ifdef DEBUG_BUILD
			pFile,
#endif
				env, grid_state[GRID_TICK], objects, obj_count, &count, TT_TICK);
		obj_count += count;
		fill_obj_list(
#ifdef DEBUG_BUILD
			pFile,
#endif
				env, grid_state[GRID_DYN], objects, obj_count, &count, TT_DYNAMIC);
		obj_count += count;

		list = tail;
		i++;
	}

}


//-----------------------------------------------------------------------------------------------
static ERL_NIF_TERM process(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef DEBUG_BUILD
	pFile = fopen("collision.log", "a");
#endif
//-----------------------------------------------------------------------------------------------

	//-------------------------------------------------------------------------------------------
	// ОБЪЯВЛЕНИЕ ПЕРЕМЕННЫХ
	int level;
	int move_type;

	// данные карты
	GRID * grids = NULL;
	int grids_count = 0;

	// клаймы
	CLAIM * claims = NULL;
	int claims_count = 0;
	int claims_capacity;

	// объекты
	GAME_OBJECT * objects = NULL;
	int objects_count = 0;

	int oldx, oldy, x, y;
	int objid;
	int target_objid = 0;
	int arity;
	char type[ATOM_BUFFER_SIZE];
	char virtual_type[ATOM_BUFFER_SIZE];
	char move_name[ATOM_BUFFER_SIZE];
	int vx = 0;
	int vy = 0;
	int vtype = 0;

	//-------------------------------------------------------------------------------------------
	// ЗАПОЛНЕНИЕ ДАННЫХ
	// _Static, _Tick, _Dyn, _Grids, _ObjID, _Type, _OldX, _OldY, _X, _Y
	ERL_NIF_TERM grids_term = 		argv[0]; // {{sg, grid}, data}
	ERL_NIF_TERM objid_term = 		argv[1]; // objid   or   {objid, move_type, move_params}
	ERL_NIF_TERM type_term =		argv[2];
	ERL_NIF_TERM virtual_type_term =argv[3]; // {type, x, y}
	enif_get_int(env, 				argv[4], &oldx);
	enif_get_int(env, 				argv[5], &oldy);
	enif_get_int(env, 				argv[6], &x);
	enif_get_int(env, 				argv[7], &y);
	enif_get_int(env, 				argv[8], &level);
	enif_get_int(env, 				argv[9], &move_type);

	//-------------------------------------------------------------------------------------------
	// ПОДГОТОВКА
	DEBUGPRINTF("grid=%i obj=%i\n", sizeof(GRID), sizeof(GAME_OBJECT));

	const ERL_NIF_TERM* objid_tuple;
	if (enif_is_tuple(env, argv[1])) {
		// {objid, move_type, move_params}
		if (enif_get_tuple(env, objid_term, &arity, &objid_tuple)) {
			if (arity == 3) {
				enif_get_int(env, objid_tuple[0], &objid);
				enif_get_atom(env, objid_tuple[1], move_name, ATOM_BUFFER_SIZE, ERL_NIF_LATIN1);
				if (strcmp(move_name, "move_object") == 0) {
					const ERL_NIF_TERM* move_params_tuple;
					// {last_time, objid}
					if (enif_get_tuple(env, objid_tuple[2], &arity, &move_params_tuple)) {
						if (arity == 2) {
							enif_get_int(env, move_params_tuple[1], &target_objid);
							DEBUGPRINTF("move object id=%i\n", target_objid);
						}
					}
				}
			}
		}

	} else enif_get_int(env,argv[1], &objid);


	const ERL_NIF_TERM* virtual_tuple;
	enif_get_atom(env, type_term, type, ATOM_BUFFER_SIZE, ERL_NIF_LATIN1);
	// если есть виртуальный тип - получаем его
	if (enif_get_tuple(env, virtual_type_term, &arity, &virtual_tuple)) {
		if (arity == 3) {
			enif_get_atom(env, virtual_tuple[0], virtual_type, ATOM_BUFFER_SIZE, ERL_NIF_LATIN1);
			enif_get_int(env, virtual_tuple[1], & vx);
			enif_get_int(env, virtual_tuple[2], & vy);
			vtype =  get_typeid(virtual_type);
			DEBUGPRINTF("virtual type %s vx=%i vy=%i", virtual_type, vx, vy);
		} else {
			DEBUGPRINTF("error virtual tuple arity %i", arity);
		}
	} else {

	}
	enif_get_list_length(env, grids_term, &grids_count);
	grids = enif_alloc(grids_count * sizeof(GRID));

	get_count_obj_grids(
#ifdef DEBUG_BUILD
			pFile,
#endif
			env, grids_term, &objects_count);
	objects = enif_alloc(objects_count * sizeof(GAME_OBJECT));

	claims_capacity = CLAIMS_BUFFER_CAPACITY;
	claims = enif_alloc( claims_capacity * sizeof(CLAIM) );

	fill_grids(
#ifdef DEBUG_BUILD
			pFile,
#endif
			env, grids, &claims, &claims_count, &claims_capacity, objects, grids_term);
	DEBUGPRINTF("claims count %i \n", claims_count);



	//-------------------------------------------------------------------------------------------
	// ОБРАБОТКА
	int cx, cy, tile;
	int cobjid;
	ERL_NIF_TERM cobj;
	int res = check_move_collision(env,
#ifdef DEBUG_BUILD
		pFile,
#endif
		grids, grids_count,
		objects, objects_count,
		level,
		objid, oldx, oldy, x, y,
		get_typeid(type),
		target_objid,
		vtype, vx, vy,
		claims, claims_count,
		move_type,
		&cx, &cy, &tile, &cobjid, &cobj);


	//-------------------------------------------------------------------------------------------
	// ВЫДАЧА РЕЗУЛЬТАТА
//	int res = 0;
	DEBUGPRINTF( "collision res=%i tile=%i cx=%i cy=%i\n", res, tile, cx, cy);
	ERL_NIF_TERM result_term;
	switch (res) {
		case 0:
			result_term = enif_make_tuple1(env, enif_make_atom(env, "no_collision"));
			break;

		case 1: // объект
			result_term = enif_make_tuple5(env,
					enif_make_atom(env, "collision_obj"),
					enif_make_int(env, cx),
					enif_make_int(env, cy),
					cobj,
					enif_make_int(env, cobjid)
					);
			break;

		case 2: // тайл
			result_term = enif_make_tuple4(env,
					enif_make_atom(env, "collision_tile"),
					enif_make_int(env, cx),
					enif_make_int(env, cy),
					enif_make_int(env, tile)
					);
			break;

		case 3: // граница мира
			result_term = enif_make_tuple4(env,
					enif_make_atom(env, "collision_tile"),
					enif_make_int(env, cx),
					enif_make_int(env, cy),
					enif_make_int(env, 0)
					);
			break;
		case 4: // виртуальная коллизия
			result_term = enif_make_tuple3(env,
					enif_make_atom(env, "collision_virtual"),
					enif_make_int(env, cx),
					enif_make_int(env, cy)
					);
			break;

		default :
			result_term = enif_make_tuple1(env, enif_make_atom(env, "no_collision"));
			break;
	}

	enif_free(grids);
	enif_free(claims);
	enif_free(objects);

//-----------------------------------------------------------------------------------------------
#ifdef DEBUG_BUILD
	fclose(pFile);
#endif

	return result_term;
}


//-----------------------------------------------------------------------------------------------
static ErlNifFunc nif_funcs[] = {
    {"process", 10, process}
};

ERL_NIF_INIT(collision, nif_funcs, NULL, NULL, NULL, NULL)
