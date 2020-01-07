/*
 * process_obj.c
 *
 *  Created on: 22.10.2011
 *  refactor on: 08.01.2012
 *      Author: arksu
 */

#include <stdio.h>
#include "erl_nif.h"
#include "../defines.h"
#include "../nif_utils.h"
#include "grid_processor.h"

#define GET_TILE(TX, TY)		src_tile[3*GRID_SIZE*GRID_SIZE + GRID_SIZE + (TY)*3*GRID_SIZE + (TX)]
#define GET_FLAG(TX, TY)		src_flag[3*GRID_SIZE*GRID_SIZE + GRID_SIZE + (TY)*3*GRID_SIZE + (TX)]


// #object
#define COPY_OBJ(FROM, TO) enif_get_tuple(env, FROM, &obj_arity, &obj_tuple); \
		TO[0] = obj_tuple[0]; \
		TO[1] = obj_tuple[1]; \
		TO[2] = obj_tuple[2]; \
		TO[3] = obj_tuple[3]; \
		TO[4] = obj_tuple[4]; \
		TO[5] = obj_tuple[5]; \
		TO[6] = obj_tuple[6]; \
		TO[7] = obj_tuple[7]; \
		TO[8] = obj_tuple[8]

#define GENERATE_OBJ(TYPE, OX, OY, OLV, HP, Q) 	enif_make_tuple9(env, \
		enif_make_atom(env, "object"), \
		enif_make_atom(env, "none"), \
		enif_make_atom(env, TYPE), \
		enif_make_tuple3(env,  enif_make_int(env, OX),  enif_make_int(env, OY), enif_make_int(env, OLV)), \
		enif_make_list(env, 0), \
		enif_make_atom(env, "none"), \
		enif_make_int(env, global_time), \
		enif_make_tuple2(env, \
				enif_make_int(env, HP), \
				enif_make_int(env, HP) \
		), \
		enif_make_int(env, Q) \
		)


//-----------------------------------------------------------------------
inline void add_obj(ERL_NIF_TERM** list, int* count, int* capacity, ERL_NIF_TERM obj) {
	(*count)++;
	if ((*count) >= (*capacity)) {
		(*capacity) += OBJ_BUFFER_CAPACITY;
		*list = enif_realloc(*list, (*capacity)*sizeof(ERL_NIF_TERM));
	}
	(*list)[(*count)-1] = obj;
}



inline void process_obj(ErlNifEnv* env,
#ifdef DEBUG_BUILD
	FILE * pFile,
#endif
	int global_time, int sg, int grid,
	byte* src_tile, byte* src_flag, byte* out_tiles,
	ERL_NIF_TERM* around_obj, ERL_NIF_TERM* work_obj,
	int around_obj_count, int work_obj_count,
	int need_spawn,
	ERL_NIF_TERM** changed_obj, ERL_NIF_TERM** deleted_obj, ERL_NIF_TERM** new_obj,
	int* changed_obj_count, int* deleted_obj_count, int* new_obj_count,
	int* changed_capacity, int* deleted_capacity, int* new_capacity
	) {

	int i, j, arity, obj_arity;
	int objid, stage, time;


	ERL_NIF_TERM obj;
	ERL_NIF_TERM state;
	const ERL_NIF_TERM* obj_tuple;
	const ERL_NIF_TERM* state_copy_tuple;
	char type[ATOM_BUFFER_SIZE];
	ERL_NIF_TERM new_object[OBJ_FIELDS_COUNT];

	// есть ли в гриде дикое растение?
	int wild_count = 0;
	// сколько кроликов в гриде
	int rabbit_count = 0;
	// стринги
	int string_count = 0;

	DEBUGPRINTF("process objects [%i]...\n", work_obj_count);

	for (i = 0; i < work_obj_count; ++i) {
		obj = work_obj[i];
		get_obj_type(env, obj, type);
		get_obj_id(env, obj, &objid);

		//DEBUGPRINTF("id=%i type=%s\n", objid, type);

		if (strcmp(type, "plant_wild") == 0) wild_count++;
		if (strcmp(type, "rabbit") == 0) rabbit_count++;
		if (strcmp(type, "string_wild") == 0) string_count++;


		//-----------------------------------------------------------------------------------------------------------
		// ищем морковку
		if (strcmp(type, "plant_carrot") == 0) {
			DEBUGPRINTF("its carrot! id=%i\n", objid);
			get_obj_state(env, obj, &state);
			// дергаем стадию из стейта объекта
			// #obj_plant
			ERL_NIF_TERM stage_term = get_tuple_elem(env, state, 2);
			int stage;
			if (enif_get_int(env, stage_term, &stage)) {
				// если стадия меньше 3 - проверяем время.
				if (stage < 3) {
					get_obj_time(env, obj, &time);
					DEBUGPRINTF("stage=%i time=%i timediff=%i\n", stage, time, (global_time - time));
					// если разница времени больше
					//if (global_time - time > 12*HOUR) {
					if (global_time - time > 5) { // 5 sec
						// нужно обновить стадию и время. добавить в измененные объекты
						COPY_OBJ(obj, new_object);

						// прибавляем стадию и обновляем время
						enif_get_tuple(env, state, &arity, &state_copy_tuple);

						// #obj_plant
						new_object[OBJ_STATE] = enif_make_tuple4(env,
								state_copy_tuple[0],
								state_copy_tuple[1],
								enif_make_int(env, stage+1),
								state_copy_tuple[3] );
						new_object[OBJ_TIME] = enif_make_int(env, global_time);

						obj = enif_make_tuple_from_array(env, new_object, OBJ_FIELDS_COUNT);
						// помещаем в список изменившихся объектов
						add_obj(changed_obj, changed_obj_count, changed_capacity, obj);
					}
				}
			}
		}
		//-----------------------------------------------------------------------------------------------------------
		// ищем пшеницу
		if (strcmp(type, "plant_wheat") == 0) {
			DEBUGPRINTF("its wheat! id=%i\n", objid);
			get_obj_state(env, obj, &state);
			// дергаем стадию из стейта объекта
			// #obj_plant
			ERL_NIF_TERM stage_term = get_tuple_elem(env, state, 2);
			int stage;
			if (enif_get_int(env, stage_term, &stage)) {
				// если стадия меньше 3 - проверяем время.
				if (stage < 3) {
					get_obj_time(env, obj, &time);
					DEBUGPRINTF("stage=%i time=%i timediff=%i\n", stage, time, (global_time - time));
					// если разница времени больше
					//if (global_time - time > 12*HOUR) {
					if (global_time - time > 5) {
						// нужно обновить стадию и время. добавить в измененные объекты
						COPY_OBJ(obj, new_object);

						// прибавляем стадию и обновляем время
						enif_get_tuple(env, state, &arity, &state_copy_tuple);

						// #obj_plant
						new_object[OBJ_STATE] = enif_make_tuple4(env,
								state_copy_tuple[0],
								state_copy_tuple[1],
								enif_make_int(env, stage+1),
								state_copy_tuple[3] );
						new_object[OBJ_TIME] = enif_make_int(env, global_time);

						obj = enif_make_tuple_from_array(env, new_object, OBJ_FIELDS_COUNT);
						// помещаем в список изменившихся объектов
						add_obj(changed_obj, changed_obj_count, changed_capacity, obj);
					}
				}
			}
		}

//		//-----------------------------------------------------------------------------------------------------------
//		// ищем дикое растение посаженное игроком
//		if (strcmp(type, "plant_wildhome") == 0) {
//			DEBUGPRINTF("its wild! id=%i\n", objid);
//			get_obj_state(env, obj, &state);
//			// дергаем стадию из стейта объекта
//			// #obj_plant
//			ERL_NIF_TERM stage_term = get_tuple_elem(env, state, 2);
//			int stage;
//			if (enif_get_int(env, stage_term, &stage)) {
//				// если стадия меньше 3 - проверяем время.
//				if (stage < 3) {
//					get_obj_time(env, obj, &time);
//					DEBUGPRINTF("stage=%i time=%i timediff=%i\n", stage, time, (global_time - time));
//					// если разница времени больше
//					if (global_time - time > 12*HOUR) {
//					//if (global_time - time > 10) {
//						// нужно обновить стадию и время. добавить в измененные объекты
//						COPY_OBJ(obj, new_object);
//
//						// прибавляем стадию и обновляем время
//						enif_get_tuple(env, state, &arity, &state_copy_tuple);
//
//						// #obj_plant
//						new_object[OBJ_STATE] = enif_make_tuple4(env,
//								state_copy_tuple[0],
//								state_copy_tuple[1],
//								enif_make_int(env, stage+1),
//								state_copy_tuple[3] );
//						new_object[OBJ_TIME] = enif_make_int(env, global_time);
//
//						obj = enif_make_tuple_from_array(env, new_object, OBJ_FIELDS_COUNT);
//						// помещаем в список изменившихся объектов
//						add_obj(changed_obj, changed_obj_count, changed_capacity, obj);
//					}
//				}
//			}
//		}

//		//-----------------------------------------------------------------------------------------------------------
//		// удаление устаревшей вспашки
//		if (strcmp(type, "plow") == 0) {
//			get_obj_time(env, obj, &time);
//			DEBUGPRINTF("its plow! id=%i timediff=%i\n", objid, global_time - time);
//			// если прошло больше времени жизни пашни
//			//if (global_time - time > 12*HOUR) {
//			if (global_time - time > 10) {
//				Coord c = get_obj_coord(env, obj);
//				DEBUGPRINTF("x=%i y=%i lv=%i\n", c.x, c.y, c.lv);
//				// удаляем объект с определенной вероятностью
//				if ((rand() % 10) == 5) {  // 1 из 10
//					int sg = get_sg(c.x, c.y, c.lv);
//					DEBUGPRINTF("delete plow! sg=%i\n", sg);
//					ERL_NIF_TERM del_obj = enif_make_tuple2(env,
//							enif_make_int(env, objid),
//							enif_make_int(env, sg)
//							);
//					add_obj(deleted_obj, deleted_obj_count, deleted_capacity, del_obj);
//				}
//			}
//
//		}
	}

	DEBUGPRINT("all objects passed\n");

	// если в гриде нет дикого растения и шанс выпадения высок - добавим его
	if ((wild_count < 5) && (rand() % 2) == 1) { // 1 из 2
		// ищем тайл с травой
		int wx, wy, level, tile;
		for (i=0; i<GRID_SIZE; i++) {
			wx = (rand() % GRID_SIZE);
			wy = (rand() % GRID_SIZE);
			byte tile = GET_TILE(wx, wy);
			if (tile == TILE_GRASS) {
				Coord my_grid_coord = get_map_coord(sg, grid);
				// спавним по центру тайла
				wx = my_grid_coord.x + wx * TILE_SIZE + TILE_SIZE / 2;
				wy = my_grid_coord.y + wy * TILE_SIZE + TILE_SIZE / 2;
				level = my_grid_coord.lv;
				DEBUGPRINTF("-------------spawn wild! %i %i\n", wx, wy);
				ERL_NIF_TERM wobj = GENERATE_OBJ("plant_wild", wx, wy, level, 100, 10);
				add_obj(new_obj, new_obj_count, new_capacity, wobj);
				break;
			}
		}
	}

	// если в гриде нет дикого растения и шанс выпадения высок - добавим его
	if ((string_count < 3) && (rand() % 5) == 1) { // 1 из 2
		// ищем тайл с травой
		int wx, wy, level, tile;
		for (i=0; i<GRID_SIZE; i++) {
			wx = (rand() % GRID_SIZE);
			wy = (rand() % GRID_SIZE);
			byte tile = GET_TILE(wx, wy);
			if (tile == TILE_FOREST_FIR || tile == TILE_FOREST_LEAF) {
				Coord my_grid_coord = get_map_coord(sg, grid);
				// спавним по центру тайла
				wx = my_grid_coord.x + wx * TILE_SIZE + TILE_SIZE / 2;
				wy = my_grid_coord.y + wy * TILE_SIZE + TILE_SIZE / 2;
				level = my_grid_coord.lv;
				DEBUGPRINTF("-------------spawn string! %i %i\n", wx, wy);
				ERL_NIF_TERM wobj = GENERATE_OBJ("string_wild", wx, wy, level, 100, 10);
				add_obj(new_obj, new_obj_count, new_capacity, wobj);
				break;
			}
		}
	}


	// SPAWN
	if (need_spawn) {
		// если в гриде нет и шанс выпадения высок - добавим его
		if ((rabbit_count < 2) && (rand() % 2) == 1) { // 1 из 2
			// ищем тайл с травой
			int wx, wy, level, tile;
			for (i=0; i<GRID_SIZE; i++) {
				wx = (rand() % GRID_SIZE);
				wy = (rand() % GRID_SIZE);
				byte tile = GET_TILE(wx, wy);
				if (tile == TILE_FOREST_FIR || tile == TILE_FOREST_LEAF) {
					Coord my_grid_coord = get_map_coord(sg, grid);
					// спавним по центру тайла
					wx = my_grid_coord.x + wx * TILE_SIZE + TILE_SIZE / 2;
					wy = my_grid_coord.y + wy * TILE_SIZE + TILE_SIZE / 2;
					level = my_grid_coord.lv;
					DEBUGPRINTF("-------------spawn rabbit! %i %i\n", wx, wy);
					ERL_NIF_TERM wobj = GENERATE_OBJ("rabbit", wx, wy, level, 100, 10);
					add_obj(new_obj, new_obj_count, new_capacity, wobj);
					break;
				}
			}
		}
	}

}

//-----------------------------------------------------------------------

