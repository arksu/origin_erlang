/*
 * main_nif.c
 *
 *  Created on: 08.01.2012
 *      Author: arksu
 */

#include <stdio.h>
#include <string.h>
#include "erl_nif.h"
#include "../defines.h"
#include "../map.h"
#include "grid_processor.h"

//-------------------------------------------------------------------------------------------
// заполнить список объектов из листа
inline void fill_obj_list(ErlNifEnv* env, ERL_NIF_TERM term, ERL_NIF_TERM* obj_list, int offset, int* count) {
	ERL_NIF_TERM head, tail;
	ERL_NIF_TERM list = term;

	(*count) = 0;
	while (enif_get_list_cell(env, list, &head, &tail)) {
		obj_list[offset + (*count)] = head;
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
		ErlNifEnv* env, ERL_NIF_TERM term, int* work_count, int* around_count, int my_sg, int my_grid) {
	ERL_NIF_TERM head, tail;
	ERL_NIF_TERM list = term;
	const ERL_NIF_TERM* tuple;
	const ERL_NIF_TERM* tuple2;
	const ERL_NIF_TERM* grid_state;

	(*work_count) = 0;
	(*around_count) = 0;

	ErlNifBinary bin;
	int sg, grid, arity, count;
	//DEBUGPRINT("00\n");
	while (enif_get_list_cell(env, list, &head, &tail)) {
		//DEBUGPRINT("222\n");

		// каждый кортеж это #neighbor_rec, надо дернуть из него стейт = NEIGHBOR_REC_STATE
		enif_get_tuple(env, head, &arity, &tuple);
		//DEBUGPRINT("111\n");
		enif_get_tuple(env, tuple[NEIGHBOR_REC_STATE], &arity, &grid_state);

		enif_get_tuple(env, grid_state[GRID_COORD], &arity, &tuple2);

		// {Sg, Grid}
		enif_get_int(env, tuple2[0], &sg);
		enif_get_int(env, tuple2[1], &grid);
		DEBUGPRINTF("neighbor %i %i \n", sg, grid);

		if (sg == my_sg && grid == my_grid) {
			enif_get_list_length(env, grid_state[GRID_STATIC], &count);
			(*work_count) += count;
			enif_get_list_length(env, grid_state[GRID_TICK], &count);
			(*work_count) += count;
			enif_get_list_length(env, grid_state[GRID_DYN], &count);
			(*work_count) += count;
		} else {
			enif_get_list_length(env, grid_state[GRID_STATIC], &count);
			(*around_count) += count;
			enif_get_list_length(env, grid_state[GRID_TICK], &count);
			(*around_count) += count;
			enif_get_list_length(env, grid_state[GRID_DYN], &count);
			(*around_count) += count;

		}

		list = tail;
	}
}

//-------------------------------------------------------------------------------------------
inline void fill_grids(
#ifdef DEBUG_BUILD
		FILE * pFile,
#endif
		ErlNifEnv* env, ERL_NIF_TERM term, byte* tile, byte* flag, ERL_NIF_TERM* around, ERL_NIF_TERM* work, int my_sg, int my_grid) {
//	DEBUGPRINT("rrr\n");
	ERL_NIF_TERM head, tail;
	ERL_NIF_TERM list = term;
	const ERL_NIF_TERM* tuple;
	const ERL_NIF_TERM* tuple2;
	const ERL_NIF_TERM* grid_state;
	ErlNifBinary bin;
	int sg, grid, arity;
	int count;
	int work_offset = 0;
	int around_offset = 0;

	// координаты грида
	Coord my_grid_coord = get_map_coord(my_sg, my_grid);
	// координаты гридов
	int my_gridx = my_grid_coord.x / GRID_FULL_SIZE;
	int my_gridy = my_grid_coord.y / GRID_FULL_SIZE;


	while (enif_get_list_cell(env, list, &head, &tail)) {

		// каждый кортеж это #neighbor_rec, надо дернуть из него стейт = NEIGHBOR_REC_STATE
		enif_get_tuple(env, head, &arity, &tuple);
		enif_get_tuple(env, tuple[NEIGHBOR_REC_STATE], &arity, &grid_state);
		enif_get_tuple(env, grid_state[GRID_COORD], &arity, &tuple2);

		// {Sg, Grid}
		enif_get_int(env, tuple2[0], &sg);
		enif_get_int(env, tuple2[1], &grid);

		if (sg == my_sg && grid == my_grid) {
			fill_obj_list(env, grid_state[GRID_STATIC], work, work_offset, &count);
			work_offset += count;
			fill_obj_list(env, grid_state[GRID_TICK], work, work_offset, &count);
			work_offset += count;
			fill_obj_list(env, grid_state[GRID_DYN], work, work_offset, &count);
			work_offset += count;
		} else {
			fill_obj_list(env, grid_state[GRID_STATIC], around, around_offset, &count);
			around_offset += count;
			fill_obj_list(env, grid_state[GRID_TICK], around, around_offset, &count);
			around_offset += count;
			fill_obj_list(env, grid_state[GRID_DYN], around, around_offset, &count);
			around_offset += count;
		}


		// только если тайлы действительно бирарные данные
		if (enif_inspect_binary(env, grid_state[GRID_TILES], &bin)) {
			if (bin.size == GRID_SIZE*GRID_SIZE*2)
			{

				// координаты грида
				Coord grid_coord = get_map_coord(sg, grid);
				int gx = grid_coord.x / GRID_FULL_SIZE;
				int gy = grid_coord.y / GRID_FULL_SIZE;

				DEBUGPRINTF("gcx=%i gcy=%i sg=%i grid=%i gx=%i gy=%i mygx=%i mygy=%i\n", grid_coord.x, grid_coord.y, sg, grid, gx, gy, my_gridx, my_gridy);

				// надо 3 массива скопировать в нужные места в общей таблице гридов
				int ax, ay, bi;
				ax = gx - my_gridx + 1;
				ay = gy - my_gridy + 1;
				DEBUGPRINTF( "ax=%i ay=%i\n", ax, ay);

				int i, ii;
				// копируем построчно
				for (i = 0; i < GRID_SIZE; ++i) {
					ii = ay * 3*GRID_SIZE*GRID_SIZE + ax * GRID_SIZE + i * 3*GRID_SIZE;
		//			DEBUGPRINTF("ax=%i ay=%i i=%i, ii=%i\n", ax, ay, i, ii);
					memcpy(&tile[ii], &bin.data[i * GRID_SIZE], GRID_SIZE);
					memcpy(&flag[ii], &bin.data[i * GRID_SIZE+GRID_SIZE*GRID_SIZE], GRID_SIZE);
				}
			} else {
				DEBUGPRINTF( "bin size tiles! sg=%i grid=%i size=%i\n", sg, grid, bin.size);
			}
		} else {
			DEBUGPRINTF( "havnt bin tiles! sg=%i grid=%i\n", sg, grid);
		}

		list = tail;
	}
}
//-------------------------------------------------------------------------------------------
static ERL_NIF_TERM process(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef DEBUG_BUILD
	FILE * pFile;
	pFile = fopen("grid_processor.log", "a");
#endif
	DEBUGPRINT("begin...\n");
	//-------------------------------------------------------------------------------------------
	// ОБЪЯВЛЕНИЕ ПЕРЕМЕННЫХ
	int sg;
	int grid;
	// глобальное время сервера
	int global_time;
	// нужен ли спавн объектов
	int need_spawn;
	// данные для обработки карты
	byte src_tile[9*GRID_SIZE*GRID_SIZE];
	byte src_flag[9*GRID_SIZE*GRID_SIZE];
	byte out_tiles[2*GRID_SIZE*GRID_SIZE]; // весь массив грида сразу без разбивки на тайлы и флаги
	int tiles_changed; // изменились ли тайлы в процессе обработки. чтобы не слать зря весь грид на клиент
	// обнуляем массив тайлов
	memset(src_tile, 0, 9*GRID_SIZE*GRID_SIZE);
	memset(src_flag, 0, 9*GRID_SIZE*GRID_SIZE);
	ERL_NIF_TERM* new_obj;
	ERL_NIF_TERM* changed_obj;
	ERL_NIF_TERM* deleted_obj;
	// количество объектов
	int changed_obj_count = 0;
	int deleted_obj_count= 0 ;
	int new_obj_count = 0;

	//-------------------------------------------------------------------------------------------
	// ЗАПОЛНЕНИЕ ДАННЫХ

	//(_Neighbors, _Sg, _Grid, _GTime, _LastTime),
	ERL_NIF_TERM neighbors_term = 	argv[0];
	enif_get_int(env, 				argv[1], &sg);
	enif_get_int(env, 				argv[2], &grid);
	enif_get_int(env, 				argv[3], &global_time);
	enif_get_int(env, 				argv[4], &need_spawn);

	// _Neighbors = [#neighbor_rec]

	//-------------------------------------------------------------------------------------------
	// ПОДГОТОВКА
	DEBUGPRINTF("global time %i\n", global_time);
	srand(global_time);

	int around_obj_count;
	int work_obj_count;

	// получим суммарное количество объектов на входе
	DEBUGPRINT("get count...\n");
	get_count_obj_grids(
#ifdef DEBUG_BUILD
			pFile,
#endif
			env, neighbors_term, &work_obj_count, &around_obj_count, sg, grid);
	DEBUGPRINTF("work=%i around=%i\n",work_obj_count, around_obj_count);

	// выделим память
	ERL_NIF_TERM* around_obj = enif_alloc(sizeof(ERL_NIF_TERM) * around_obj_count);
	ERL_NIF_TERM* work_obj = enif_alloc(sizeof(ERL_NIF_TERM) * work_obj_count);

	DEBUGPRINTF("sg=%i grid=%i work=%i around=%i \n", sg, grid, work_obj_count, around_obj_count);

	Coord grid_coord;
	DEBUGPRINT("fill grid tiles...\n");
	fill_grids(
#ifdef DEBUG_BUILD
			pFile,
#endif
			env, neighbors_term, src_tile, src_flag, around_obj, work_obj, sg, grid);
	DEBUGPRINT("tiles ok\n");

	// объем буферов
	int changed_capacity, deleted_capacity, new_capacity;
	changed_capacity = deleted_capacity = new_capacity = OBJ_BUFFER_CAPACITY;
	// выделяем память под буферы
	changed_obj = 	enif_alloc(changed_capacity*sizeof(ERL_NIF_TERM));
	deleted_obj = 	enif_alloc(deleted_capacity*sizeof(ERL_NIF_TERM));
	new_obj = 		enif_alloc(new_capacity*sizeof(ERL_NIF_TERM));

	// ОБРАБОТКА
	//-------------------------------------------------------------------------------------------
	process_tiles(env,
#ifdef DEBUG_BUILD
			pFile,
#endif
			src_tile, src_flag, out_tiles,
			&tiles_changed);
	//-------------------------------------------------------------------------------------------
	process_obj(env,
#ifdef DEBUG_BUILD
			pFile,
#endif
			global_time, sg, grid,
			src_tile, src_flag, out_tiles,
			around_obj, work_obj,
			around_obj_count, work_obj_count,
			need_spawn,
			&changed_obj, &deleted_obj, &new_obj,
			&changed_obj_count, &deleted_obj_count, &new_obj_count,
			&changed_capacity, &deleted_capacity, &new_capacity
			);

	//-------------------------------------------------------------------------------------------
	// ВЫДАЧА РЕЗУЛЬТАТА
	ErlNifBinary out_map_bin;
	enif_alloc_binary(2*GRID_SIZE*GRID_SIZE, &out_map_bin);
	out_map_bin.size = 2*GRID_SIZE*GRID_SIZE;
	memcpy(out_map_bin.data, out_tiles, 2*GRID_SIZE*GRID_SIZE);
	ERL_NIF_TERM result_tiles_changed = enif_make_int(env, tiles_changed);
	ERL_NIF_TERM result_map = enif_make_binary(env, &out_map_bin);

	DEBUGPRINTF("changed=%i del=%i new=%i\n", changed_obj_count, deleted_obj_count, new_obj_count);


	ERL_NIF_TERM result_changed = 	enif_make_list_from_array(env, changed_obj, changed_obj_count);
	ERL_NIF_TERM result_deleted = 	enif_make_list_from_array(env, deleted_obj, deleted_obj_count);
	ERL_NIF_TERM result_new = 		enif_make_list_from_array(env, new_obj, new_obj_count);

	enif_free(around_obj);
	enif_free(work_obj);

	enif_free(changed_obj);
	enif_free(deleted_obj);
	enif_free(new_obj);

#ifdef DEBUG_BUILD
	fclose(pFile);
#endif
	return enif_make_tuple5(env, result_tiles_changed, result_map, result_changed, result_deleted, result_new);
	//{ResultMap, ResultChanged, ResultDeleted, ResultNew}
}
//-------------------------------------------------------------------------------------------
static ErlNifFunc nif_funcs[] = {
    {"process", 5, process}
};

ERL_NIF_INIT(grid_processor, nif_funcs, NULL, NULL, NULL, NULL)
