/*
 * collision.c
 *
 *  Created on: 31.12.2011
 *      Author: arksu
 */
#include "erl_nif.h"
#include <stdio.h>
#include <string.h>

#include "../defines.h"
#include "../nif_utils.h"
#include "../map.h"
#include "../tiles.h"
#include "collision.h"
#include <math.h>

#define TMP_BUFFER_LEN 5000


// проверить коллизию с тайлом в указанной точке. вернет тип тайла > 0 если есть коллизия
inline int check_tile_collision(
#ifdef DEBUG_BUILD
		FILE * pFile,
#endif
		GRID * grids, int grids_count,
		int level,
		int x, int y, int type, int move_type) {
	DEBUGPRINTF("check_tile_collision x=%i y=%i \n", x, y);

	int i;
	Coord gc;
//	DEBUGPRINTF("grids count=%i\n", grids_count);
	// проходим по всем гридам
	for (i=0; i<grids_count; i++) {
//		DEBUGPRINTF("grid=%i\n", i);
		gc = get_map_coord(grids[i].sg, grids[i].grid);
		if (gc.lv != level) {
			DEBUGPRINT("not my level!");
			return 0;
		}

		// проверяем принадлежат ли координаты гриду
		if (x>= gc.x && x<gc.x+GRID_FULL_SIZE && y>=gc.y && y < gc.y+GRID_FULL_SIZE) {
			// ищем тайл по индексу
			int sg, grid, index;

			get_grid_coord(x, y, level, &sg, &grid, &index);
//			DEBUGPRINTF("111  %i   %i  \n", i, index);
			int tile = grids[i].tile[index];
			DEBUGPRINTF("check tile index=%i tile=%i movetype=%i\n", index, tile, move_type);

			byte res = get_tiles_collision(move_type, type, tile);
			if (res != 0) return res;
		}
	}
	return 0;
}

// проверить выходят ли координаты за границы мира
inline int check_world_coord(int x, int y) {
	if (
			x < MIN_X_COORD+WORLD_BUFFER_SIZE ||
			x > MAX_X_COORD-WORLD_BUFFER_SIZE ||
			y < MIN_Y_COORD+WORLD_BUFFER_SIZE ||
			y > MAX_Y_COORD-WORLD_BUFFER_SIZE
	) return 1;
	return 0;
}

inline int is_follow_obj(ErlNifEnv* env, ERL_NIF_TERM head) {
	ERL_NIF_TERM params_head, params_tail;
	const ERL_NIF_TERM* param_tuple;
	ERL_NIF_TERM params;
	char param_name_atom[ATOM_BUFFER_SIZE];
	int arity;

	// получаем параметры
	get_obj_params(env, head, &params);

	// смотрим есть ли в параметрах follow
	while (enif_get_list_cell(env, params, &params_head, &params_tail)) {
		enif_get_tuple(env, params_head, &arity, &param_tuple);
		if (arity == 2) {
			enif_get_atom(env, param_tuple[0], param_name_atom, ATOM_BUFFER_SIZE, ERL_NIF_LATIN1);
			if (strcmp("follow", param_name_atom) == 0) {
				//DEBUGPRINTF("follow detect %i\n", id);
				return 1;
			}
		}
		params = params_tail;
	}
	return 0;
}

// обсчитать коллизии
inline int check_move_collision(
		ErlNifEnv* env,
#ifdef DEBUG_BUILD
		FILE * pFile,
#endif
		GRID * grids, int grids_count,
		GAME_OBJECT * objects, int objects_count,
		int level,
		int id, int oldx, int oldy, int x, int y,
		int type,
		int target_objid,
		int virtual_type, int vx, int vy,
		CLAIM * claims, int claims_count,
		int move_type,
		int * collision_x, int * collision_y, int * tile, int * cobj_id, ERL_NIF_TERM * cobj) {

	*tile = 0;
	*collision_x = oldx;
	*collision_y = oldy;
	*cobj_id = 0;

	GAME_OBJECT * obj_buffer = enif_alloc(TMP_BUFFER_LEN * sizeof(GAME_OBJECT));
	int obj_buffer_count;

	if (check_world_coord(oldx, oldy) || check_world_coord(x, y)) return 3;

	DEBUGPRINTF("check collision oldx=%i oldy=%i x=%i y=%i\n", oldx, oldy, x, y);
	obj_buffer_count = 0;
	int i;
	for (i=0; i<objects_count; i++) {

		if (is_follow_obj(env, objects[i].obj)) {
			DEBUGPRINTF("follow %i\n", objects[i].id);
		}
		// фильтруем список
		// объект к которому движемся мы - дает всегда коллизию, нужно для корректного взаимодействия с ним
		if (target_objid == objects[i].id) {

		}
			if (
					(
					//c.lv != level
					objects[i].x < oldx-COLLISION_DISTANCE
					|| objects[i].x > oldx+COLLISION_DISTANCE
					|| objects[i].y < oldy-COLLISION_DISTANCE
					|| objects[i].y > oldy+COLLISION_DISTANCE

					|| objects[i].id == id

					|| (!get_collision(objects[i].type, type))
					|| (!get_collision_object_state(env, type, objects[i].type, objects[i].obj))
					// то что мы несем на себе - не дает коллизий
					|| (is_follow_obj(env, objects[i].obj))
					) && (
							(target_objid != objects[i].id) ||
							(target_objid == objects[i].id && !get_collision_object_to_move(objects[i].type, type))
					)
			) continue;

		obj_buffer[obj_buffer_count] = objects[i];
		obj_buffer_count++;

		if (obj_buffer_count >= TMP_BUFFER_LEN) {
			DEBUGPRINTF("ERROR: tmp obj buffer overflow %i\n", obj_buffer_count);
			break;
		}
	}
	DEBUGPRINTF("filtered obj count %i\n", obj_buffer_count);

	double nx, ny;
	int need_exit = 0;
	double cx = oldx;
	double cy = oldy;

	int l, t, r, b;
	int vl, vt, vr, vb; // virtual
	int lo, to, ro, bo;
	get_bounds(type, &l, &t, &r, &b);
	get_bounds(virtual_type, &vl, &vt, &vr, &vb);
	RECT rr, rro, vrr;
	rr = rect_from_bound(oldx, oldy, l, t, r, b);
	vrr = rect_from_bound(vx, vy, vl, vt, vr, vb);

	// проверяем коллизию на исходных координатах
	// если есть - выходим
	for (i=0; i<obj_buffer_count; i++) {
		Coord oc = get_obj_coord(env, obj_buffer[i].obj);
		get_bounds(obj_buffer[i].type, &lo, &to, &ro, &bo);
		rro = rect_from_bound(oc.x, oc.y, lo, to, ro, bo);

			if (is_rect_intersect(rr, rro)) {
				*cobj_id = obj_buffer[i].id;
				*cobj = obj_buffer[i].obj;

				//DEBUGPRINTF("collid %i\n", get_obj_id(*cobj));

				DEBUGPRINTF("collision at begin! id=%i\n", obj_buffer[i].id);
				enif_free(obj_buffer);
				return 1;
			}
	}
	//----------------------------
	// проверим тайл
	int tile_collision = check_tile_collision(
#ifdef DEBUG_BUILD
			pFile,
#endif
			grids, grids_count, level,
			oldx, oldy, type, move_type);
	if (tile_collision) {
		*tile = tile_collision;
		enif_free(obj_buffer);
		return 2;
	}
	//------------------------------
	// проверим виртуальную коллизию
	if (virtual_type > 0 && get_collision(virtual_type, type) && is_rect_intersect(rr, vrr)) {
		DEBUGPRINT("virtual collision at begin!\n");
		enif_free(obj_buffer);
		return 4;
	}
	//------------------------------
	// проверим клаймы
//	if (type == TYPE_PLAYER)
//	for (i=0; i<claims_count; i++) {
//		if (claims[i].owner_id != id && is_rect_intersect(rr, claims[i].rect)) {
//			DEBUGPRINTF("claim collision %i \n", claims[i].owner_id);
//			*tile = 0;
//			enif_free(obj_buffer);
//			return 2;
//		}
//	}



	int cycles = 0;
	// теперь начинаем итеративно двигаться и проверяем коллизии на каждой итерации
	while (1) {
		// если текущее расстояние меньше итерации - возьмем конечную точку
		double d = distance(cx, cy, x, y);
		DEBUGPRINTF("distance=%f cx=%f cy=%f rect=%i %i %i %i\n", d, cx, cy, l, t, r, b);
		if (d < COLLISION_ITERATION_LENGTH) {
			nx = x;
			ny = y;
			need_exit = 1;
		} else {
			double k = COLLISION_ITERATION_LENGTH / d;
			nx = cx + (x - cx) * k;
			ny = cy + (y - cy) * k;
		}
		DEBUGPRINTF("current nx=%f ny=%f", nx, ny);

		// переводим текущие координаты в инт
		int inx = round(nx);
		int iny = round(ny);

		// проверяем коллизию на текущих координатах
		rr = rect_from_bound(inx, iny, l, t, r, b);
		for (i=0; i<obj_buffer_count; i++) {
			get_bounds(obj_buffer[i].type, &lo, &to, &ro, &bo);
			DEBUGPRINTF("obj coord %i %i bounds %i %i %i %i\n", obj_buffer[i].x, obj_buffer[i].y, lo, to, ro, bo);
			rro = rect_from_bound(obj_buffer[i].x, obj_buffer[i].y, lo, to, ro, bo);
			DEBUGPRINTF("obj rect=%i %i %i %i\n", rro.left, rro.top, rro.right, rro.bottom);

			// дает ли объект коллизию
				if (is_rect_intersect(rr, rro)) {
					*collision_x = round(cx);
					*collision_y = round(cy);
					*cobj_id = obj_buffer[i].id;
					*cobj = obj_buffer[i].obj;
					//DEBUGPRINTF("collid %i\n", get_obj_id(cobj));
					DEBUGPRINTF("collision id=%i cx=%i cy=%i\n", obj_buffer[i].id, *collision_x, *collision_y);
					enif_free(obj_buffer);
					return 1;
				}
		}

		//-------------------------------
		// проверяем коллизию с тайлом на текущих координатах
		tile_collision = check_tile_collision(
#ifdef DEBUG_BUILD
			pFile,
#endif
			grids, grids_count, level,
			inx, iny, type, move_type);
		if (tile_collision) {
			*tile = tile_collision;
			*collision_x = round(cx);
			*collision_y = round(cy);
			DEBUGPRINTF("tile collision tile=%i cx=%i cy=%i\n", *tile, *collision_x, *collision_y);
			enif_free(obj_buffer);
			return 2;
		}

		//------------------------------
		// проверим виртуальную коллизию
		if (virtual_type > 0 && get_collision(virtual_type, type) && is_rect_intersect(rr, vrr)) {
			DEBUGPRINT("virtual collision!\n");
			*collision_x = round(cx);
			*collision_y = round(cy);
			enif_free(obj_buffer);
			return 4;
		}

		//------------------------------
		// проверим клаймы
		if (type == TYPE_PLAYER)
		for (i=0; i<claims_count; i++) {
			if (claims[i].owner_id != id && is_rect_intersect(rr, claims[i].rect)) {
				*collision_x = round(cx);
				*collision_y = round(cy);
				*tile = 0;
				DEBUGPRINTF("claim collision %i [%i %i %i %i] \n", claims[i].owner_id, claims[i].rect.left, claims[i].rect.top, claims[i].rect.right, claims[i].rect.bottom);
				enif_free(obj_buffer);
				return 2;
			}
		}

		//----------------------------------
		// проверим выход за границы мира
		if (check_world_coord(inx, iny)) {
			*collision_x = round(cx);
			*collision_y = round(cy);
			DEBUGPRINT("off world!");
			enif_free(obj_buffer);
			return 3;
		}

		// если нет. смотрим нужно ли выйти
		if (need_exit) {
			// координаты не важны. главное что нет коллизии. и двигаемся в конечную точку
			*collision_x = inx;
			*collision_y = iny;
			cobj_id = 0;
			DEBUGPRINT("no collision\n");
			// выходим. говорим нет коллизии
			enif_free(obj_buffer);
			return 0;
		}

		cx = nx;
		cy = ny;

		cycles++;
		if (cycles > 20) {
			DEBUGPRINT("FAIL!");
			enif_free(obj_buffer);
			return 0;
		}
	}
	return 0;
}
