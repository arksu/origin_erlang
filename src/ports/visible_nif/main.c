/*
 * main.c
 *
 *  Created on: 04.01.2012
 *      Author: arksu
 */
#include <stdio.h>
#include <math.h>
#include "erl_nif.h"
#include "../defines.h"
#include "../map.h"

// сколько всего объектов может принимать участие в расчетах
#define MAX_OBJECT_BUFFER_SIZE 5000
// сколько времени будет виден объект после того как пропал из области видимости
#define VISIBLE_TIME 5


typedef struct {
	int x;				//
	int y;				//
	int id;				// ид объекта
	int trans;			// прозрачен ли объект
	int stealth;		// скрытность
	int type;			// тип
	int time;			// время последнего апдейта когда объект видим
	ERL_NIF_TERM term; 	// исходный терм
} VISIBLE_OBJECT;


inline void fill_obj_list(ErlNifEnv* env, VISIBLE_OBJECT* obj_list, ERL_NIF_TERM term) {
	ERL_NIF_TERM head, tail;
	int i = 0;
	ERL_NIF_TERM list = term;
	const ERL_NIF_TERM* tuple;
	int arity;
	while (enif_get_list_cell(env, list, &head, &tail) && i<MAX_OBJECT_BUFFER_SIZE) {

		enif_get_tuple(env, head, &arity, &tuple);

		// каждый кортеж это #visible_obj
		enif_get_int(env, tuple[1], &obj_list[i].x);
		enif_get_int(env, tuple[2], &obj_list[i].y);
		enif_get_int(env, tuple[3], &obj_list[i].id);
		enif_get_int(env, tuple[4], &obj_list[i].trans);
		enif_get_int(env, tuple[5], &obj_list[i].stealth);
		enif_get_int(env, tuple[6], &obj_list[i].type);
		enif_get_int(env, tuple[7], &obj_list[i].time);

		obj_list[i].term = head;

		list = tail;
		i++;
	}

}

inline ERL_NIF_TERM to_erl_int_array(ErlNifEnv* env, int* arr, int count) {
	ERL_NIF_TERM list = enif_make_list(env, 0);

	int i;
	for (i=0; i<count; i++) {
		list = enif_make_list_cell(env, enif_make_int(env, arr[i]), list);
	}
	return list;
}

inline ERL_NIF_TERM to_erl_visible_obj_array(ErlNifEnv* env, VISIBLE_OBJECT* arr, int count) {
	ERL_NIF_TERM list = enif_make_list(env, 0);

	int i;
	for (i=0; i<count; i++) {
		ERL_NIF_TERM term = enif_make_tuple8(env,
				enif_make_atom(env, "visible_obj"),
				enif_make_int(env, arr[i].x),
				enif_make_int(env, arr[i].y),
				enif_make_int(env, arr[i].id),
				enif_make_int(env, arr[i].trans),
				enif_make_int(env, arr[i].stealth),
				enif_make_int(env, arr[i].type),
				enif_make_int(env, arr[i].time)
				);

		list = enif_make_list_cell(env, term, list);

		// старый способ когда хранили терм
		//list = enif_make_list_cell(env, arr[i].term, list);
	}
	return list;
}

// проверить скрыт ли объект от нас
// 1 - если видим
inline int check_stealth(int sight, int stealth, double distance) {
	//return 1;
	return (sight > stealth*distance*0.01f) ? 1 : 0;
}

// _Buf, _OldList, _X, _Y, _Sight, _myid
static ERL_NIF_TERM process(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef DEBUG_BUILD
	FILE * pFile;
	pFile = fopen("visible.log", "a");
#endif

	// vars
	int pos_x, pos_y, sight, my_id;
	int buf_count, list_count, out_list_count, out_added_count, out_deleted_count;
	int global_time;
	VISIBLE_OBJECT buf[MAX_OBJECT_BUFFER_SIZE];
	VISIBLE_OBJECT list[MAX_OBJECT_BUFFER_SIZE];
	VISIBLE_OBJECT out_list[MAX_OBJECT_BUFFER_SIZE];
	int out_added[MAX_OBJECT_BUFFER_SIZE];
	int out_deleted[MAX_OBJECT_BUFFER_SIZE];
	int i, j, f;
	int cur_time;

	ERL_NIF_TERM buf_term = argv[0];
	ERL_NIF_TERM list_term = argv[1];
	enif_get_int(env, argv[2], &pos_x);
	enif_get_int(env, argv[3], &pos_y);
	enif_get_int(env, argv[4], &sight);
	enif_get_int(env, argv[5], &my_id);
	enif_get_int(env, argv[6], &global_time);

	// читаем исходные данные
	enif_get_list_length(env, buf_term, &buf_count);
	enif_get_list_length(env, list_term, &list_count);
	DEBUGPRINTF("buf count=%i old_count=%i x=%i y=%i myid=%i\n", buf_count, list_count, pos_x, pos_y, my_id);

	// заполняем массивы
	fill_obj_list(env, buf, buf_term);
//	for (i=0; i<buf_count; i++) {
//		DEBUGPRINTF("x=%i y=%i id=%i type=%i\n", buf[i].x, buf[i].y, buf[i].id, buf[i].type);
//	}

	fill_obj_list(env, list, list_term);
//	for (i=0; i<list_count; i++) {
//		DEBUGPRINTF("x=%i y=%i id=%i\n", buf[i].x, buf[i].y, buf[i].id);
//	}

	out_list_count = 0;
	out_added_count = 0;
	out_deleted_count = 0;

	// размеры текущего объекта
	int cl, ct, cr, cb, tl, tt, tr, tb;
	// координаты текущего объекта
	int cx, cy, tx, ty;

	int intersected, found;

	// проходим по всем объектам
	for (i=0; i<buf_count; i++) {
		//if (1) {
		if (buf[i].id == my_id || is_always_visible(buf[i].type)) {
			// объект видим
			out_list[out_list_count] = buf[i];
			out_list[out_list_count].time = global_time;
			out_list_count++;
			found = 0;
			for (f=0; f<list_count; f++) {
				if (list[f].id == buf[i].id) {
					found = 1;
					break;
				}
			}
			if (!found) {
				out_added[out_added_count] = buf[i].id; out_added_count++;
			}
			continue;
		}

		cx = buf[i].x;
		cy = buf[i].y;

		double d = sqrt((pos_x-cx)*(pos_x-cx) + (pos_y-cy)*(pos_y-cy));
		// DEBUGPRINTF("sight=%d stealth=%f d=%f \n", sight, buf[i].stealth*d*0.01f, d);
		if (  check_stealth(sight, buf[i].stealth, d)  ) {
			intersected = 0;

//			get_bounds( buf[i].type, &cl, &ct, &cr, &cb ); cl += cx; cr += cx; ct += cy; cb += cy;
//			// проверяем пересекается линия взгляда до текущего объекта с двумя диагоналями любого из других объектов
//			//DEBUGPRINTF("check intersect %i %i %i %i\n", cl, ct, cr, cb);
//			int v1, v2, v3, v4;
//			v1=v2=v3=v4=0;
//			for (j=0; j<buf_count; j++) {
//				if (buf[j].id == my_id || j == i || buf[j].trans == 1 || is_transparent(buf[j].type)) continue;
//
//				tx = buf[j].x; ty = buf[j].y;
//				get_bounds( buf[j].type, &tl, &tt, &tr, &tb ); tl += tx; tr += tx; tt += ty; tb += ty;
//				//DEBUGPRINTF("check intersect ==== %i %i %i %i\n", tl, tt, tr, tb);
//
//				// если есть пересечение
//				if (intersection( pos_x, pos_y, cl, ct,     tl,tt, tr,tb )) { v1 = 1; break; }
//				if (intersection( pos_x, pos_y, cl, ct,     tr,tt, tl,tb )) { v1 = 1; break; }
//			}
//
//			for (j=0; j<buf_count; j++) {
//				if (buf[j].id == my_id || j == i || buf[j].trans == 1 || is_transparent(buf[j].type)) continue;
//				tx = buf[j].x; ty = buf[j].y;
//				get_bounds( buf[j].type, &tl, &tt, &tr, &tb ); tl += tx; tr += tx; tt += ty; tb += ty;
//				//DEBUGPRINTF("check intersect ==== %i %i %i %i\n", tl, tt, tr, tb);
//
//				if (intersection( pos_x, pos_y, cr, ct,     tl,tt, tr,tb )) { v2 = 1; break; }
//				if (intersection( pos_x, pos_y, cr, ct,     tr,tt, tl,tb )) { v2 = 1; break; }
//			}
//
//			for (j=0; j<buf_count; j++) {
//				if (buf[j].id == my_id || j == i || buf[j].trans == 1 || is_transparent(buf[j].type)) continue;
//				tx = buf[j].x; ty = buf[j].y;
//				get_bounds( buf[j].type, &tl, &tt, &tr, &tb ); tl += tx; tr += tx; tt += ty; tb += ty;
//				//DEBUGPRINTF("check intersect ==== %i %i %i %i\n", tl, tt, tr, tb);
//
//				if (intersection( pos_x, pos_y, cr, cb,     tl,tt, tr,tb )) { v3 = 1; break; }
//				if (intersection( pos_x, pos_y, cr, cb,     tr,tt, tl,tb )) { v3 = 1; break; }
//			}
//
//			for (j=0; j<buf_count; j++) {
//				if (buf[j].id == my_id || j == i || buf[j].trans == 1 || is_transparent(buf[j].type)) continue;
//				tx = buf[j].x; ty = buf[j].y;
//				get_bounds( buf[j].type, &tl, &tt, &tr, &tb ); tl += tx; tr += tx; tt += ty; tb += ty;
//				//DEBUGPRINTF("check intersect ==== %i %i %i %i\n", tl, tt, tr, tb);
//
//				if (intersection( pos_x, pos_y, cl, cb,     tl,tt, tr,tb )) { v4 = 1; break; }
//				if (intersection( pos_x, pos_y, cl, cb,     tr,tt, tl,tb )) { v4 = 1; break; }
//			}
//
//			if (v1 && v2 && v3 && v4) intersected = 1;

			if (intersected) {
				DEBUGPRINTF("intersect! obj=%i type=%i\n", buf[i].type, buf[j].type);
				// объект не видим
				found = 0;
				for (f=0; f<list_count; f++) {
					if (list[f].id == buf[i].id) {
						cur_time = list[f].time;
						found = 1;
						break;
					}
				}
				if (found) {
					if (global_time - cur_time > VISIBLE_TIME) {
						DEBUGPRINTF("obj visible time off gtime=%d otime=%d\n", global_time, buf[i].time);
						// если время после апдейта больше времени видимости - удаляем
						for (f=0; f<list_count; f++) {
							if (list[f].id == buf[i].id) {
								out_deleted[out_deleted_count] = buf[i].id; out_deleted_count++;
								break;
							}
						}
					} else {
						// иначе просто ничего не делаем. считаем объект видимым
						out_list[out_list_count] = buf[i];
						out_list[out_list_count].time = cur_time;
						out_list_count++;
						if (!found) {
							out_added[out_added_count] = buf[i].id; out_added_count++;
						}
					}
				} else {

				}
			} else {
				// объект видим, надо обновить время видимости
				out_list[out_list_count] = buf[i];
				out_list[out_list_count].time = global_time;
				out_list_count++;
				found = 0;
				for (f=0; f<list_count; f++) {
					if (list[f].id == buf[i].id) {
						found = 1;
						break;
					}
				}
				if (!found) {
					out_added[out_added_count] = buf[i].id; out_added_count++;
				}
			}
		} else {
			//DEBUGPRINTF("obj not visible %d\n", buf[i].id);
			// объект не видим
			found = 0;
			for (f=0; f<list_count; f++) {
				if (list[f].id == buf[i].id) {
					cur_time = list[f].time;
					found = 1;
					break;
				}
			}
			if (found) {
				DEBUGPRINTF("found obj not visible gtime=%d otime=%d\n", global_time, cur_time);
				if (global_time - cur_time > VISIBLE_TIME) {
					// если время после апдейта больше времени видимости - удаляем
					for (f=0; f<list_count; f++) {
						if (list[f].id == buf[i].id) {
							out_deleted[out_deleted_count] = buf[i].id; out_deleted_count++;
							break;
						}
					}
				} else {
					// иначе просто ничего не делаем. считаем объект видимым
					out_list[out_list_count] = buf[i];
					out_list[out_list_count].time = cur_time;
					out_list_count++;
					if (!found) {
						out_added[out_added_count] = buf[i].id; out_added_count++;
					}
				}
			} else {

			}
		}

	}

	ERL_NIF_TERM out_added_term = to_erl_int_array(env, out_added, out_added_count);
	ERL_NIF_TERM out_deleted_term = to_erl_int_array(env, out_deleted, out_deleted_count);
	ERL_NIF_TERM out_list_term = to_erl_visible_obj_array(env, out_list, out_list_count);

	DEBUGPRINTF("done: buf=%i in=%i out=%i add=%i del=%i\n", buf_count, list_count, out_list_count, out_added_count, out_deleted_count);
#ifdef DEBUG_BUILD
	fclose(pFile);
#endif

	return enif_make_tuple3(env, out_list_term, out_added_term, out_deleted_term);
}

static ErlNifFunc nif_funcs[] = {
    {"process", 7, process},
};

ERL_NIF_INIT(visible, nif_funcs, NULL, NULL, NULL, NULL)
