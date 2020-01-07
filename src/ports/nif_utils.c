/*
 * nif_utils.c
 *
 *  Created on: 09.01.2012
 *      Author: arksu
 */

#include <stdio.h>
#include "erl_nif.h"
#include "defines.h"
#include <math.h>

inline void get_obj_type(ErlNifEnv* env, ERL_NIF_TERM obj, char* buf) {
	const ERL_NIF_TERM* tuple;
	int arity;
	enif_get_tuple(env, obj, &arity, &tuple);
	enif_get_atom(env, tuple[OBJ_TYPE], buf, ATOM_BUFFER_SIZE, ERL_NIF_LATIN1);
}

inline void get_obj_state(ErlNifEnv* env, ERL_NIF_TERM obj, ERL_NIF_TERM* out) {
	const ERL_NIF_TERM* tuple;
	int arity;
	enif_get_tuple(env, obj, &arity, &tuple);
	*out = tuple[OBJ_STATE];
}

inline void get_obj_params(ErlNifEnv* env, ERL_NIF_TERM obj, ERL_NIF_TERM* out) {
	const ERL_NIF_TERM* tuple;
	int arity;
	enif_get_tuple(env, obj, &arity, &tuple);
	*out = tuple[OBJ_PARAMS];
}

inline void get_obj_id(ErlNifEnv* env, ERL_NIF_TERM obj, int* out) {
	const ERL_NIF_TERM* tuple;
	int arity;
	enif_get_tuple(env, obj, &arity, &tuple);
	enif_get_int(env, tuple[OBJ_ID], out);
}

inline void get_obj_time(ErlNifEnv* env, ERL_NIF_TERM obj, int* out) {
	const ERL_NIF_TERM* tuple;
	int arity;
	enif_get_tuple(env, obj, &arity, &tuple);
	enif_get_int(env, tuple[OBJ_TIME], out);
}

inline void get_obj_hp(ErlNifEnv* env, ERL_NIF_TERM obj, int* out) {
	const ERL_NIF_TERM* tuple;
	int arity;
	enif_get_tuple(env, obj, &arity, &tuple);
	enif_get_int(env, tuple[OBJ_HP], out);
}


inline Coord get_obj_coord(ErlNifEnv* env, ERL_NIF_TERM obj) {
	const ERL_NIF_TERM* tuple;
	const ERL_NIF_TERM* tc;
	int arity;
	enif_get_tuple(env, obj, &arity, &tuple);
	enif_get_tuple(env, tuple[OBJ_COORD], &arity, &tc);
	Coord c;
	enif_get_int(env, tc[0], &c.x);
	enif_get_int(env, tc[1], &c.y);
	enif_get_int(env, tc[2], &c.lv);
	return c;
}

inline ERL_NIF_TERM get_tuple_elem(ErlNifEnv* env, ERL_NIF_TERM term, int index) {
	const ERL_NIF_TERM* tuple;
	int arity;
	enif_get_tuple(env, term, &arity, &tuple);
	return tuple[index];
}

inline double distance(double x1, double y1, double x2, double y2) {
	return sqrt( (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) );
}

inline RECT rect (int l, int t, int r, int b) {
	RECT rect;
	rect.left = l;
	rect.top = t;
	rect.right = r;
	rect.bottom = b;
	return rect;
}

inline RECT bounds(int x, int y, int w, int h) {
	RECT r;
	r.left = x;
	r.top = y;
	r.right = x + w;
	r.bottom = y + h;
	return r;
}

inline RECT rect_from_bound(int x, int y, int l, int t, int r, int b) {
	RECT rect;
	rect.left = x + l;
	rect.top = y + t;
	rect.right = x + r;
	rect.bottom = y + b;
	return rect;
}

inline int is_rect_intersect(RECT r1, RECT r2) {
	if (
			(((r1.left > r2.left) && (r1.left <= r2.right)) || ((r1.right > r2.left) && (r1.right <= r2.right)) ||
			((r2.left > r1.left)  && (r2.left <= r1.right)) || ((r2.right > r1.left) && (r2.right <= r1.right))) &&
			(((r1.top > r2.top) && (r1.top <= r2.bottom)) || ((r1.bottom > r2.top) && (r1.bottom <= r2.bottom)) ||
			((r2.top > r1.top)  && (r2.top <= r1.bottom)) || ((r2.bottom > r1.top) && (r2.bottom <= r1.bottom)))
	)
		return 1;
	else
		return 0;
}

