/*
 * nif_utils.h
 *
 *  Created on: 09.01.2012
 *      Author: arksu
 */

#ifndef NIF_UTILS_H_
#define NIF_UTILS_H_

#include <stdio.h>
#include "erl_nif.h"
#include "defines.h"

inline void get_obj_type(ErlNifEnv* env, ERL_NIF_TERM obj, char* buf);
inline void get_obj_state(ErlNifEnv* env, ERL_NIF_TERM obj, ERL_NIF_TERM* out);
inline void get_obj_params(ErlNifEnv* env, ERL_NIF_TERM obj, ERL_NIF_TERM* out);
inline void get_obj_id(ErlNifEnv* env, ERL_NIF_TERM obj, int* out);
inline void get_obj_time(ErlNifEnv* env, ERL_NIF_TERM obj, int* out);
inline void get_obj_hp(ErlNifEnv* env, ERL_NIF_TERM obj, int* out);
inline Coord get_obj_coord(ErlNifEnv* env, ERL_NIF_TERM obj);
inline ERL_NIF_TERM get_tuple_elem(ErlNifEnv* env, ERL_NIF_TERM term, int index);
inline double distance(double x1, double y1, double x2, double y2);
inline RECT rect (int l, int t, int r, int b);
inline RECT bounds(int x, int y, int w, int h);
inline RECT rect_from_bound(int x, int y, int l, int t, int r, int b);
inline int is_rect_intersect(RECT r1, RECT r2);



#endif /* NIF_UTILS_H_ */
