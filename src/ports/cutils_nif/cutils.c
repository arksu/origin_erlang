/*
 * cutils.c
 *
 *  Created on: 17.01.2012
 *      Author: arksu
 */

#include <stdio.h>
#include <string.h>
#include "erl_nif.h"

#include "../defines.h"
#include "../nif_utils.h"
#include "../tiles.h"
#include "../map.h"
#include <math.h>

#ifdef DEBUG_BUILD
static	FILE * pFile;
#endif

//-------------------------------------------------------------------------------------------------
// в каких гридах находится объект, вернет список {Sg, Grid}
static ERL_NIF_TERM get_object_grids(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	char buf[ATOM_BUFFER_SIZE];
	int l,t,r,b;
	int cx, cy, csg, cgrid;
	int x, y, tox, toy, lv;
	int x1, x2, y1, y2;
	int sg[4];
	int grid[4];
	ERL_NIF_TERM term[4];
	int i;
	int count = 0;
	int found;

	// тип, X, Y, Lv
	enif_get_atom(env, 	argv[0], buf, ATOM_BUFFER_SIZE, ERL_NIF_LATIN1);
	enif_get_int(env, 	argv[1], &x);
	enif_get_int(env, 	argv[2], &y);
	enif_get_int(env, 	argv[3], &tox);
	enif_get_int(env, 	argv[4], &toy);
	enif_get_int(env, 	argv[5], &lv);

	// получаем размеры объекта
	get_bounds(get_typeid(buf), &l, &t, &r, &b);

	x1 = x; x2 = tox;
	if (x > tox) {x1 = tox; x2 = x;}
	y1 = y; y2 = toy;
	if (y > toy) {y1 = toy; y2 = y;}

	// ищем 4 точки объекта, смотрим в каком гриде, если этого грида нет в результате - добавим
	cx=x1+l; cy=y1+t;
	get_sg_grid(cx, cy, lv, &sg[0], &grid[0]);
	term[0] = enif_make_tuple2(env, enif_make_int(env, sg[0]), enif_make_int(env, grid[0]));
	count++;

	cx=x2+r; cy=y1+t;
	get_sg_grid(cx, cy, lv, &csg, &cgrid);
	found = 0;
	for (i = 0; i < count; ++i) {
		if (sg[i] == csg && grid[i] == cgrid) {
			found = 1;
			break;
		}
	}
	if (!found) {
		sg[count] = csg;
		grid[count] = cgrid;
		term[count] = enif_make_tuple2(env, enif_make_int(env, csg), enif_make_int(env, cgrid));
		count++;
	}

	cx=x2+r; cy=y2+b;
	get_sg_grid(cx, cy, lv, &csg, &cgrid);
	found = 0;
	for (i = 0; i < count; ++i) {
		if (sg[i] == csg && grid[i] == cgrid) {
			found = 1;
			break;
		}
	}
	if (!found) {
		sg[count] = csg;
		grid[count] = cgrid;
		term[count] = enif_make_tuple2(env, enif_make_int(env, csg), enif_make_int(env, cgrid));
		count++;
	}

	cx=x1+l; cy=y2+b;
	get_sg_grid(cx, cy, lv, &csg, &cgrid);
	found = 0;
	for (i = 0; i < count; ++i) {
		if (sg[i] == csg && grid[i] == cgrid) {
			found = 1;
			break;
		}
	}
	if (!found) {
		term[count] = enif_make_tuple2(env, enif_make_int(env, csg), enif_make_int(env, cgrid));
		count++;
	}

	// конвертим в список и возвращаем
	return enif_make_list_from_array(env, term, count);
}

//-------------------------------------------------------------------------------------------------
// является ли объект вещью
static ERL_NIF_TERM is_object_item(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	char buf[ATOM_BUFFER_SIZE];
	if (enif_get_atom(env, argv[0], buf, ATOM_BUFFER_SIZE, ERL_NIF_LATIN1)) {
		if (is_item(get_typeid(buf))) {
			return enif_make_atom(env, "true");
		} else {
			return enif_make_atom(env, "false");
		}
	} else {
		return enif_make_atom(env, "false");
	}
}

//-------------------------------------------------------------------------------------------------
static ERL_NIF_TERM get_object_typeid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	char buf[ATOM_BUFFER_SIZE];
	if (enif_get_atom(env, argv[0], buf, ATOM_BUFFER_SIZE, ERL_NIF_LATIN1)) {
		return enif_make_int(env, get_typeid(buf));
	} else {
		return enif_make_int(env, 0);
	}
}

//-------------------------------------------------------------------------------------------------
static ERL_NIF_TERM get_object_type(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int typeid;
	if (enif_get_int(env, argv[0], &typeid)) {
		char buf[ATOM_BUFFER_SIZE];
		get_type(typeid, buf);
		return enif_make_atom(env, buf);
	} else {
		return enif_make_atom(env, "error");
	}
}

//-------------------------------------------------------------------------------------------------
static ERL_NIF_TERM get_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef DEBUG_BUILD
	pFile = fopen("cutils.log", "a");
#endif

//Data, Index
	ErlNifBinary bin;
	int index;
	char tile_type_atom[ATOM_BUFFER_SIZE];


	if (enif_inspect_binary(env, argv[0], &bin)) {
		DEBUGPRINT("data bin ok\n");

		if (enif_get_int(env, argv[1], &index)) {
			DEBUGPRINTF("int ok %d\n ",index);

			if (index >= 0 && index < (GRID_SIZE*GRID_SIZE)) {
				get_tile_type(bin.data[index], tile_type_atom);
				DEBUGPRINTF("tile=%s\n", tile_type_atom);

				#ifdef DEBUG_BUILD
					fclose(pFile);
				#endif

				return enif_make_tuple3(env,
								enif_make_atom(env, "tile"), // record name
								enif_make_atom(env, tile_type_atom), // tile type
								enif_make_int(env, bin.data[(GRID_SIZE*GRID_SIZE) + index]) // flag
								);
			} else {
				return enif_make_tuple3(env,
								enif_make_atom(env, "tile"), // record name
								enif_make_atom(env, "none"), // tile type
								enif_make_int(env, 0) // flag
								);
			}
		}

	} else {
		return enif_make_tuple3(env,
										enif_make_atom(env, "tile"), // record name
										enif_make_atom(env, "none"), // tile type
										enif_make_int(env, 0) // flag
										);
	}


#ifdef DEBUG_BUILD
	fclose(pFile);
#endif

	return 0;

}

//----------------------------------------------------------------------------------------
static ERL_NIF_TERM set_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef DEBUG_BUILD
	pFile = fopen("cutils.log", "a");
#endif

	// Data, Index, Tile
	ErlNifBinary bin;
	ErlNifBinary bin2;
	int index;
	char tile_type_atom[ATOM_BUFFER_SIZE];
	byte bin_tiles[2*GRID_SIZE*GRID_SIZE];


	if (enif_inspect_binary(env, argv[0], &bin)) {
		DEBUGPRINT("data bin ok\n");
		memcpy(bin_tiles, bin.data, GRID_SIZE*GRID_SIZE);

		if (enif_get_int(env, argv[1], &index)) {
			DEBUGPRINTF("int ok %d\n ",index);

			if (index >= 0 && index < (GRID_SIZE*GRID_SIZE)) {
				get_tile_type(bin.data[index], tile_type_atom);
				DEBUGPRINTF("tile=%s\n", tile_type_atom);

				int arity;
				const ERL_NIF_TERM * tuple;
				// #tile record
				if (enif_get_tuple(env, argv[2], &arity, &tuple))
					if (arity == 3) {
						// tile type
						enif_get_atom(env, tuple[1], tile_type_atom, ATOM_BUFFER_SIZE, ERL_NIF_LATIN1);
						bin_tiles[index] = get_tile_typeid(tile_type_atom); // tile type id
						DEBUGPRINTF("tile type=%d\n", bin_tiles[index]);
						int flag;
						// tile flag
						enif_get_int(env, tuple[2], &flag);
						DEBUGPRINTF("tile flag=%d\n", flag);
						bin_tiles[(GRID_SIZE*GRID_SIZE) + index] = flag; // flag

						enif_alloc_binary(2*GRID_SIZE*GRID_SIZE, &bin2);
						//bin2.size = 2*GRID_SIZE*GRID_SIZE;
						memcpy(bin2.data, bin_tiles, 2*GRID_SIZE*GRID_SIZE);
#ifdef DEBUG_BUILD
	fclose(pFile);
#endif
						return enif_make_binary(env, &bin2);

					}
			}
		}
	}
	return enif_make_atom(env, "ok");
}

//-------------------------------------------------------------------------------------------------
static ERL_NIF_TERM get_object_bounds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	char buf[ATOM_BUFFER_SIZE];
	if (enif_get_atom(env, argv[0], buf, ATOM_BUFFER_SIZE, ERL_NIF_LATIN1)) {
		int l, t, r, b;
		get_bounds(get_typeid(buf), &l, &t, &r, &b);

		return enif_make_tuple4(env,
										enif_make_int(env, l),
										enif_make_int(env, t),
										enif_make_int(env, r),
										enif_make_int(env, b)
										);
	} else {
		return enif_make_atom(env, "error");
	}
}

//-------------------------------------------------------------------------------------------------
static ERL_NIF_TERM get_object_collision(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	char buf[ATOM_BUFFER_SIZE];
	int type1, type2;
	if (enif_get_atom(env, argv[0], buf, ATOM_BUFFER_SIZE, ERL_NIF_LATIN1)) {
		type1 = get_typeid(buf);

		if (enif_get_atom(env, argv[0], buf, ATOM_BUFFER_SIZE, ERL_NIF_LATIN1)) {
			type2 = get_typeid(buf);

			if (get_collision(type1, type2)) {
				return enif_make_atom(env, "true");
			} else {
				return enif_make_atom(env, "false");
			}
		} else {
			return enif_make_atom(env, "error");
		}
	} else {
		return enif_make_atom(env, "error");
	}
}

//-----------------------------------------------------------------------------------------------
static ErlNifFunc nif_funcs[] = {
		{"get_tile", 			2, get_tile},
		{"set_tile", 			3, set_tile},
		{"get_object_typeid", 	1, get_object_typeid},
		{"get_object_type", 	1, get_object_type},
		{"is_object_item", 		1, is_object_item},
		{"get_object_grids", 	6, get_object_grids},
		{"get_object_bounds", 	1, get_object_bounds},
		{"get_object_collision",2, get_object_collision},
};

ERL_NIF_INIT(cutils, nif_funcs, NULL, NULL, NULL, NULL)
