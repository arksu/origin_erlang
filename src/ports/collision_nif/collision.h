/*
 * collision.h
 *
 *  Created on: 31.12.2011
 *      Author: arksu
 */

#ifndef COLLISION_H_
#define COLLISION_H_

#include "../defines.h"

typedef struct {
	int owner_id;
	int l,t,r,b;
	RECT rect;
} CLAIM;
#define CLAIMS_BUFFER_CAPACITY 100;

typedef struct {
	int sg;
	int grid;
	byte tile[GRID_SIZE*GRID_SIZE];
	byte flag[GRID_SIZE*GRID_SIZE];
} GRID;


typedef struct {
	int id;
	int type;
	int time_type;
	int x;
	int y;
	ERL_NIF_TERM obj;
} GAME_OBJECT;



#endif /* COLLISION_H_ */
