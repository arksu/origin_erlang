#!/bin/sh
cd collision_nif 
make
cd .. 
echo '-----------------------------------'

cd grid_processor_nif 
make 
cd .. 
echo '-----------------------------------'


cd visible_nif 
make 
cd ..
echo '-----------------------------------'

cd cutils_nif 
make 
cd ..
echo '-----------------------------------'
