#! /bin/bash

python create_HG_KG_instances.py 

python match_KG_main_sub_instances.py

python match_HG_main_sub_instances.py