#!/bin/bash

copy_done=false

while [ "$copy_done" = false ];
do
	timeout 10 sleep 10
	for container_name_dummy in $(docker ps --format "{{.Names}}" --filter name="geoserver"); do
	    for container_name in $(docker ps --format "{{.Names}}" --filter name="ontop"); do
            docker cp ./ontop.obda "$container_name":/
		    copy_done=true
		done
    done
done