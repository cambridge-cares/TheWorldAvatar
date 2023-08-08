#!/bin/bash

copy_done=false

while [ "$copy_done" = false ];
do
	timeout 10 sleep 10
	for container_name in $(docker ps --format "{{.Names}}" --filter name="aermod-agent"); do
		docker cp . "$container_name":/vis_data
		copy_done=true
		echo "$container_name"
	done
done