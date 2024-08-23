#!/bin/bash

copy_done=false

while [ "$copy_done" = false ];
do
	timeout 10 sleep 10
	for container_name in $(docker ps --format "{{.Names}}" --filter name="ship-input-agent"); do
		docker cp ./data "$container_name":/
		copy_done=true
		echo "$container_name"
	done
done