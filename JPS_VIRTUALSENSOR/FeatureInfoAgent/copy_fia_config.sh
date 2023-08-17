#!/bin/bash

copy_done=false

while [ "$copy_done" = false ];
do
	for container_name in $(docker ps --format "{{.Names}}" --filter name="feature-info-agent"); do
		docker cp ./queries "$container_name":/app/
		copy_done=true
		echo "$container_name"
	done
done