#!/bin/bash

## Load variables from .env file

source .env

TWA_DIR=$(dirname "$(dirname "$(pwd)")")
DEPLOY_DIR="$TWA_DIR/Deploy/stacks/dynamic"

## Remove existing stack of the same name

(cd "$DEPLOY_DIR/stack-manager" && ./stack.sh rm "$STACK_NAME" -v)

echo "Spinning up the stack..."

(cd "$DEPLOY_DIR/stack-manager" && ./stack.sh start "$STACK_NAME" "$PORT_NUMBER")

spin_up=false

beginning=$SECONDS

while [ "$spin_up" = false ];
do
	sleep 10
	if [ -n "$(docker ps -f "name=$STACK_NAME_stack-manager" -f "status=running" -q )" ]; then
		#echo "the manager is running!"
		duration=$(( SECONDS - beginning))
		echo "$duration seconds have passed since stack manager started"
	else
		echo "manager has exited with the following status:"
		docker ps -a -f "name=$STACK_NAME_stack-manager" --format="{{.Status}}"
		spin_up=true
	fi
done

echo "Uploading data..."

(cd "$DEPLOY_DIR/stack-data-uploader" && ./stack.sh start $STACK_NAME)

uploaded=false

beginning=$SECONDS

sleep 10

while [ "$uploaded" = false ];
do
	sleep 10
	if [ -n "$(docker ps -f "name=$STACK_NAME_stack-data-uploader" -f "status=running" -q )" ]; then
		#echo "the uploader is running!"
		duration=$(( SECONDS - beginning))
		echo "$duration seconds have passed since data upload commenced"
	else
		echo "the uploader stops with the following status:"
		docker ps -a -f "name=$STACK_NAME_stack-data-uploader" --format="{{.Status}}"
		uploaded=true
	fi
done

