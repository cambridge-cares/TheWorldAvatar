#!/bin/bash

# Start the redis server
redis-server --daemonize yes

# Start tomcat in the background
startup.sh run

# Start the node app
pm2-runtime comunica_node.js