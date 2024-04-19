#!/bin/bash

# Start the redis server
redis-server --daemonize yes

# Start tomcat in the foreground
catalina.sh run


