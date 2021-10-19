#!/bin/bash

# This script initialises the a scheduler that writes output files once a day
echo "Running start-up.sh script..."

echo "Starting FloodAgent code in new process..."
java -cp /app/FloodAgent-1.0.0-SNAPSHOT.jar uk.ac.cam.cares.jps.agent.flood.LaunchScheduledWriterOnly &
echo "FloodAgent has started."

echo "Starting Apache web server..."
/usr/sbin/httpd -D FOREGROUND