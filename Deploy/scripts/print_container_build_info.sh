#!/bin/sh
docker ps --format 'table {{.Names}}\t{{.Networks}}\t{{.Label "builder"}}\t{{.Label "hash"}}'
