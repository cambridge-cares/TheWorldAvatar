#!/bin/sh

# Combine all daily logs into a single one
cat /var/log/reverse_proxy/*.log > /var/log/goaccess/combined.log

# Run GoAccess to produce static HTML file
goaccess /var/log/goaccess/combined.log -o /var/log/goaccess/index.html --log-format=COMBINED