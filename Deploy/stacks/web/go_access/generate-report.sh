#!/bin/sh

# Combine all daily logs into a single one
cat /var/log/reverse_proxy/*.log > /var/log/combined.log

# Run GoAccess to produce static HTML file
goaccess /var/log/combined.log -o /var/www/html/index.html --log-format=COMBINED