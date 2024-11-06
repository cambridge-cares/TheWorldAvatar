#!/bin/bash

if [[ ${DEBUG} == ON ]]; then
  catalina.sh jpda run
else
  catalina.sh run
fi
