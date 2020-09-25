#!/bin/bash
CMD="gORUN.exe "$project_name" "$activity" "$output" "$password
echo $CMD
echo -e "\nExecuting command:\n$CMD\n==================\n"
eval $CMD
exit