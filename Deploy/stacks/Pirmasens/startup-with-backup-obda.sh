#!/bin/bash
(cd stack-manager && ./stack.sh start dhstack)
(cd ontop-backup && ./copy_ontop_file.sh)
