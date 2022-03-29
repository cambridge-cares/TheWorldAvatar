#!/bin/bash
# D. Nurkowski (danieln@cmclinnovations.com)
echo `date` "deleting: "
find /app/logs/* -mmin +1 -type d | xargs rm -rfv