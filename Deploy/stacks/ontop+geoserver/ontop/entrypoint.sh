#!/bin/bash

# Get the postgres password from a Docker secret, or set a default if none was supplied.
# Write the result to the relevant properties file
if [ -e "$POSTGRES_PASSWORD_FILE" ]; then
  postgres_password="$(cat $POSTGRES_PASSWORD_FILE)"
else 
 if [ -n "$POSTGRES_PASSWORD" ]; then
    postgres_password="$POSTGRES_PASSWORD"
  else
    postgres_password="postpass"
  fi
fi

# If ONTOP_PROPERTIES_FILE_TEMPLATE wasn't supplied, assume we can just append '.template' to ONTOP_PROPERTIES_FILE
if [ -z "$ONTOP_PROPERTIES_FILE_TEMPLATE" ]; then 
  ONTOP_PROPERTIES_FILE_TEMPLATE="${ONTOP_PROPERTIES_FILE}.template"
fi

# Ensure the target directory for the properties file exists
mkdir -p $(dirname "$ONTOP_PROPERTIES_FILE")

sed -e "s/POSTGRES_HOST/$POSTGRES_HOST/" \
    -e "s/POSTGRES_PORT/$POSTGRES_PORT/" \
    -e "s/POSTGRES_USER/$POSTGRES_USER/" \
    -e "s/POSTGRES_DB/$POSTGRES_DB/"\
    -e "s/POSTGRES_PASSWORD/$postgres_password/" \
    "$ONTOP_PROPERTIES_FILE_TEMPLATE" \
    > $ONTOP_PROPERTIES_FILE

# Run ontop, waiting for the postgis server to start first
/opt/ontop/wait-for-it.sh ${POSTGRES_HOST}:${POSTGRES_PORT} \
                                --timeout=0 \
                                --strict \
                                -- \
                                /opt/ontop/entrypoint.sh