#!/bin/sh

# Get the postgres password from a Docker secret, or set a default if none was supplied.
# Write the result to the relevant properties file
if [ -e "$POSTGRES_PASSWORD_FILE" ]; then
  postgres_password=$(cat $POSTGRES_PASSWORD_FILE)
else 
  postgres_password="postpass"
fi
sed -i "s/POSTGRES_PASSWORD/$postgres_password/" /opt/ontop/obda/cropmap.properties

# Run ontop, waiting for the postgis server to start first
/opt/wait-for-it/wait-for-it.sh postgis:${POSTGRES_PORT} \
                                --timeout=0 \
                                --strict \
                                -- \
                                /opt/ontop/entrypoint.sh