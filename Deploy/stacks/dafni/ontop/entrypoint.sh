#!/bin/sh

# Get the postgres password from a Docker secret, or set a default if none was supplied.
# Write the result to the relevant properties file
if [ -e "$POSTGRES_PASSWORD_FILE" ]; then
  postgres_password=$(cat $POSTGRES_PASSWORD_FILE)
else 
  postgres_password="postpass"
fi
sed -e "s/POSTGRES_HOST/$POSTGRES_HOST/" \
    -e "s/POSTGRES_USER/$POSTGRES_USER/" \
    -e "s/POSTGRES_PASSWORD/$postgres_password/" \
    ${ONTOP_PROPERTIES_FILE}.template \
    > $ONTOP_PROPERTIES_FILE

# Run ontop, waiting for the postgis server to start first
/opt/wait-for-it/wait-for-it.sh ${POSTGRES_HOST}:5432 \
                                --timeout=0 \
                                --strict \
                                -- \
                                /opt/ontop/entrypoint.sh