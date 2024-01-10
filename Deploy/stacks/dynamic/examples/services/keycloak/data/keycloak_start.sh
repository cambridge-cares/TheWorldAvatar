#!/bin/bash
export KEYCLOAK_ADMIN=$(cat /run/secrets/keycloak_admin_username)
export KEYCLOAK_ADMIN_PASSWORD=$(cat /run/secrets/keycloak_admin_password)
/opt/keycloak/bin/kc.sh start --hostname-debug=true
exit