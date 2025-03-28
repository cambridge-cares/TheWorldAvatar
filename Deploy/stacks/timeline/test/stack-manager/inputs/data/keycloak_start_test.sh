#!/bin/bash
export KEYCLOAK_ADMIN=$(cat /run/secrets/keycloak_admin_username_test)
export KEYCLOAK_ADMIN_PASSWORD=$(cat /run/secrets/keycloak_admin_password_test)
/opt/keycloak/bin/kc.sh start-dev --import-realm
exit
