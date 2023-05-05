import os
from flask_appbuilder.security.manager import AUTH_OAUTH


def get_string_from_file(file_path: str) -> str:
    '''Get string from a specified file'''
    with open(file_path, 'r') as file:
        return file.read()


WTF_CSRF_ENABLED = False

PUBLIC_ROLE_LIKE = os.environ.get('PUBLIC_ROLE_LIKE')

APP_NAME = os.environ.get('APP_NAME')

SECRET_KEY = get_string_from_file(os.environ.get('SUPERSET_SECRET_KEY_FILE'))

# ----------------------------------------------------
# AUTHENTICATION CONFIG
# ----------------------------------------------------
# The authentication type
# AUTH_OID : Is for OpenID
# AUTH_DB : Is for database (username/password()
# AUTH_LDAP : Is for LDAP
# AUTH_REMOTE_USER : Is for using REMOTE_USER from web server
AUTH_TYPE = AUTH_OAUTH

OAUTH_PROVIDERS = [
     {
        "name": "keycloak",
        "icon": "fa-key",
        "token_key": "access_token",
        "remote_app": {
            "client_id": os.environ.get("KEYCLOAK_CLIENT_ID"),
            "client_secret": os.environ.get("KEYCLOAK_CLIENT_SECRET"),
            "api_base_url": f"https://{os.environ.get('KEYCLOAK_DOMAIN')}/"
            f"realms/{os.environ.get('KEYCLOAK_REALM')}/protocol/openid-connect",
            "client_kwargs": {"scope": "email profile"},
            "access_token_url": f"https://{os.environ.get('KEYCLOAK_DOMAIN')}/"
            f"realms/{os.environ.get('KEYCLOAK_REALM')}/protocol/openid-connect/token",
            "authorize_url": f"https://{os.environ.get('KEYCLOAK_DOMAIN')}/"
            f"realms/{os.environ.get('KEYCLOAK_REALM')}/protocol/openid-connect/auth",
            "request_token_url": None,
        },
    },
]

# Uncomment to setup Full admin role name
# AUTH_ROLE_ADMIN = 'Admin'

# Uncomment to setup Public role name, no authentication needed
# AUTH_ROLE_PUBLIC = 'Public'

# Will allow user self registration
AUTH_USER_REGISTRATION = True

# The default user self registration role for all users
AUTH_USER_REGISTRATION_ROLE = "Admin"

# Self registration role based on user info
# AUTH_USER_REGISTRATION_ROLE_JMESPATH = "contains(['alice@example.com', 'celine@example.com'], email) && 'Admin' || 'Public'"

# Replace users database roles each login with those received from OAUTH/LDAP
AUTH_ROLES_SYNC_AT_LOGIN = True

# A mapping from LDAP/OAUTH group names to FAB roles
AUTH_ROLES_MAPPING = {
    # For OAUTH
    # "USER_GROUP_NAME": ["User"],
    # "ADMIN_GROUP_NAME": ["Admin"],
    # For LDAP
    # "cn=User,ou=groups,dc=example,dc=com": ["User"],
    # "cn=Admin,ou=groups,dc=example,dc=com": ["Admin"],
}

# When using LDAP Auth, setup the ldap server
# AUTH_LDAP_SERVER = "ldap://ldapserver.new"
# AUTH_LDAP_USE_TLS = False

# Uncomment to setup OpenID providers example for OpenID authentication
# OPENID_PROVIDERS = [
#    { 'name': 'Google', 'url': 'https://www.google.com/accounts/o8/id' },
#    { 'name': 'Yahoo', 'url': 'https://me.yahoo.com' },
#    { 'name': 'AOL', 'url': 'http://openid.aol.com/<username>' },
#    { 'name': 'Flickr', 'url': 'http://www.flickr.com/<username>' },
#    { 'name': 'MyOpenID', 'url': 'https://www.myopenid.com' }]
