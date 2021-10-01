import pyuploader.common.querykg as querykg
import requests
import os

def get_user_credentials():
    user_login = os.environ['CMCL_FILE_SERVER_LOGIN']
    user_passwd = os.environ['CMCL_FILE_SERVER_PASSWORD']
    return (user_login, user_passwd)

def get_server_upload_url():
    return os.environ['CMCL_FILE_SERVER_UPLOAD_URL']


