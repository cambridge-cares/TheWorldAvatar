from pyuploader.uploaders.uploader import Uploader
import requests
from typing import Tuple, Optional

FS_URL_ENV_VAR_VALUE = 'KG_FILE_SERVER_SPECS'
FS_AUTH_ENV_VAR_VALUE = 'KG_FILE_SERVER_SECRETS'

def get_file_server_uploader(
        uploader_name: str = 'file server',
        supported_file_ext: str='all',
        default_url: Optional[str] = None,
        default_auth_file: Optional[str] = None,
        default_no_auth: bool = False
        ) -> Uploader:

    fs_uploader = Uploader(
        upload_file_func= upload_file,
        uploader_name=uploader_name,
        supported_file_ext=supported_file_ext,
        default_url = default_url,
        default_auth_file = default_auth_file,
        default_no_auth = default_no_auth
    )

    fs_uploader.set_url_env_var_value(FS_URL_ENV_VAR_VALUE)
    fs_uploader.set_auth_env_var_value(FS_AUTH_ENV_VAR_VALUE)

    return fs_uploader

def upload_file(
    url: str,
    auth: Tuple[str,str],
    file_path: str,
    subdirs: Optional[str]=None,
    *args,
    **kwargs) -> str:

    headers = {}
    if subdirs is not None:
        if not subdirs.endswith('/'): subdirs = f"{subdirs}/"
        headers = {'subDir': subdirs}

    with open(file_path,'rb') as file_obj:
        response = requests.post(url= url,
                                auth= auth,
                                headers= headers,
                                files= {'file': file_obj})
        response.raise_for_status()
    return response.headers.pop('file', url)