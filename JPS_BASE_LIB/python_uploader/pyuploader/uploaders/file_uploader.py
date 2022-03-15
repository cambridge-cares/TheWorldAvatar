from pyuploader.uploaders.uploader import Uploader
import requests
from typing import Tuple, Optional

FS_URL_ENV_VAR_VALUE = 'KG_FILE_SERVER_SPECS'
FS_AUTH_ENV_VAR_VALUE = 'KG_FILE_SERVER_SECRETS'

FS_UPLOADER = 'file server'

class File_Server_Uploader(Uploader):
    """File server uploader class."""
    def __init__(
        self,
        uploader_name: str = FS_UPLOADER,
        subdirs: Optional[str] = None,
        supported_file_ext: str = 'all',
        default_url: Optional[str] = None,
        default_auth_file: Optional[str] = None,
        default_no_auth: bool = False):

        super().__init__(
            uploader_name=uploader_name,
            supported_file_ext=supported_file_ext,
            default_url=default_url,
            default_auth_file=default_auth_file,
            default_no_auth=default_no_auth)

        self.subdirs = subdirs


    def _upload_file(
        self,
        url: str,
        auth: Tuple[str,str],
        file_path: str,
        **kwargs) -> str:

        headers = {}
        subdirs: Optional[str] = kwargs.get('subdirs')
        if subdirs is None:
            subdirs = self.subdirs
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

def get_file_server_uploader(
        uploader_name: str = 'file server',
        subdirs: Optional[str] = None,
        supported_file_ext: str='all',
        default_url: Optional[str] = None,
        default_auth_file: Optional[str] = None,
        default_no_auth: bool = False
        ) -> Uploader:

    fs_uploader = File_Server_Uploader(
        uploader_name=uploader_name,
        subdirs=subdirs,
        supported_file_ext=supported_file_ext,
        default_url = default_url,
        default_auth_file = default_auth_file,
        default_no_auth = default_no_auth
    )

    fs_uploader.set_url_env_var_value(FS_URL_ENV_VAR_VALUE)
    fs_uploader.set_auth_env_var_value(FS_AUTH_ENV_VAR_VALUE)

    return fs_uploader