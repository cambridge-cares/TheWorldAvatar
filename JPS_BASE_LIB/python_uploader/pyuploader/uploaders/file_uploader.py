from pyuploader.uploaders.uploader import Uploader, Upload_Client
import functools as functools
import requests
from typing import Tuple, Optional, Callable, List

FS_URL_ENV_VAR_VALUE = 'KG_FILE_SERVER_SPECS'
FS_AUTH_ENV_VAR_VALUE = 'KG_FILE_SERVER_SECRETS'

FS_UPLOADER = 'file server'

class File_Server_Uploader(Uploader):
    """File server uploader class."""
    def __init__(
        self,
        uploader_name: str = FS_UPLOADER,
        subdirs: Optional[str] = None,
        supported_file_ext: List[str] = ['all'],
        url: Optional[str] = None,
        auth_file: Optional[str] = None,
        no_auth: bool = False,
        url_env_var: Optional[str] = None,
        auth_file_env_var: Optional[str] = None):

        super().__init__(
            uploader_name=uploader_name,
            supported_file_ext=supported_file_ext,
            url=url,
            auth_file=auth_file,
            no_auth=no_auth,
            subdirs=subdirs,
            url_env_var=url_env_var if url_env_var is not None else FS_URL_ENV_VAR_VALUE,
            auth_file_env_var=auth_file_env_var if auth_file_env_var is not None else FS_AUTH_ENV_VAR_VALUE)


    def _get_upload_client(self, url: str, auth: Tuple[str,str]) -> Upload_Client:
        return functools.partial(self.__upload_wrapper, url, auth)

    @staticmethod
    def __upload_wrapper(url: str, auth: Tuple[str,str], file_path: str, *kwargs)->str:
        with open(file_path,'rb') as file_obj:
            response = requests.post(url= url,
                                    auth= auth,
                                    files= {'file': file_obj})
            response.raise_for_status()
        return response.headers.pop('file', url)

def get_file_server_uploader(
        uploader_name: str = 'file server',
        subdirs: Optional[str] = None,
        supported_file_ext: List[str] = ['all'],
        url: Optional[str] = None,
        auth_file: Optional[str] = None,
        no_auth: bool = False,
        url_env_var: Optional[str] = None,
        auth_file_env_var: Optional[str] = None
        ) -> Uploader:

    fs_uploader = File_Server_Uploader(
        uploader_name=uploader_name,
        subdirs=subdirs,
        supported_file_ext=supported_file_ext,
        url = url,
        auth_file = auth_file,
        no_auth = no_auth,
        url_env_var = url_env_var,
        auth_file_env_var = auth_file_env_var
    )

    return fs_uploader