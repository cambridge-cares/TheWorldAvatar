from typing import Dict, Tuple, Union
import requests
from pyuploader.base.uploader import Uploader, \
                                     BASE_URL_ENV_VAR_KEY, \
                                     BASE_AUTH_ENV_VAR_KEY

FS_URL_ENV_VAR_VALUE = 'KG_FILE_SERVER_SPECS'
FS_AUTH_ENV_VAR_VALUE = 'KG_FILE_SERVER_SECRETS'

class FileServerUploader(Uploader):

    def __init__(self):
        super().__init__()
        self.supported_file_ext = 'all'
        self.uploader_descr = 'file server uploader'
        self.env_vars[BASE_URL_ENV_VAR_KEY] = FS_URL_ENV_VAR_VALUE
        self.env_vars[BASE_AUTH_ENV_VAR_KEY] = FS_AUTH_ENV_VAR_VALUE

    def upload_file(
        self,
        url: str,
        auth: Tuple[str,str],
        file_path: str,
        subdirs: Union[str, None]=None) -> str:

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
        return response.headers['file']