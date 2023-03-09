from pyuploader.uploaders.uploader import Uploader
import pyuploader.uploaders.file_uploader as fs_uploader
import pyuploader.uploaders.triple_store_uploader as ts_uploader
import pyuploader.errorhandling.appexceptions as appexcept
from typing import Optional

def get_uploader(
    uploader_type: str,
    subdirs: Optional[str] = None,
    url: Optional[str] = None,
    auth_file: Optional[str] = None,
    no_auth: bool = False,
    url_env_var: Optional[str] = None,
    auth_file_env_var: Optional[str] = None) -> Uploader:

    if uploader_type == fs_uploader.FS_UPLOADER:
        uploader = fs_uploader.get_file_server_uploader(
            url=url,
            auth_file=auth_file,
            no_auth=no_auth,
            subdirs=subdirs,
            url_env_var=url_env_var,
            auth_file_env_var=auth_file_env_var
            )
    elif uploader_type == ts_uploader.TS_UPLOADER:
        uploader = ts_uploader.get_triple_store_uploader(
            url=url,
            auth_file=auth_file,
            no_auth=no_auth,
            url_env_var=url_env_var,
            auth_file_env_var=auth_file_env_var
            )
    else:
        raise appexcept.NotSupportedUploader(f"Error: Selected uploader type {uploader_type} is not supported.")
    return uploader