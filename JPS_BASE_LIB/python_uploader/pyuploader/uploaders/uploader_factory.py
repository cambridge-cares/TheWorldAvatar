from pyuploader.uploaders.uploader import Uploader
import pyuploader.uploaders.file_uploader as fs_uploader
import pyuploader.uploaders.triple_store_uploader as ts_uploader
import pyuploader.errorhandling.appexceptions as appexcept
from typing import Optional

def get_uploader(
    uploader_type: str,
    default_url: Optional[str] = None,
    default_auth_file: Optional[str] = None,
    default_no_auth: bool = False) -> Uploader:

    if uploader_type == 'fs_uploader':
        uploader = fs_uploader.get_file_server_uploader(
            default_url=default_url,
            default_auth_file=default_auth_file,
            default_no_auth=default_no_auth
            )
    elif uploader_type == 'ts_uploader':
        uploader = ts_uploader.get_triple_store_uploader(
            default_url=default_url,
            default_auth_file=default_auth_file,
            default_no_auth=default_no_auth
            )
    else:
        raise appexcept.NotSupportedUploader(f"Error: Selected uploader type {uploader_type} is not supported.")
    return uploader