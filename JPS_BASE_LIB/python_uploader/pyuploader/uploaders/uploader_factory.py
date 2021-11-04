from pyuploader.uploaders.uploader import Uploader
import pyuploader.uploaders.file_uploader as fs_uploader
import pyuploader.uploaders.triple_store_uploader as ts_uploader
import pyuploader.errorhandling.appexceptions as appexcept

def get_uploader(uploader_type: str) -> Uploader:
    if uploader_type == 'fs_uploader':
        uploader = fs_uploader.get_file_server_uploader()
    elif uploader_type == 'ts_uploader':
        uploader = ts_uploader.get_triple_store_uploader()
    else:
        raise appexcept.NotSupportedUploader(f"Error: Selected uploader type {uploader_type} is not supported.")
    return uploader