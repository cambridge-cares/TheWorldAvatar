from pyuploader.uploaders.uploader_factory import get_uploader
import logging
from typing import Union, Dict

logger = logging.getLogger(__name__)

def upload(
    uploader_type: str,
    file_or_dir: str,
    file_ext: str,
    url: Union[str, None]=None,
    auth_file: Union[str, None]=None,
    no_auth: bool= False,
    subdirs: Union[str, None]=None,
    log_file_dir: Union[str, None]= None,
    log_file_name: Union[str, None]= None,
    no_file_logging: bool = False,
    dry_run: bool=False) -> Dict[str,str]:

    uploaded_locations = {}
    try:
        uploader = get_uploader(uploader_type)

        uploader.set_logging(
            log_file_dir=log_file_dir,
            log_file_name=log_file_name,
            no_file_logging=no_file_logging
        )

        uploaded_locations = uploader.upload(
            file_or_dir=file_or_dir,
            file_ext=file_ext,
            url=url,
            auth_file=auth_file,
            no_auth=no_auth,
            subdirs=subdirs,
            dry_run=dry_run
            )

    except Exception as e:
        logger.error("Upload failed. Please check the log for a more detailed error description.")
        logger.exception(e)

    return uploaded_locations