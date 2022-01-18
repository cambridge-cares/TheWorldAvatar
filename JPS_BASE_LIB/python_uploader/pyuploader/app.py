from pyuploader.uploaders.uploader_factory import get_uploader
import logging
from typing import Optional, Dict

logger = logging.getLogger(__name__)

def app_upload(*args, **kwargs) -> None:
    try:
        upload(*args, **kwargs)
    except Exception as e:
        logger.error("Upload failed. Please check the log for a more detailed error description.")
        logger.exception(e)

def upload(
    uploader_type: str,
    file_or_dir: str,
    file_ext: str,
    url: Optional[str]=None,
    auth_file: Optional[str]=None,
    no_auth: bool= False,
    subdirs: Optional[str]=None,
    log_file_dir: Optional[str]= None,
    log_file_name: Optional[str]= None,
    no_file_logging: bool = False,
    dry_run: bool=False) -> Dict[str,str]:

    uploaded_locations = {}

    uploader = get_uploader(
        uploader_type=uploader_type,
        default_url=url,
        default_auth_file=auth_file,
        default_no_auth=no_auth
        )

    uploader.set_logging(
        log_file_dir=log_file_dir,
        log_file_name=log_file_name,
        no_file_logging=no_file_logging
        )

    uploaded_locations = uploader.upload(
        file_or_dir=file_or_dir,
        file_ext=file_ext,
        subdirs=subdirs,
        dry_run=dry_run
        )

    return uploaded_locations