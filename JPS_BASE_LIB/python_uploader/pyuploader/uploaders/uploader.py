import pyuploader.common.utils as utils
import pyuploader.common.logconfig as logconfig
from typing import Callable, Dict, Tuple, Union
import pathlib
import logging
import os

logger = logging.getLogger(__name__)

BASE_URL_ENV_VAR_KEY = 'url_file'
BASE_AUTH_ENV_VAR_KEY = 'auth_file'

class Uploader:

    def __init__(
        self,
        upload_file_func: Callable,
        uploader_name: str = 'uploader',
        supported_file_ext: str = 'all'):

        self.upload_file_func = upload_file_func
        self.env_vars: Dict[str,str] = {BASE_URL_ENV_VAR_KEY: '',
                                        BASE_AUTH_ENV_VAR_KEY: ''}

        self.uploader_name = uploader_name
        self.supported_file_ext = supported_file_ext

    def set_url_env_var_value(self,
        url_env_var_value: str) -> None:
        self.env_vars[BASE_URL_ENV_VAR_KEY] = url_env_var_value

    def set_auth_env_var_value(self,
        auth_env_var_value: str) -> None:
        self.env_vars[BASE_AUTH_ENV_VAR_KEY] = auth_env_var_value

    def set_logging(self,
        log_file_dir: Union[str, None] = None,
        log_file_name: Union[str, None]= None,
        no_file_logging: bool = False):

        if log_file_name is None:
            log_file_name = self.uploader_name + '.log'

        logconfig.config_logging(
            log_file_dir,
            log_file_name,
            no_file_logging)

    def upload(self,
        url: Union[str, None],
        auth_file: Union[str, None],
        file_ext: str,
        dry_run: bool,
        file_or_dir: str,
        no_auth: bool,
        subdirs: Union[str, None]=None,
        *args, **kwargs) -> Dict[str,str]:

        if dry_run:
            logger.info(f"#######################")
            logger.info(f"## THIS IS A DRY-RUN ##")
            logger.info(f"#######################")
            logger.info(f"")

        if url is None: url = self._get_url()

        auth: Tuple[str,str] = ('','')
        if not no_auth: auth = self._get_auth(auth_file)

        if self.supported_file_ext != 'all':
            if file_ext != self.supported_file_ext:
                raise NotImplementedError(f"Only {self.supported_file_ext} files are currently supported.")
        files = utils.get_files_by_extensions(file_or_dir,file_ext)

        file_locations: Dict[str,str] = {}
        logger.info(f"---------------------------------------------------------------------------")
        logger.info(f"{self.uploader_name.upper()} UPLOAD")
        logger.info(f"---------------------------------------------------------------------------")
        if files:
            logger.info(f"Uploading files to the {self.uploader_name}...")
            for f in files:
                basenf = pathlib.Path(f).name
                logger.info(f"Uploading file: {basenf} to the {self.uploader_name}.")
                if not dry_run:
                    location = self._upload_file(url, auth, f, subdirs, *args, **kwargs)
                    logger.info(f"File: {basenf} successfully uploaded to: {location}.")
                    file_locations[f] = location
            logger.info(f"Uploading files to the {self.uploader_name} finished.")
        else:
            logger.info('No files to upload')
        logger.info(f"---------------------------------------------------------------------------")
        return file_locations

    def _upload_file(self,
        url: str,
        auth: Tuple[str,str],
        file_path: str,
        subdirs: Union[str,None],
        *args, **kwargs) -> str:
        location = self.upload_file_func(url, auth, file_path, subdirs, *args, **kwargs)
        return location

    def _get_url(self) -> str:
        logger.info(f"Reading the {self.uploader_name} specs file path from the environment variables.")
        url_file = utils.get_env_var_value(self.env_vars[BASE_URL_ENV_VAR_KEY])
        logger.info(f"Reading the {self.uploader_name} url from the specs file.")
        try:
            url = utils.read_file_content(url_file)
        except FileNotFoundError:
            raise FileNotFoundError(f"Error: The {self.uploader_name} specs file does not exist.")
        return url

    def _get_auth(self,
            auth_file: Union[str,None]) -> Tuple[str,str]:
        if auth_file is None:
            logger.info(f"Reading the {self.uploader_name} secrets file path from environment variables.")
        logger.info(f"Reading the {self.uploader_name} auth from the secrets file.")
        try:
            auth_file = utils.get_env_var_value(self.env_vars[BASE_AUTH_ENV_VAR_KEY])
        except FileNotFoundError:
            raise FileNotFoundError(f"Error: The {self.uploader_name} secrets file does not exist.")
        auth = utils.get_credentials_from_file(auth_file)

        return auth