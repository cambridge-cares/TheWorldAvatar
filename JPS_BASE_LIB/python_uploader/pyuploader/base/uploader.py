from abc import ABC, abstractmethod
import logging
import pyuploader.common.utils as utils
from typing import Dict, Tuple
import pathlib

logger = logging.getLogger(__name__)

BASE_URL_ENV_VAR_KEY = 'url_file'
BASE_AUTH_ENV_VAR_KEY = 'auth_file'

class Uploader(ABC):

    def __init__(self):
        self.uploader_descr: str = ''
        self.supported_file_ext: str = 'all'
        self.env_vars: Dict[str,str] = {BASE_URL_ENV_VAR_KEY: '',
                                        BASE_AUTH_ENV_VAR_KEY: ''}
    def upload(self,
        file_ext: str,
        url: str,
        auth_file: str,
        dry_run: bool,
        file_or_dir: str,
        no_auth: bool
    ):

        if url is None: url = self._get_url()

        auth: Tuple[str,str] = ('','')
        if not no_auth: auth = self._get_auth(auth_file)

        if self.supported_file_ext != 'all':
            if file_ext != self.supported_file_ext:
                raise NotImplementedError(f"Only {self.supported_file_ext} files are currently supported.")
        files = utils.get_files_by_extensions(file_or_dir,file_ext)

        file_locations: Dict[str,str] = {}
        logger.info(f"---------------------------------------------------------------------------")
        logger.info(f"{self.uploader_descr.upper()} UPLOAD")
        logger.info(f"---------------------------------------------------------------------------")
        if files:
            logger.info(f"Uploading files to the {self.uploader_descr}...")
            for f in files:
                basenf = pathlib.Path(f).name
                logger.info(f"Uploading file: {basenf} to the {self.uploader_descr}.")
                if not dry_run:
                    location = self.upload_file(f, url, auth)
                    logger.info(f"File: {basenf} successfully uploaded to: {location}.")
                    file_locations[f] = location
            logger.info(f"Uploading files to the file server finished.")
        else:
            logger.info('No files to upload')
        logger.info(f"---------------------------------------------------------------------------")
        return file_locations

    @abstractmethod
    def upload_file(self, f, url, auth, *args, **kwargs) -> str:
        pass

    def _get_url(self) -> str:
        logger.info(f"Reading the {self.uploader_descr} specs file path from the environment variables.")
        url_file = utils.get_env_var_value(self.env_vars[BASE_URL_ENV_VAR_KEY])
        logger.info(f"Reading the {self.uploader_descr} url from the specs file.")
        try:
            url = utils.read_file_content(url_file)
        except FileNotFoundError:
            raise FileNotFoundError(f"Error: The {self.uploader_descr} specs file does not exist.")
        return url

    def _get_auth(self,
            auth_file: str) -> Tuple[str,str]:
        if auth_file is None:
            logger.info(f"Reading the {self.uploader_descr} secrets file path from environment variables.")
        logger.info(f"Reading the {self.uploader_descr} auth from the secrets file.")
        try:
            auth_file = utils.get_env_var_value(self.env_vars[BASE_AUTH_ENV_VAR_KEY])
        except FileNotFoundError:
            raise FileNotFoundError(f"Error: The {self.uploader_descr} secrets file does not exist.")
        auth = utils.get_credentials_from_file(auth_file)

        return auth