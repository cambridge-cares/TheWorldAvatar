import pyuploader.common.utils as utils
import pyuploader.errorhandling.appexceptions as appexcept
from typing import Dict, Tuple, Optional, Callable
from abc import ABC, abstractmethod
import pathlib
import logging
import textwrap

logger = logging.getLogger(__name__)

BASE_URL_ENV_VAR_KEY = 'url_file'
BASE_AUTH_ENV_VAR_KEY = 'auth_file'

class Uploader(ABC):
    """Abstract uploader class."""
    def __init__(
        self,
        uploader_name: str = 'uploader',
        supported_file_ext: str = 'all',
        url: Optional[str] = None,
        auth_file: Optional[str] = None,
        no_auth: bool = False,
        subdirs: Optional[str] = None,
        url_env_var: str = '',
        auth_file_env_var: str = ''):

        self.uploader_name = uploader_name
        self._supported_file_ext = supported_file_ext

        self._env_vars: Dict[str,str] = {BASE_URL_ENV_VAR_KEY: url_env_var,
                                         BASE_AUTH_ENV_VAR_KEY: auth_file_env_var}

        url = url if url is not None else self._get_url()
        if not url.endswith('/'): url = f"{url}/"

        if subdirs is not None:
            if not subdirs.endswith('/'): subdirs = f"{subdirs}/"
            url = f"{url}{subdirs}"

        auth = ('','')
        if no_auth is False:
            auth = self._get_auth(auth_file=auth_file)

        self._upload_client: Callable[[str], str] =  self._get_upload_client(url=url, auth=auth)


    @abstractmethod
    def _get_upload_client(self, url: str, auth: Tuple[str,str])->Callable[[str],str]:
        """Abstract method for setting up the upload client function. The function
           should accept a single str argument, which is a filepath and return the
           file location on a server after the upload."""
        pass

    def set_url_env_var_value(self,
        url_env_var_value: str) -> None:
        self._env_vars[BASE_URL_ENV_VAR_KEY] = url_env_var_value

    def set_auth_env_var_value(self,
        auth_env_var_value: str) -> None:
        self._env_vars[BASE_AUTH_ENV_VAR_KEY] = auth_env_var_value

    def get_url_env_var_value(self) -> str:
        return self._env_vars[BASE_URL_ENV_VAR_KEY]

    def get_auth_env_var_value(self) -> str:
        return self._env_vars[BASE_AUTH_ENV_VAR_KEY]

    def upload(self,
        file_ext: str,
        file_or_dir: str,
        dry_run: bool = False,
        **kwargs) -> Dict[str,str]:

        if dry_run:
            logger.info(f"#######################")
            logger.info(f"## THIS IS A DRY-RUN ##")
            logger.info(f"#######################")
            logger.info(f"")

        if self._supported_file_ext != 'all':
            for file_ext_i in file_ext.split(','):
                if file_ext_i.strip() != self._supported_file_ext:
                    raise NotImplementedError(f"Only {self._supported_file_ext} files are currently supported.")
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
                    try:
                        location = self._upload_client(f)
                    except Exception as e:
                        raise appexcept.FileUploadError(textwrap.dedent("""
                            Error: Failed to upload file: #f#
                                Potential reasons and returned response status codes:
                                - Wrong upload url endpoint (400 - status code)
                                - Upload url endpoint is down (400 - status code)
                                - Wrong auth details (401 - status code)
                                - Unsupported file type (500 - status code)
                                - File is too large (500 - status code)
                        """.replace('#f#', f))) from e
                    logger.info(f"File: {basenf} successfully uploaded to: {location}.")
                    file_locations[f] = location
            logger.info(f"Uploading files to the {self.uploader_name} finished.")
        else:
            logger.info('No files to upload. Check if the file/directory exists or if the directory contains files that match the specified extensions.')
        logger.info(f"---------------------------------------------------------------------------")
        return file_locations


    def _get_url(self) -> str:
        logger.info(f"Reading the {self.uploader_name} specs file path from the environment variables.")
        url_file = utils.get_env_var_value(self._env_vars[BASE_URL_ENV_VAR_KEY])
        logger.info(f"Reading the {self.uploader_name} url from the specs file.")
        try:
            url = utils.read_file_content(url_file)
        except FileNotFoundError as ex:
            raise appexcept.SpecsFileNotFoundError(f"Error: The {self.uploader_name} specs file does not exist.") from ex
        return url

    def _get_auth(self,
            auth_file: Optional[str]) -> Tuple[str, str]:

        if auth_file is None:
            logger.info(f"Reading the {self.uploader_name} secrets file path from environment variables.")
            logger.info(f"Reading the {self.uploader_name} auth from the secrets file.")
            auth_file = utils.get_env_var_value(self._env_vars[BASE_AUTH_ENV_VAR_KEY])

        try:
            auth = utils.get_credentials_from_file(auth_file)
        except FileNotFoundError as ex:
            raise appexcept.SecretsFileNotFoundError(f"Error: The {self.uploader_name} secrets file does not exist.") from ex
        return auth