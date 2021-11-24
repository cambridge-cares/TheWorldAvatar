import pyuploader.common.utils as utils
import pyuploader.common.logconfig as logconfig
import pyuploader.errorhandling.appexceptions as appexcept
from typing import Callable, Dict, Tuple, Union
import pathlib
import logging
import textwrap

logger = logging.getLogger(__name__)

BASE_URL_ENV_VAR_KEY = 'url_file'
BASE_AUTH_ENV_VAR_KEY = 'auth_file'

class Uploader:

    def __init__(
        self,
        upload_file_func: Callable,
        uploader_name: str = 'uploader',
        supported_file_ext: str = 'all',
        default_url: Union[str, None] = None,
        default_auth_file: Union[str, None] = None,
        default_no_auth: bool = False):

        self.upload_file_func = upload_file_func
        self.env_vars: Dict[str,str] = {BASE_URL_ENV_VAR_KEY: '',
                                        BASE_AUTH_ENV_VAR_KEY: ''}

        self.uploader_name = uploader_name
        self.supported_file_ext = supported_file_ext
        self.default_url = default_url
        self.default_auth_file = default_auth_file
        self.default_no_auth = default_no_auth

    def set_url_env_var_value(self,
        url_env_var_value: str) -> None:
        self.env_vars[BASE_URL_ENV_VAR_KEY] = url_env_var_value

    def set_auth_env_var_value(self,
        auth_env_var_value: str) -> None:
        self.env_vars[BASE_AUTH_ENV_VAR_KEY] = auth_env_var_value

    def get_url_env_var_value(self) -> str:
        return self.env_vars[BASE_URL_ENV_VAR_KEY]

    def get_auth_env_var_value(self) -> str:
        return self.env_vars[BASE_AUTH_ENV_VAR_KEY]

    def set_logging(self,
        log_file_dir: Union[str, None] = None,
        log_file_name: Union[str, None]= None,
        no_file_logging: bool = False):

        if log_file_name is None:
            log_file_name = self.uploader_name.replace(' ', '_') + '.log'

        logconfig.config_logging(
            log_file_dir,
            log_file_name,
            no_file_logging)

    def upload(self,
        file_ext: str,
        file_or_dir: str,
        url: Union[str, None] = None,
        auth_file: Union[str, None] = None,
        no_auth: Union[bool,None] = None,
        dry_run: bool = False,
        subdirs: Union[str, None]=None,
        *args, **kwargs) -> Dict[str,str]:

        if dry_run:
            logger.info(f"#######################")
            logger.info(f"## THIS IS A DRY-RUN ##")
            logger.info(f"#######################")
            logger.info(f"")

        # url and auth details
        #-------------------------------------------
        if url is None: url= self.default_url
        if url is None: url= self._get_url()

        auth: Tuple[str,str] = ('','')
        if no_auth is None: no_auth = self.default_no_auth
        if not no_auth:
            if auth_file is None: auth_file= self.default_auth_file
            auth = self._get_auth(auth_file)
        #-------------------------------------------

        if self.supported_file_ext != 'all':
            for file_ext_i in file_ext.split(','):
                if file_ext_i.strip() != self.supported_file_ext:
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
                    try:
                        location = self._upload_file(url, auth, f, subdirs, *args, **kwargs)
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

    def _upload_file(self,
        url: str,
        auth: Tuple[str,str],
        file_path: str,
        subdirs: Union[str,None]=None,
        *args, **kwargs) -> str:
        location = self.upload_file_func(url, auth, file_path, subdirs, *args, **kwargs)
        return location

    def _get_url(self) -> str:
        logger.info(f"Reading the {self.uploader_name} specs file path from the environment variables.")
        url_file = utils.get_env_var_value(self.env_vars[BASE_URL_ENV_VAR_KEY])
        logger.info(f"Reading the {self.uploader_name} url from the specs file.")
        try:
            url = utils.read_file_content(url_file)
        except FileNotFoundError as ex:
            raise appexcept.SpecsFileNotFoundError(f"Error: The {self.uploader_name} specs file does not exist.") from ex
        return url

    def _get_auth(self,
            auth_file: Union[str,None]) -> Tuple[str,str]:

        if auth_file is None:
            logger.info(f"Reading the {self.uploader_name} secrets file path from environment variables.")
            logger.info(f"Reading the {self.uploader_name} auth from the secrets file.")
            auth_file = utils.get_env_var_value(self.env_vars[BASE_AUTH_ENV_VAR_KEY])

        try:
            auth = utils.get_credentials_from_file(auth_file)
        except FileNotFoundError as ex:
            raise appexcept.SecretsFileNotFoundError(f"Error: The {self.uploader_name} secrets file does not exist.") from ex
        return auth