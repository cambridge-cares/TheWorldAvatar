from pyuploader.common.gateways import jpsBaseLibGW
from pyuploader.uploaders.uploader import Uploader
import functools as functools
from typing import Tuple, Optional, Callable

jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

TS_URL_ENV_VAR_VALUE = 'TRIPLE_STORE_SPECS'
TS_AUTH_ENV_VAR_VALUE = 'TRIPLE_STORE_SECRETS'

TS_UPLOADER = 'triple store'

class Triple_Store_Uploader(Uploader):
    """Triple store uploader class."""
    def __init__(
        self,
        uploader_name: str = TS_UPLOADER,
        url: Optional[str] = None,
        auth_file: Optional[str] = None,
        no_auth: bool = False,
        url_env_var: Optional[str] = None,
        auth_file_env_var: Optional[str] = None):

        super().__init__(
            uploader_name=uploader_name,
            supported_file_ext='owl',
            url=url,
            auth_file=auth_file,
            no_auth=no_auth,
            url_env_var=url_env_var if url_env_var is not None else TS_URL_ENV_VAR_VALUE,
            auth_file_env_var=auth_file_env_var if auth_file_env_var is not None else TS_AUTH_ENV_VAR_VALUE)


    def _get_upload_client(self, url: str, auth: Tuple[str, str]) -> Callable[[str], str]:
        client = jpsBaseLib_view.RemoteStoreClient(
                url, url, auth[0], auth[1])
        return functools.partial(self.__upload_wrapper, client, url)

    @staticmethod
    def __upload_wrapper(client: Callable, url: str, file_path: str)->str:
        rdfFile = jpsBaseLib_view.java.io.File(file_path)
        client.uploadRDFFile(rdfFile)
        return url

def get_triple_store_uploader(
        uploader_name: str = 'triple store',
        url: Optional[str] = None,
        auth_file: Optional[str] = None,
        no_auth: bool = False,
        url_env_var: Optional[str] = None,
        auth_file_env_var: Optional[str] = None) -> Uploader:

    ts_uploader = Triple_Store_Uploader(
        uploader_name=uploader_name,
        url = url,
        auth_file = auth_file,
        no_auth=no_auth,
        url_env_var = url_env_var,
        auth_file_env_var = auth_file_env_var
        )

    return ts_uploader
