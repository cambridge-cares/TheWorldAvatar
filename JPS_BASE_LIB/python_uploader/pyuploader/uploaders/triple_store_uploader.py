from pyuploader.common.gateways import jpsBaseLibGW
from pyuploader.uploaders.uploader import Uploader
from typing import Tuple, Optional

jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

TS_URL_ENV_VAR_VALUE = 'TRIPLE_STORE_SPECS'
TS_AUTH_ENV_VAR_VALUE = 'TRIPLE_STORE_SECRETS'

def get_triple_store_uploader(
        uploader_name: str = 'triple store',
        supported_file_ext: str='owl',
        default_url: Optional[str] = None,
        default_auth_file: Optional[str] = None,
        default_no_auth: bool = False
        ) -> Uploader:

    ts_uploader = Uploader(
        upload_file_func= upload_file,
        uploader_name=uploader_name,
        supported_file_ext=supported_file_ext,
        default_url = default_url,
        default_auth_file = default_auth_file,
        default_no_auth=default_no_auth
        )

    ts_uploader.set_url_env_var_value(TS_URL_ENV_VAR_VALUE)
    ts_uploader.set_auth_env_var_value(TS_AUTH_ENV_VAR_VALUE)

    return ts_uploader

def upload_file(
    url: str,
    auth: Tuple[str,str],
    file_path: str,
    *args,
    **kwargs) -> str:

    client = jpsBaseLib_view.RemoteStoreClient(url, url, auth[0], auth[1])
    rdfFile = jpsBaseLib_view.java.io.File(file_path)
    client.uploadRDFFile(rdfFile)

    return url