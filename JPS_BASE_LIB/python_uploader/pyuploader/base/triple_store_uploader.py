from typing import Dict, Tuple
from pyuploader.common.gateways import jpsBaseLibGW
from pyuploader.base.uploader import Uploader, \
                                     BASE_URL_ENV_VAR_KEY, \
                                     BASE_AUTH_ENV_VAR_KEY

jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

TS_URL_ENV_VAR_VALUE = 'TRIPLE_STORE_SPECS'
TS_AUTH_ENV_VAR_VALUE = 'TRIPLE_STORE_SECRETS'

class TripleStoreUploader(Uploader):

    def __init__(self):
        super().__init__()
        self.supported_file_ext = 'owl'
        self.uploader_descr = 'triple store uploader'
        self.env_vars[BASE_URL_ENV_VAR_KEY] = TS_URL_ENV_VAR_VALUE
        self.env_vars[BASE_AUTH_ENV_VAR_KEY] = TS_AUTH_ENV_VAR_VALUE

    def upload_file(
        self,
        url: str,
        auth: Tuple[str,str],
        file_path: str) -> str:

        client = jpsBaseLib_view.RemoteStoreClient(url, url, auth[0], auth[1])
        rdfFile = jpsBaseLib_view.java.io.File(file_path)
        client.uploadRDFFile(rdfFile)

        return url