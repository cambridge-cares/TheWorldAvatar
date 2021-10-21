import pyuploader.triplestore.base as tsbase
import pyuploader.common.utils as utils
from pyuploader.common.gateways import jpsBaseLibGW
import logging
import pathlib
from typing import Union, Tuple

logger = logging.getLogger(__name__)

jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

def upload_to_triple_store(
        file_or_dir: str,
        url: Union[str, None]=None,
        auth_str: Union[str, None]=None,
        file_ext: str='owl',
        dry_run: bool= False) -> None:

    if url is None: url = tsbase.get_tstore_url()
    if auth_str is None:  auth_str = tsbase.get_tstore_credentials_from_envar()

    auth = utils.get_credentials_from_str(auth_str)
    if file_ext != 'owl': raise NotImplementedError('Only owl files are currently supported.')
    files = utils.get_files_by_extensions(file_or_dir,file_ext)

    logger.info(f"---------------------------------------------------------------------------")
    logger.info(f"TRIPLE STORE UPLOAD")
    logger.info(f"---------------------------------------------------------------------------")
    if files:
        logger.info(f"Uploading files to the triple store...")
        for f in files:
            basenf = pathlib.Path(f).name
            logger.info(f"Uploading file: {basenf}")
            if not dry_run:
                upload_rdf_file_to_triple_store(url, auth, f)
        logger.info(f"Uploading files to the triple store finished.")
    else:
        logger.info('No files to upload')
    logger.info(f"---------------------------------------------------------------------------")


def upload_rdf_file_to_triple_store(
    url: str,
    auth: Tuple[str, str],
    f: str) -> None:

    client = jpsBaseLib_view.RemoteStoreClient(url, url, auth[0], auth[1])
    rdfFile = jpsBaseLib_view.java.io.File(f)
    client.uploadRDFFile(rdfFile)