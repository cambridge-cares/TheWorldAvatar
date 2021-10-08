import pyuploader.triplestore.base as tsbase
import pyuploader.common.utils as utils
from pyuploader.common.gateways import jpsBaseLibGW
import logging
import pathlib

logger = logging.getLogger(__name__)

jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.upload.*")

def upload_to_triple_store(
        file_or_dir,
        url='',
        auth='',
        namespace='base',
        file_ext='.owl',
        dry_run= False):

    if not url: url = tsbase.get_striple_store_url()
    if not auth: auth = tsbase.get_user_credentials()
    files = utils.get_files_by_extensions(file_or_dir,file_ext)

    logger.info(f"---------------------------------------------------------------------------")
    logger.info(f"TRIPLE STORE UPLOAD")
    logger.info(f"---------------------------------------------------------------------------")
    if files:
        logger.info(f"Uploading files to the triple store: {url} at namespace: {namespace}.")
        for f in files:
            basenf = pathlib.Path(f).name
            logger.info(f"Uploading file: {basenf}")
            if not dry_run:
                KnowledgeRepository = jpsBaseLib_view.RemoteStoreClient(url, url, auth[0], auth[1], namespace,f,"")
                KnowledgeRepository.uploadOntology()
    else:
        logger.info('No files to upload')
    logger.info(f"---------------------------------------------------------------------------")