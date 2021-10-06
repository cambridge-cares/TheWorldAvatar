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
        tstore_nmsp,
        tstore_fileext,
        dry_run):

    tstore_url = tsbase.get_striple_store_url()
    files = utils.get_files_by_extensions(file_or_dir,tstore_fileext)

    logger.info(f"---------------------------------------------------------------------------")
    logger.info(f"TRIPLE STORE UPLOAD")
    logger.info(f"---------------------------------------------------------------------------")
    if files:
        logger.info(f"Uploading files to the triple store: {tstore_url} at namespace: {tstore_nmsp}.")
        for f in files:
            basenf = pathlib.Path(f).name
            logger.info(f"Uploading file: {basenf}")
            if not dry_run:
                KnowledgeRepository = jpsBaseLib_view.KnowledgeRepository(tstore_url, tstore_nmsp,f,"")
                KnowledgeRepository.uploadOntology()
    else:
        logger.info('No files to upload')
    logger.info(f"---------------------------------------------------------------------------")