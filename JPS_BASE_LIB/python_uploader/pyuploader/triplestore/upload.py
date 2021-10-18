from py4j.protocol import Py4JError, Py4JJavaError
import textwrap
import pyuploader.triplestore.base as tsbase
import pyuploader.common.utils as utils
from pyuploader.common.gateways import jpsBaseLibGW
import logging
import pathlib

logger = logging.getLogger(__name__)

jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

def upload_to_triple_store(
        file_or_dir,
        url='',
        auth='',
        file_ext='owl',
        dry_run= False):

    if not url: url = tsbase.get_triple_store_url()
    if not auth: auth = tsbase.get_user_credentials()
    if file_ext != 'owl': raise NotImplementedError('Only owl files are currently supported.')
    files = utils.get_files_by_extensions(file_or_dir,file_ext)

    logger.info(f"---------------------------------------------------------------------------")
    logger.info(f"TRIPLE STORE UPLOAD")
    logger.info(f"---------------------------------------------------------------------------")
    if files:
        logger.info(f"Uploading files to the triple store: {url}.")
        for f in files:
            basenf = pathlib.Path(f).name
            logger.info(f"Uploading file: {basenf}")
            if not dry_run:
                upload_rdf_file_to_triple_store(url, auth, f)
    else:
        logger.info('No files to upload')
    logger.info(f"---------------------------------------------------------------------------")


def upload_rdf_file_to_triple_store(url, auth, f):
    client = jpsBaseLib_view.RemoteStoreClient(url, url, auth[0], auth[1])
    rdfFile = jpsBaseLib_view.java.io.File(f)
    client.uploadRDFFile(rdfFile)