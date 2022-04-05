from pyuploader.uploaders.uploader_factory import get_uploader
from pyuploader.uploaders.file_uploader import FS_UPLOADER
from pyuploader.uploaders.triple_store_uploader import TS_UPLOADER
import logging

logging.getLogger(__name__).addHandler(logging.NullHandler())
logging.getLogger('py4j').propagate = False