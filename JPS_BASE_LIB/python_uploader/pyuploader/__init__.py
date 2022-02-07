from pyuploader.uploaders.uploader_factory import get_uploader
import logging

logging.getLogger(__name__).addHandler(logging.NullHandler())
logging.getLogger('py4j').propagate = False