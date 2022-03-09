import logging

logging.getLogger(__name__).addHandler(logging.NullHandler())
logging.getLogger('py4j').propagate = False