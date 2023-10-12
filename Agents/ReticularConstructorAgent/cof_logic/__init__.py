import logging

logging.getLogger(__name__).addHandler(logging.NullHandler())
logging.basicConfig(format='%(asctime)s | %(message)s',
                    level=logging.INFO,
                    datefmt='%I:%M:%S')

