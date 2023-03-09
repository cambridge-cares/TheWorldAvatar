import json
import logging
import sys
from functools import wraps
import os
import warnings
import tensorflow as tf
import numpy as np

if __name__ == '__main__':
    from location import SOURCE_DIR
else:
    from .location import SOURCE_DIR

# np.warnings.filterwarnings("ignore")
# # suppress other warnings
# warnings.filterwarnings("ignore")
# # suppress tensorflow warnings
# tf.get_logger().setLevel(logging.ERROR)
# os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'


def MarieLog(func):
    # @wraps(func)
    def wrapper(*args, **kwargs):
        # for handler in logging.root.handlers[:]:
        #     logging.root.removeHandler(handler)
        logging.basicConfig(filename=os.path.join(SOURCE_DIR, 'Marie.log'), filemode='w', level=logging.DEBUG)
        logger = logging.getLogger('Function called')
        logger.setLevel(logging.INFO)
        logger.info('{} is called with input {}'.format(func.__name__, args[1:]))
        # logger.warning('Test our own warnings')
        return func(*args)

    return wrapper


def MarieQuestionLog(func):
    def wrapper(*args, **kwargs):
        logging.basicConfig(filename=os.path.join(SOURCE_DIR, 'Marie.log'), filemode='w', level=logging.DEBUG)
        logger = logging.getLogger('Question')
        logger.setLevel(logging.INFO)
        logger.info('\n======================== starting a question =========================')
        logger.info('Processing question {} \n'.format(args[1:]))
        return func(*args)

    return wrapper


def MarieError(err):
    logging.basicConfig(filename=os.path.join(SOURCE_DIR, 'Marie.log'), filemode='w', level=logging.DEBUG)
    logger = logging.getLogger('Error')
    logger.setLevel(logging.ERROR)
    logger.error(err)


def MarieMessage(message):
    logging.basicConfig(filename=os.path.join(SOURCE_DIR, 'Marie2.log'), filemode='w', level=logging.DEBUG)
    logger = logging.getLogger('Message')
    logger.setLevel(logging.DEBUG)
    logger.info(message)


def MarieIOLog(func):
    def wrapper(*args, **kwargs):
        logging.basicConfig(filename=os.path.join(SOURCE_DIR, 'Marie.log'), filemode='w', level=logging.DEBUG)
        logger = logging.getLogger('Function I/O')
        logger.setLevel(logging.INFO)
        rst = func(*args)
        if rst is None:
            logger.warning('{} is called with input {} but returned None'.format(func.__name__, args[1:]))
        else:
            try:
                rst_string = json.dumps(rst, indent=4)
            except:
                rst_string = rst
            logger.info('{} is called with input {} and output {}'.format(func.__name__, args[1:], rst_string))
        return rst

    return wrapper


@MarieIOLog
def mock_function(input):
    return 'hello' + input


@MarieQuestionLog
def mock_questions(q):
    return 'this is a question {}'.format(q)


if __name__ == '__main__':
    mock_function(' world')
    mock_questions('what is the pce of somethign')
