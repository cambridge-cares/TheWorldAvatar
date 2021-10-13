import json
import logging


def MarieLog(func):
    # @wraps(func)
    def wrapper(*args, **kwargs):
        logger = logging.getLogger('Function called')
        logger.setLevel(logging.INFO)
        logger.info('{} is called with input {}'.format(func.__name__, args[1:]))
        # logger.warning('Test our own warnings')
        return func(*args)

    return wrapper


def MarieQuestionLog(func):
    def wrapper(*args, **kwargs):
        logger = logging.getLogger('Question')
        logger.setLevel(logging.INFO)
        logger.info('\n======================== starting a question =========================')
        logger.info('Processing question {} \n'.format(args[1:]))
        return func(*args)

    return wrapper


def MarieError(err):
    logging.basicConfig(level=logging.DEBUG)
    logger = logging.getLogger('Error')
    logger.setLevel(logging.DEBUG)
    logger.error(err)


def MarieMessage(message):
    logging.basicConfig(level=logging.DEBUG)
    logger = logging.getLogger('Message')
    logger.setLevel(logging.DEBUG)
    logger.info(message)


def MarieIOLog(func):
    def wrapper(*args, **kwargs):
        logging.basicConfig(level=logging.DEBUG)
        logger = logging.getLogger('Function I/O')
        logger.setLevel(logging.DEBUG)
        rst = func(*args)
        if rst is None:
            logger.warning('{} is called with input {} but returned None'.format(func.__name__, args[1:]))
        else:
            logger.info('{} is called with input {} and output {}'.format(func.__name__, args[1:], rst))
        return rst

    return wrapper
