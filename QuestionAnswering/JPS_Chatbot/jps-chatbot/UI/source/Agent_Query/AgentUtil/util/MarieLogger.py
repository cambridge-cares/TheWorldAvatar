import json
import logging
import os

if __name__ == '__main__':
    from location import ROOT_DIR
else:
    from .location import ROOT_DIR


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
        logging.basicConfig(filename=os.path.join(ROOT_DIR, 'Marie.log'), filemode='a', level=logging.DEBUG)
        logger = logging.getLogger('Question')
        logger.setLevel(logging.INFO)
        logger.info('\n======================== starting a question =========================')
        logger.info('Processing question {} \n'.format(args[1:]))
        return func(*args)

    return wrapper


def MarieError(err):
    # for handler in logging.root.handlers[:]:
    #     logging.root.removeHandler(handler)
    logging.basicConfig(filename=os.path.join(ROOT_DIR, 'Marie.log'), filemode='a', level=logging.DEBUG)
    logging.basicConfig(level=logging.DEBUG)
    logger = logging.getLogger('Error')
    logger.setLevel(logging.DEBUG)
    logger.error(err)


def MarieMessage(message):
    for handler in logging.root.handlers[:]:
        logging.root.removeHandler(handler)
    #logging.basicConfig(filename=os.path.join(ROOT_DIR, 'Marie.log'), filemode='a', level=logging.INFO)
    logger = logging.getLogger('Message')
    logger.setLevel(logging.INFO)
    logger.debug(message)


def MarieIOLog(func):
    def wrapper(*args, **kwargs):
        # for handler in logging.root.handlers[:]:
        #     logging.root.removeHandler(handler)
        # logging.basicConfig(filename=os.path.join(ROOT_DIR, 'Marie.log'), filemode='w', level=logging.DEBUG)
        logger = logging.getLogger('Function I/O')
        logger.setLevel(logging.DEBUG)
        rst = func(*args)
        if rst is None:
            logger.warning('{} is called with input {} but returned None'.format(func.__name__, args[1:]))
        else:
            logger.warning('{} is called with input {} and output {}'.format(func.__name__, args[1:], rst))
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