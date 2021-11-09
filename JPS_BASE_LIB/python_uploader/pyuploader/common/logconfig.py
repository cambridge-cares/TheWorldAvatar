import logging
import os
from typing import Union

def config_logging(
    log_file_dir: Union[str,None],
    log_file_name: str,
    no_file_logging: bool) -> None:

    if log_file_dir is None: log_file_dir = os.getcwd()
    log_file = os.path.join(log_file_dir, log_file_name)

    logHandlers= []
    logHandlers.append(logging.StreamHandler())
    if not no_file_logging:
        logHandlers.append(logging.FileHandler(filename=log_file, mode='w'))

    logging.basicConfig(level=logging.DEBUG,
                        format='%(asctime)s [%(threadName)s] [%(levelname)s] %(message)s',
                        handlers=logHandlers)