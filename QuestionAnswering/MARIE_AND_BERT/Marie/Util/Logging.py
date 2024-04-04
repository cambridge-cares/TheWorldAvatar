import logging
import os

from Marie.Util.location import ROOT_DIR


class MarieLogger:

    def __init__(self):
        self.setup_logger('error', os.path.join(ROOT_DIR, 'error.log'))
        self.setup_logger('debug', os.path.join(ROOT_DIR, 'debug.log'))
        self.error_log = logging.getLogger('error')
        self.debug_log = logging.getLogger('debug')
        self.error_log.propagate = False
        self.debug_log.propagate = False

    def setup_logger(self, logger_name, log_file, level=logging.INFO):
        l = logging.getLogger(logger_name)
        if not len(l.handlers):
            formatter = logging.Formatter('%(asctime)s : %(levelname)s: %(message)s')
            fileHandler = logging.FileHandler(log_file)
            fileHandler.setFormatter(formatter)
            streamHandler = logging.StreamHandler()
            streamHandler.setFormatter(formatter)
            l.setLevel(level)
            l.addHandler(fileHandler)
            l.addHandler(streamHandler)

    def error(self, message):
        self.error_log.error(message)

    def critical(self, message):
        self.error_log.critical(message)

    def debug(self, message):
        self.debug_log.debug(message)

    def info(self, message):
        self.debug_log.info(message)


if __name__ == "__main__":
    ml = MarieLogger()
    ml.error("something is wrong")
    ml.critical("Something serious is wrong")
    ml.info("Just for your information lolololo")

    # ml.debug("Debugging information")
