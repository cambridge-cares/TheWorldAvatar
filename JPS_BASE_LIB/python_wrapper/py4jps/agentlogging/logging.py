# NOTE content of this file is copied from:
# https://github.com/cambridge-cares/TheWorldAvatar/tree/f290fb98ce746b591d8b8c93cca1e89a409c959e/Agents/utils/python-utils


from io import TextIOBase
import concurrent_log_handler
import logging.config
import sys
import os


class StreamToLogger(TextIOBase):
    """
        Fake file-like stream object that redirects writes to a logger instance.
    """

    # NOTE StreamToLogger is made to extend TextIOBase to prevent error like below when running pytest with docker-compose
    # as part of dockerised test in developing pyderivationagent package:
    # AttributeError: 'StreamToLogger' object has no attribute 'isatty'
    #
    # To reproduce the error, one may checkout to below commit and run "pytest -s --docker-compose=./docker-compose.test.yml" in the folder:
    # https://github.com/cambridge-cares/TheWorldAvatar/tree/ab354e2a759d812c64bb5236ba37d1ba9e53e552/JPS_BASE_LIB/python_derivation_agent
    #
    # This error was due to below line in pytest checking if sys.stdout.isatty() is True/False
    # https://github.com/pytest-dev/pytest/blob/main/src/_pytest/terminal.py#L332
    #
    # another fix is to provide "def isatty(self) -> bool:"" but extending TextIOBase seems to be a "safer"/"cleaner" fix,
    # according to:
    # https://stackoverflow.com/questions/19425736/how-to-redirect-stdout-and-stderr-to-logger-in-python#comment114971340_39215961

    def __init__(self, logger, log_level=logging.DEBUG):
        self.logger = logger
        self.log_level = log_level
        self.linebuf = ''

    def write(self, buf):
        for line in buf.rstrip().splitlines():
            self.logger.log(self.log_level, line.rstrip())

    def flush(self):
        pass


def _config_logging():
    """
        Initialise and configure loggers.
    """
    # Create logs directory
    log_dir = os.path.join(os.path.expanduser("~"), ".jps", "logs")
    if not os.path.isdir(log_dir):
        os.makedirs(log_dir)

    # Set the logfile to be generated in ~/.jps/logs
    log_fpath = os.path.join(os.path.expanduser("~"), ".jps", "logs", "jps.log")

    # Configure logging from file
    this_dir = os.path.dirname(os.path.abspath(__file__))
    log_file = os.path.join(this_dir, "logging.conf")

    logging.config.fileConfig(
        log_file,
        defaults={'log_fpath': log_fpath}
    )

    # Redirect standard out to use the root logger
    stdout_logger = logging.getLogger("root")
    sys.stdout = StreamToLogger(stdout_logger, logging.DEBUG)


def get_logger(logger_name):
    """
        Get the dev or prod logger (avoids having to import 'logging' in calling code).

        Parameters:
            logger_name - 'dev' or 'prod'

        Returns:
            Logger to use for logging statements.
    """
    valid_logger_names = ['dev','prod']

    if logger_name in valid_logger_names:
        return logging.getLogger(logger_name)
    else:
        raise ValueError("Invalid logger name: allowed values are "+",".join(valid_logger_names))
        return None


def shutdown():
    """
        Shutdown the logging system, should be called
        before application exit after all logging calls.
    """
    logging.shutdown()


# Perform configuration on module import
_config_logging()
