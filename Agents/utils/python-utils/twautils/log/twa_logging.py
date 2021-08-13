import concurrent_log_handler
import logging.config
import sys
import os


class StreamToLogger(object):
    """
        Fake file-like stream object that redirects writes to a logger instance.
    """
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
        os.mkdirs(log_dir)
    
    # Set the logfile to be generated in ~/.jps/logs
    log_fpath = os.path.join(os.path.expanduser("~"), ".jps", "logs", "jps.log")
    
    # Configure logging from file
    this_dir = os.path.dirname(os.path.abspath(__file__))
    log_file = os.path.join(this_dir, "twa_logging.conf")
    
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