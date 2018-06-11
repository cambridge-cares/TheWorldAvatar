import requests
import logging
from io import StringIO

logServerURL = 'http://www.theworldavatar.com/JPS_BASE/LogServer'
# logServerURL = 'http://localhost:8080/JPS_BASE/LogServer'

class LevelFilter(logging.Filter):
    def __init__(self, levels):
        self.levels = levels

    def filter(self, record):
        return record.levelno in self.levels

class PythonLogger(object):

    def __init__(self, pythonScript):
        self.log_stream = StringIO()
        self.logging = logging
        self.logging.basicConfig(stream=self.log_stream,
                                 level=logging.INFO,
                                 format='%(asctime)s %(levelname)s [Python] {} %(message)s'.format(pythonScript))

    def addFilter(self):
        self.logging.getLogger().addFilter(LevelFilter((logging.WARNING, logging.ERROR)))

    def postInfoToLogServer(self, message):
        self.logging.info(message)
        requests.post(logServerURL, data=self.log_stream.getvalue())
        self.log_stream.seek(0)
        self.log_stream.truncate(0)