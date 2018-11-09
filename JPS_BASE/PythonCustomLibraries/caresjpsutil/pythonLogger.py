import requests
import logging
import os
import traceback
from io import StringIO

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
        
        #if (cwd.startswith("C:/Users/Andreas") or cwd.startswith("C:\\Users\\Andreas")):
        #    self.logging.info("PythonLogger startswith True")
        #else:
        #     self.logging.info("PythonLogger startswith False")
        
        cwd = os.getcwd() # current working directory
        self.logServerURL = 'http://localhost:8080/JPS_BASE/LogServer'
        if (cwd.startswith("C:\\TOMCAT\\webapps") or cwd.startswith("C:/TOMCAT/webapps")):
            self.logServerURL =  'http://www.theworldavatar.com/JPS_BASE/LogServer'

    def addFilter(self):
        self.logging.getLogger().addFilter(LevelFilter((logging.WARNING, logging.ERROR)))

    def postInfoToLogServer(self, message):   
        
        if (issubclass(type(message), Exception)):
            self.postErrorToLogServer(message)
            return
         
        self.logging.info(message)
        requests.get(self.logServerURL, data=self.log_stream.getvalue())
        self.log_stream.seek(0)
        self.log_stream.truncate(0)
        
    def postErrorToLogServer(self, exception):   
        message = str(exception) + ', ' + traceback.format_exc()
        self.logging.error(message)  
        requests.get(self.logServerURL, data=self.log_stream.getvalue())
        self.log_stream.seek(0)
        self.log_stream.truncate(0)