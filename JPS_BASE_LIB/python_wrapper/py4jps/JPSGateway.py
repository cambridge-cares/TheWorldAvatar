from py4j.java_gateway import JavaGateway, java_import
from os import path
from py4jps.resRegistry.resManager import resReg

class JPSGateway:
    def __init__(self, resName=None, jarPath=None):
        self.gateway = None
        self.resName = resName
        if jarPath is None:
            self.jarPath = resReg.getResMainFilePath(resName)
        else:
            self.jarPath = jarPath
        self._isStarted = False

    def start(self):
        if not self._isStarted:
            self.gateway = JavaGateway.launch_gateway(jarpath=self.jarPath,  die_on_exit=True)
            self._isStarted = True
        else:
            print('Info: JPSGateway already started.')

    def shutdown(self):
        if self._isStarted:
            self.gateway.shutdown()

    def createModuleView(self):
        if self._isStarted:
            return self.gateway.new_jvm_view()
        else:
            print('Error: Cannot create the module view. JPSGateway not started. Call the JPSGateway start() method first.')

    def importPackages(self,moduleView,importStatement):
        if self._isStarted:
            java_import(moduleView,importStatement)
        else:
            print('Error: Cannot import packages. JPSGateway not started. Call the JPSGateway start() method first.')