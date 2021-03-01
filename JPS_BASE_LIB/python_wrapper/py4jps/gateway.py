from py4j.java_gateway import JavaGateway, java_import
from os import path

class JPSGateway:
    _jarPath = path.join(path.dirname(__file__),'resources','jps-base-lib.jar')

    def __init__(self):
        self.gateway = None
        self._isStarted = False

    def start(self):
        self.gateway = JavaGateway.launch_gateway(jarpath=self._jarPath,  die_on_exit=True)
        self._isStarted = True

    def shutdown(self):
        if self._isStarted:
            self.gateway.shutdown()

    def createModuleView(self):
        if self._isStarted:
            return self.gateway.new_jvm_view()
        else:
            print('Error: Cannot create the module view. Gateway not started. Call the Gateway start() method first.')

    def importPackages(self,moduleView,importStatement):
        if self._isStarted:
            java_import(moduleView, importStatement)
        else:
            print('Error: Cannot import packages. Gateway not started. Call the Gateway start() method first.')