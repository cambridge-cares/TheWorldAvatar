from py4j.java_gateway import JavaGateway, java_import
from os import path

class Gateway:
    def __init__(self):
        self.gateway = None

    def start(self):
        self.gateway = JavaGateway.launch_gateway(jarpath=path.join(path.dirname(__file__),'resources','jps-base-lib.jar'),
                                        die_on_exit=True,
                                        cwd=path.join(path.dirname(__file__),'resources'))
    def shutdown(self):
        self.gateway.shutdown()

    def createModuleView(self):
        return self.gateway.new_jvm_view()

    @staticmethod
    def importJava(moduleView,importStatement):
        java_import(moduleView, importStatement)