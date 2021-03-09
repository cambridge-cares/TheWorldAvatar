from py4j.java_gateway import JavaGateway, java_import
from os import path
from py4jps.resRegistry.resManager import resReg
import textwrap

class JPSGateway:
    def __init__(self, resName=None, jarPath=None, **JGkwargs):
        self.resName = resName
        if jarPath is None:
            self.jarPath = resReg.getResMainFilePath(resName)
        else:
            self.jarPath = jarPath
        auto_convert = JGkwargs.pop("auto_convert", True)
        self.gateway = JavaGateway(**JGkwargs)
        self._isStarted = False

    def start(self, **LGkwargs):
        if not self._isStarted:
            if 'jarpath' in LGkwargs:
                LGkwargs.pop("jarpath")
                if self.__class__.__name__ == 'JPSGateway':
                    print("Warning: Skipping the 'jarpath' argument. The gateway jarpath can only be changed in the JPSGateway constructor via its 'jarPath' argument.")
                else:
                    print(textwrap.dedent("""\
                        Warning: Skipping the 'jarpath' argument. An installed resource jarpath can not be changed.
                                 Use the base 'JPSGateway' class if you wish to acces an uninstalled java resource (not recommended)
                                 or use the 'jpsrm' command line resource manager to install the new resource (recommended).
                        """))
            die_on_exit = LGkwargs.pop('die_on_exit', True)

            self.gateway = self.gateway.launch_gateway(jarpath=self.jarPath,  die_on_exit=die_on_exit, **LGkwargs)
            self._isStarted = True
        else:
            print("Info: JavaGateway already started.")

    def shutdown(self):
        if self._isStarted:
            self.gateway.shutdown()

    def createModuleView(self):
        if self._isStarted:
            return self.gateway.new_jvm_view()
        else:
            print("Error: Cannot create the module view. The JavaGateway is not started. Call the gateway start() method first.")

    def importPackages(self,moduleView,importStatement):
        if self._isStarted:
            java_import(moduleView,importStatement)
        else:
            print("Error: Cannot import packages. The JavaGateway is not started. Call the gateway start() method first.")