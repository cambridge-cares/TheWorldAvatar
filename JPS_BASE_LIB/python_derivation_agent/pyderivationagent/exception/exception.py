from pyderivationagent.kg_operations.gateway import jpsBaseLibGW

jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.exception.*")

class PythonException:
    def __init__(self, err_msg: str) -> None:
        self.exception = jpsBaseLib_view.PythonException(err_msg)
