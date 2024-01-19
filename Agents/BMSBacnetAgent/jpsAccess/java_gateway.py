from py4jps.resources import JpsBaseLib

def singleton(class_):
    instances = {}
    def getinstance(*args, **kwargs):
        if class_ not in instances:
            instances[class_] = class_(*args, **kwargs)
        return instances[class_]
    return getinstance

@singleton
class jpsBaseLibView():
    def __init__(self):
        jpsBaseLibGW = JpsBaseLib()
        jpsBaseLibGW.launchGateway()
        self.jpsBaseLibViewO = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.jpsBaseLibViewO, "uk.ac.cam.cares.jps.base.query.*")
        jpsBaseLibGW.importPackages(self.jpsBaseLibViewO, "uk.ac.cam.cares.jps.base.timeseries.*")
    def getView(self):
        return self.jpsBaseLibViewO

