
from py4jps import JPSGateway
class JpsBaseLib(JPSGateway):
    def __init__(self,**JGkwargs):
        super(JpsBaseLib, self).__init__(resName='JpsBaseLib',**JGkwargs)
    __init__.__doc__ = JPSGateway.__init__.__doc__.replace('        resName : str\n            name of the Java resource'\
            +'\n        jarPath : str\n            absolute path to the main jar file of the java resource\n','')