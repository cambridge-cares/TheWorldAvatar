from py4j.java_gateway import JavaGateway, java_import, launch_gateway ,GatewayParameters
from os import path
from py4jps.resRegistry.resManager import resReg
import textwrap


def _processLGkwargs(_LG_DEF, className, **kwargs):
    """
    Pre-processes launch_gateway arguments, inserting overidden defaults if necessary.

    Arguments:
    ----------
    _LG_DEF : dict
        overidden launch_gateway default arguments - hardcoded
    className : str
        name of the class that made the call
    kwargs : dict, optional
        dictionary with a user set arguments

    Returns:
    LGkwargs: dict
        pre-processed launch_gateway arguments dictionary
    """
    LGkwargs = kwargs.pop('LGkwargs',{})
    if 'jarpath' in LGkwargs:
        LGkwargs.pop("jarpath",'')
        if className == 'JPSGateway':
            print(textwrap.dedent("""\
                Warning: Skipping the 'jarpath' parameter of the launch_gateway method. The gateway 'jarpath' can only
                            be changed in the JPSGateway constructor via its 'jarPath' argument."""))
        else:
            print(textwrap.dedent("""\
                Warning: Skipping the 'jarpath' argument. An installed resource jarpath can not be changed.
                        Use the base 'JPSGateway' class if you wish to acces an uninstalled java resource (not recommended)
                        or use the 'jpsrm' command line resource manager to install the new resource (recommended).
                """))
    elif 'return_proc' in LGkwargs:
        LGkwargs.pop('return_proc',False)
        print(textwrap.dedent("""\
                Warning: Skipping the 'return_process' parameter of the launch_gateway method. The parameter must always be True
                            and cannot be changed by a user."""))

    return {**_LG_DEF, **LGkwargs}

def _processJGkwargs(_JG_DEF, port, proc, auth_token, **kwargs):
    """
    Pre-processes JavaGateway constructor arguments, inserting overidden defaults if necessary.

    Arguments:
    ----------
    _JG_DEF : dict
        overidden gateway constructor default arguments - hardcoded
    port : int
        the port to launch the Java Gateway on
    proc: subprocess.Popen object
        the subprocess.Popen object for the Java process that the JavaGateway shall connect to
    kwargs : dict, optional
        dictionary with a user set arguments

    Returns:
    ----------
    JGkwargs: dict
        pre-processed JavaGateway arguments dictionary
    """
    # init JGkwargs
    JGkwargs = kwargs.pop('JGkwargs',{})
    # replace any missing values with defaults
    JGkwargs = {**_JG_DEF, **JGkwargs}

    # add JG args returned by the launch call
    JGkwargs.update({'java_process':proc})
    # wrap user defined gateway_parameters into GatewayParameters object
    gateway_parameters = JGkwargs['gateway_parameters']
    # insert the params returned from the launch_gateway call
    gateway_parameters = {**gateway_parameters, **{'port':port, 'auth_token':auth_token}}
    JGkwargs.update({'gateway_parameters': GatewayParameters(**gateway_parameters)})
    return JGkwargs

class JPSGateway:
    """
    Wrapper class of the py4j JavaGateway class for managing Python-Java communication.

    Attributes:
    ----------
    resName : str
        name of the Java resource
    jarPath : str
        absolute path to the main jar file of the java resource
    gateway : JavaGateway objects
        the gateway object handling Python-Java communication
    _isStarted : bool
        flag indicating if the gateway was launched
    """
    def __init__(self, resName=None, jarPath=None):
        self.resName = resName
        if jarPath is None:
            self.jarPath = resReg.getResMainFilePath(resName)
        else:
            self.jarPath = jarPath
        self.gateway = None
        self._isStarted = False

    def launchGateway(self, **kwargs):
        """
        Wrapper method for the py4j launch_gateway method which launches a Gateway in a new Java process
        and creates a default JavaGateway to connect to it.

        Arguments:
        ----------
        kwargs : dict
            A dictionary containing the py4j JavaGateway constructor and py4j launch_gateway method arguments.
            The format is as follows:
                kwargs = {'JGkwargs': {any JavaGateway constructor kwargs}, 'LGkwargs': {any launch_gateway kwargs}}

            Note that the JGkwargs related to the 'gateway_parameters' can be passed as a dictionary which then will
            be automatically wrapped into the 'GatewayParameters' object. As an example, the following arguments:

                kwargs = {'JGkwargs':{'gateway_parameters':{'auto_convert':True}}

            will be automatically converted to:

                kwargs = {'JGkwargs':{'gateway_parameters': GatewayParameters(auto_convert=True)}}

            Note that the LGkwargs 'jarpath' and 'return_proc' arguments cannot be changed in this method and will
            be neglected if provided
        """
        # launch_gateway defaults
        _LG_DEF = {'die_on_exit':True,
                'return_proc':True,
                'enable_auth': False,
                'jarpath': self.jarPath}

        # java_gateway defaults
        _JG_DEF = {'auto_convert': True,
                'gateway_parameters': {'auto_convert':True}}

        if not self._isStarted:
            LGkwargs = _processLGkwargs(_LG_DEF, self.__class__.__name__, **kwargs)

            _ret = launch_gateway(**LGkwargs)
            if LGkwargs['enable_auth']:
                _port, _auth_token, proc = _ret
            else:
                _port, proc, _auth_token = _ret + (None, )

            JGkwargs = _processJGkwargs(_JG_DEF, _port, proc, _auth_token, **kwargs)
            self.gateway = JavaGateway(**JGkwargs)
            self._isStarted = True
        else:
            print("Info: JavaGateway already started.")

    def shutdown(self):
        """
        Wrapper method for the py4j shutdown method to stop the JavaGateway client.
        """
        if self._isStarted:
            self.gateway.shutdown()

    def createModuleView(self):
        """
        Wrapper method for the py4j new_jvm_view method. Creates a new JVM view with its own imports.

        Returns:
        ----------
        new_jvm_view : JavaGateway.JVM
            A new JVM view object
        """
        if self._isStarted:
            return self.gateway.new_jvm_view()
        else:
            print("Error: Cannot create the module view. The JavaGateway is not started. Call the gateway start() method first.")

    def importPackages(self,moduleView,importStatement):
        """
        Wrapper method for the py4j java_import method. Imports a class / package into the specified JVM view

        Arguments:
        ----------
        jvm_view : JavaGateway.JVM
            A new JVM view object
        importStatement : str
            The class / package name to import
        """
        if self._isStarted:
            java_import(moduleView,importStatement)
        else:
            print("Error: Cannot import packages. The JavaGateway is not started. Call the gateway start() method first.")