from py4j.java_gateway import JavaGateway, java_import, launch_gateway ,GatewayParameters
from os import path
from py4jps.resRegistry.resManager import resReg
import textwrap

def _processLGkwargs(className, **LGkwargs):
    """
    Pre-processes the launch_gateway arguments.

    Arguments:
    ----------
    className : str
        name of the class that made the call
    LGkwargs : dict, optional
        dictionary with a user set arguments

    Returns:
    ----------
    LGkwargs: dict
        pre-processed launch_gateway arguments dictionary
    """
    _SKIPPED_ARGS = {
        'jarpath': ("Warning: Skipping the 'jarpath' parameter of the launch_gateway method."
                   " The gateway 'jarpath' can only be changed in the JPSGateway constructor"
                   " via its 'jarPath' argument.")
                   if className == 'JPSGateway' else \
                   ("Warning: Skipping the 'jarpath' argument. An installed resource jarpath"
                    " can not be changed. Use the base 'JPSGateway' class if you wish to acces"
                    " an uninstalled java resource (not recommended) or use the 'jpsrm' command"
                    " line resource manager to install the new resource (recommended)")
                   ,
        'return_proc':  ("Warning: Skipping the 'return_process' parameter of the launch_gateway"
                         " method. The parameter must always be True and cannot be changed by a user.")
                 }

    # launch_gateway defaults
    _LG_DEF = {'die_on_exit': True,
               'return_proc': True,
               'enable_auth': False}

    for arg, arg_val in _SKIPPED_ARGS.items():
        if arg in LGkwargs:
            LGkwargs.pop(arg)
            print(arg_val)

    # add the defaults to the LGkwargs, defaults are added if missing
    # so they do not overwrite any present LGkwargs values
    LGkwargs = {**_LG_DEF, **LGkwargs}
    return LGkwargs

def _processJGkwargs(**JGkwargs):
    """
    Pre-processes JavaGateway constructor arguments.

    Arguments:
    ----------
    JGkwargs : dict, optional
        dictionary with a user set arguments

    Returns:
    ----------
    JGkwargs: dict
        pre-processed JavaGateway arguments dictionary
    """
    _SKIPPED_ARGS = {
        'java_process':("Warning: Skipping the 'java_process' argument of the JavaGateway class."
                        " The argument is automatically set during the launch_gateway call and"
                        " cannot be changed by a user."),
        'auth_token':  ("Warning: Skipping the 'auth_token' argument of the JavaGateway class."
                        " The argument is automatically set by the launch_gateway method if"
                        " 'enable_auth' flag is set to True."),
        'port':        ("Warning: Skipping the 'port' argument of the JavaGateway class."
                        " The argument should be set in the launch_gateway method instead."),
                 }
    # java_gateway defaults
    _JG_DEF = {'auto_convert': True,
            'gateway_parameters': {'auto_convert': True}}

    # add the defaults to the JGkwargs
    if 'gateway_parameters' in JGkwargs:
        JGkwargs['gateway_parameters'] = {**_JG_DEF['gateway_parameters'],
                                          **JGkwargs['gateway_parameters']}
        _JG_DEF.pop('gateway_parameters')

    JGkwargs = {**_JG_DEF, **JGkwargs}

    # remove any args that should be set in the launch_gateway call
    gateway_parameters = JGkwargs['gateway_parameters']
    for arg, arg_val in _SKIPPED_ARGS.items():
        if arg in JGkwargs:
            JGkwargs.pop(arg)
            print(arg_val)
        if arg in gateway_parameters:
            gateway_parameters.pop(arg)
            print(arg_val)
    return JGkwargs

def _addConJGParams(port, proc, auth_token, JGkwargs):
    """
    Adds JavaGateway connection parameters.

    Arguments:
    ----------
    port : int
        the port to launch the Java Gateway on
    proc: subprocess.Popen object
        the subprocess.Popen object for the Java process that
        the JavaGateway shall connect to
    auth_token : bool
        If True, the server will require clients to provide
        an authentication token when connecting.
    JGkwargs : dict
        dictionary with a user set arguments

    Returns:
    ----------
    JGkwargs: dict
        JavaGateway arguments dictionary
    """
    # insert the params returned from the launch_gateway call
    gateway_parameters = JGkwargs['gateway_parameters']
    gateway_parameters = {**gateway_parameters, **{'port':port, 'auth_token':auth_token}}
    JGkwargs.update({'java_process':proc, 'gateway_parameters':GatewayParameters(**gateway_parameters)})
    return JGkwargs

class JPSGateway:
    """
    Wrapper class of the py4j JavaGateway class for managing
    Python-Java communication.

    Attributes:
    ----------
    resName : str
        name of the Java resource
    jarPath : str
        absolute path to the main jar file of the java resource
    gateway : JavaGateway objects
        the gateway object handling Python-Java communication
    _isStarted : bool, private
        flag indicating if the gateway was launched
    _gatewayUserParams : dict, private
        dictionary storing user provided JavaGateway parameters
    _launchGatewayUserParams : dict, private
        dictionary storing user provided launch_gateway parameters
    """
    def __init__(self, resName=None, jarPath=None, **JGkwargs):
        """
        JPSGateway constructor class

        Arguments:
        ----------
        resName : str
            name of the Java resource
        jarPath : str
            absolute path to the main jar file of the java resource
        JGkwargs : dict, optional
            dictionary storing user provided JavaGateway parameters

            note that the JGkwargs related to the 'gateway_parameters'
            argument should be passed as a dictionary which is then
            automatically converted into the 'GatewayParameters' object
            As an example, the following arguments:

            JGkwargs = {'gateway_parameters':{'auto_convert':True}}

            will be automatically converted to:

            JGkwargs = {'gateway_parameters':
                                GatewayParameters(auto_convert=True)}}

            note that the 'java_process' and 'auth_token' arguments
            will be skipped if present and they are automatically
            set by the launch_gateway method

            note that the 'port' argument will skipped if present
            as it can only be passed to the launch_gateway call

            note that setting the JavaGateway 'eager_load' and the
            launch_gateway 'enable_auth' arguments to True at the
            same time does not work. The arguments are mutually
            exclusive
        """
        self.resName = resName
        if jarPath is None:
            self.jarPath = resReg.getResMainFilePath(resName)
        else:
            self.jarPath = jarPath
        self.gateway = None
        self._gatewayUserParams = _processJGkwargs(**JGkwargs)
        self._launchGatewayUserParams = None
        self._isStarted = False

    def launchGateway(self, **LGkwargs):
        """
        Wrapper method for the py4j launch_gateway method which
        launches the Gateway in a new Java process and creates a
        default JavaGateway to connect to it.

        Arguments:
        ----------
        LGkwargs : dict, optional
            a dictionary containing the py4j launch_gateway method
            arguments

            note that the 'jarpath' and 'return_proc' arguments cannot
            be changed and will be skipped if provided
        """

        if not self._isStarted:
            LGkwargs = _processLGkwargs(self.__class__.__name__, **LGkwargs)
            self._launchGatewayUserParams = LGkwargs

            # this launches the java process
            _ret = launch_gateway(jarpath=self.jarPath, **LGkwargs)
            if LGkwargs['enable_auth']:
                _port, _auth_token, proc = _ret
            else:
                _port, proc, _auth_token = _ret + (None, )

            self._gatewayUserParams = _addConJGParams(_port, proc, _auth_token, self._gatewayUserParams)
            # this creates the JavaGateway object connected to the launched java process above
            self.gateway = JavaGateway(**self._gatewayUserParams)
            self._isStarted = True
        else:
            print("Info: JavaGateway already started.")

    def shutdown(self):
        """
        Wrapper method for the py4j shutdown method
        to stop the JavaGateway client.
        """
        if self._isStarted:
            self.gateway.shutdown()

    def createModuleView(self):
        """
        Wrapper method for the py4j new_jvm_view method.
        Creates a new JVM view with its own imports.

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
        Wrapper method for the py4j java_import method.
        Imports a class / package into the specified JVM view

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