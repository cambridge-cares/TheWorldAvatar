import random
import uuid
import pytest
from twa.resources import JpsBaseLib
from py4j.java_gateway import GatewayParameters
from os import path
import json

def test_JGLGkwargs():
    # ===============================================================================================
    # sub test 1 - checks most important params, except enable_auth = True, as it does not work with the
    #              eager_load = True option
    # JGkwargs
    eager_load = True   # py4j default is False, tested if can be changed to True
    auto_field = True   # py4j default is False, tested if can be changed to True
    auth_token = True   # py4j default is None, shouldn't be set by a user, tested if removed correctly
    auto_convert = True # py4j default is False, tested if can be changed to True
    auto_close = False  # py4j default is True, tested if can be changed to False
    # LGkwargs
    jarpath = 'dummyPath' # tested if correctly removed from the LGkwargs
    port = 49154 # tested if correctly set by the launch_gateway call

    # instantiate jsp gatway object using the JGkwargs values defined above
    jpsGW = JpsBaseLib(**{'eager_load': eager_load,
                        'java_process': 1,
                        'auto_convert': auto_convert,
                        'gateway_parameters':{
                            'auto_field':auto_field, 'port':port,
                            'auth_token': auth_token,
                            'auto_convert': auto_convert,
                            'auto_close': auto_close,
                            'eager_load': eager_load}
                        })
    # check user provided JGkwargs pre-processing
    assert ('java_process' not in jpsGW._gatewayUserParams)
    assert ('auth_token' not in jpsGW._gatewayUserParams['gateway_parameters'])
    assert jpsGW._gatewayUserParams['eager_load'] == eager_load
    assert jpsGW._gatewayUserParams['auto_convert'] == auto_convert
    assert jpsGW._gatewayUserParams['gateway_parameters']['auto_convert'] == auto_convert
    assert jpsGW._gatewayUserParams['gateway_parameters']['auto_field'] == auto_field
    assert jpsGW._gatewayUserParams['gateway_parameters']['auto_close'] == auto_close
    assert ('port' not in jpsGW._gatewayUserParams['gateway_parameters'])

    # launch the gateway with the LGkwargs values defined above
    jpsGW.launchGateway(**{'port':port, 'jarpath':jarpath})

    # check adding connection params to JGkwargs
    assert ('java_process' in jpsGW._gatewayUserParams)
    assert jpsGW.jarPath != jarpath # check if dummy jarpath removed
    # check for correct gateway_parameters type conversion
    assert isinstance(jpsGW._gatewayUserParams['gateway_parameters'], GatewayParameters)
    JGparams = jpsGW._gatewayUserParams['gateway_parameters']
    # check paratemers values
    assert JGparams.port == port
    assert JGparams.auto_convert == auto_convert
    assert JGparams.auto_field == auto_field
    assert JGparams.auto_close == auto_close
    assert JGparams.eager_load == eager_load

    # now check if the internal py4j gateway params match the user settings
    py4jGWparams = jpsGW.gateway.gateway_parameters
    assert py4jGWparams.port == port
    assert py4jGWparams.auto_convert == auto_convert
    assert py4jGWparams.auto_field == auto_field
    assert py4jGWparams.auto_close == auto_close
    assert py4jGWparams.eager_load == eager_load

    jpsGW.shutdown()
    # ===============================================================================================
    # sub test 2  - checks the enable_auth = True option
    # LGkwargs
    enable_auth = True  # py4j default is False, tested if can be changed to True
    jpsGW = JpsBaseLib()
    jpsGW.launchGateway(**{'enable_auth': enable_auth})

    # check user provided JGkwargs pre-processing
    JGparams = jpsGW._gatewayUserParams['gateway_parameters']
    assert (JGparams.auth_token is not None) == enable_auth
    # now check if the internal py4j gateway params match the user settings
    py4jGWparams = jpsGW.gateway.gateway_parameters
    assert (py4jGWparams.auth_token is not None) == enable_auth

def test_fileReading():
    jpsGW = JpsBaseLib()
    jpsGW.launchGateway()
    jpsGW_view = jpsGW.createModuleView()
    jpsGW.importPackages(jpsGW_view,'uk.ac.cam.cares.jps.base.util.*')

    FileUtil = jpsGW_view.FileUtil
    file_str = FileUtil.readFileLocally(path.abspath(path.join(path.dirname(__file__),'test_file1.txt')))
    assert "test file1" == file_str
    jpsGW.shutdown()

def test_tripleStoreQuery(initialise_triple_store):
    endpoint = initialise_triple_store
    jpsGW = JpsBaseLib()
    jpsGW.launchGateway()
    jpsGW_view = jpsGW.createModuleView()
    jpsGW.importPackages(jpsGW_view,"uk.ac.cam.cares.jps.base.query.*")

    StoreClient = jpsGW_view.RemoteStoreClient(endpoint, endpoint)
    triples = []
    for i in range(random.randint(1,100)):
        triples.append(f"<http://{str(uuid.uuid4())}> <http://{str(uuid.uuid4())}> <http://{str(uuid.uuid4())}>.")
    triples.append(f"""<http://{str(uuid.uuid4())}> <http://{str(uuid.uuid4())}> "Infinity"^^<http://www.w3.org/2001/XMLSchema#double>.""")
    triples.append(f"""<http://{str(uuid.uuid4())}> <http://{str(uuid.uuid4())}> "-Infinity"^^<http://www.w3.org/2001/XMLSchema#double>.""")
    triples.append(f"""<http://{str(uuid.uuid4())}> <http://{str(uuid.uuid4())}> "NaN"^^<http://www.w3.org/2001/XMLSchema#double>.""")
    StoreClient.executeUpdate(f"INSERT {{ {' '.join(triples)} }} WHERE {{}}")
    response = StoreClient.executeQuery((f"ASK {{ {' '.join(triples)} }}"))
    response = json.loads(str(response))
    assert response[0]['ASK']
    jpsGW.shutdown()

def test_javaPythonObjConversion():
    jpsGW = JpsBaseLib()
    jpsGW.launchGateway()
    jpsGW_view = jpsGW.createModuleView()
    jpsGW.importPackages(jpsGW_view,'uk.ac.cam.cares.jps.base.util.*')

    # craete the Java File object instance
    javaFolder = jpsGW_view.java.io.File(path.abspath(path.join(path.dirname(__file__))))
    # create a FileUtil instance in order to access its non static methods
    FileUtil = jpsGW_view.FileUtil()
    # call the getDirectoryFiles method
    # note that passed [".txt"] Python list is automatically converted to the Java List<String> instance
    fileListArray = FileUtil.getDirectoryFiles(javaFolder, [".txt"])
    # note that the returned value type is ArrayList<File>, so one needs to know a bit of Java
    # to access its values
    retFilesList = []
    for i in range(fileListArray.size()):
        retFilesList.append(fileListArray.get(i).toString())

    assert retFilesList == [path.join(path.abspath(path.join(path.dirname(__file__))), 'test_file1.txt')]
