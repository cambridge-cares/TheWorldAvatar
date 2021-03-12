import unittest
from py4jps.resources import jpsBaseLib
from py4j.java_gateway import GatewayParameters
from os import path
class TestWrapper(unittest.TestCase):

    def JGLGkwargsTest(self):
        # ===============================================================================================
        # sub test 1 - checks most important params, except enable_auth = True, as it does not work with the
        #              eager_load = True option
        # JGkwargs
        eager_load = True   # py4j default is False, tested if can be changed to True
        auto_field = True   # py4j default is False, tested if can be changed to True
        auth_token = True   # py4j default is None, shouldnt be set by a user, tested if removed correctly
        auto_convert = True # py4j default is False, tested if can be changed to True
        auto_close = False  # py4j default is True, tested if can be changed to False
        # LGkwargs
        jarpath = 'dummyPath' # tested if correctly removed from the LGkwargs
        port = 49154 # tested if correctly set by the launch_gateway call

        # instantiate jsp gatway object using the JGkwargs values defined above
        jps = jpsBaseLib(**{'eager_load': eager_load,
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
        self.assertEqual('java_process' in jps._gatewayUserParams, False)
        self.assertEqual('auth_token' in jps._gatewayUserParams['gateway_parameters'], False)
        self.assertEqual(jps._gatewayUserParams['eager_load'], eager_load)
        self.assertEqual(jps._gatewayUserParams['auto_convert'], auto_convert)
        self.assertEqual(jps._gatewayUserParams['gateway_parameters']['auto_convert'], auto_convert)
        self.assertEqual(jps._gatewayUserParams['gateway_parameters']['auto_field'], auto_field)
        self.assertEqual(jps._gatewayUserParams['gateway_parameters']['auto_close'], auto_close)
        self.assertEqual('port' in jps._gatewayUserParams['gateway_parameters'], False)

        # launch the gateway with the LGkwargs values defined above
        jps.launchGateway(**{'port':port, 'jarpath':jarpath})

        # check adding connection params to JGkwargs
        self.assertEqual('java_process' in jps._gatewayUserParams, True)
        self.assertEqual(jps.jarPath != jarpath, True) # check if dummy jarpath removed
        # check for correct gateway_parameters type conversion
        self.assertEqual(isinstance(jps._gatewayUserParams['gateway_parameters'], GatewayParameters), True)
        JGparams = jps._gatewayUserParams['gateway_parameters']
        # check paratemers values
        self.assertEqual(JGparams.port, port)
        self.assertEqual(JGparams.auto_convert, auto_convert)
        self.assertEqual(JGparams.auto_field, auto_field)
        self.assertEqual(JGparams.auto_close, auto_close)
        self.assertEqual(JGparams.eager_load, eager_load)

        # now check if the internal py4j gateway params match the user settings
        py4jGWparams = jps.gateway.gateway_parameters
        self.assertEqual(py4jGWparams.port, port)
        self.assertEqual(py4jGWparams.auto_convert, auto_convert)
        self.assertEqual(py4jGWparams.auto_field, auto_field)
        self.assertEqual(py4jGWparams.auto_close, auto_close)
        self.assertEqual(py4jGWparams.eager_load, eager_load)

        jps.shutdown()
        # ===============================================================================================
        # sub test 2  - checks the enable_auth = True option
        # LGkwargs
        enable_auth = True  # py4j default is False, tested if can be changed to True
        jps = jpsBaseLib()
        jps.launchGateway(**{'enable_auth': enable_auth})

        # check user provided JGkwargs pre-processing
        JGparams = jps._gatewayUserParams['gateway_parameters']
        self.assertEqual(JGparams.auth_token is not None, enable_auth)
        # now check if the internal py4j gateway params match the user settings
        py4jGWparams = jps.gateway.gateway_parameters
        self.assertEqual(py4jGWparams.auth_token is not None, enable_auth)

    def fileReadingTest(self):
        jps = jpsBaseLib()
        jps.launchGateway()
        tw_jpsBaseLib_view = jps.createModuleView()
        jps.importPackages(tw_jpsBaseLib_view,'uk.ac.cam.cares.jps.base.util.*')

        FileUtil = tw_jpsBaseLib_view.FileUtil
        file_str = FileUtil.readFileLocally(path.abspath(path.join(path.dirname(__file__),'test_file1.txt')))
        self.assertEqual("test file1", file_str)
        jps.shutdown()

    def remoteKGqueryTest(self):
        jps = jpsBaseLib()
        jps.launchGateway()
        tw_jpsBaseLib_view = jps.createModuleView()
        jps.importPackages(tw_jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

        KGRouter = tw_jpsBaseLib_view.KGRouter
        KGClient = KGRouter.getKnowledgeBaseClient('http://kb/ontokin', True, False)
        response = KGClient.executeQuery(("PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#> \
                                        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>	SELECT ?mechanismIRI \
                                        WHERE	{ ?mechanismIRI rdf:type ontokin:ReactionMechanism .} LIMIT 10"))
        jps.shutdown()

    def javaPythonObjConversionTest(self):
        jps = jpsBaseLib()
        jps.launchGateway()
        tw_jpsBaseLib_view = jps.createModuleView()
        jps.importPackages(tw_jpsBaseLib_view,'uk.ac.cam.cares.jps.base.util.*')

        # craete tje Java File object instance
        javaFolder = tw_jpsBaseLib_view.java.io.File(path.abspath(path.join(path.dirname(__file__))))
        # create a FileUtil instance in order to access its non static methods
        FileUtil = tw_jpsBaseLib_view.FileUtil()
        # call the getDirectoryFiles method
        # note that passed [".txt"] Python list is automatically converted to the Java List<String> instance
        fileListArray = FileUtil.getDirectoryFiles(javaFolder, [".txt"])
        # note that the returned value type is ArrayList<File>, so one needs to know a bit of Java
        # to access its values
        retFilesList = []
        for i in range(fileListArray.size()):
            retFilesList.append(fileListArray.get(i).toString())

        self.assertEqual(retFilesList, [path.join(path.abspath(path.join(path.dirname(__file__))), 'test_file1.txt')])

def runTests():
    suite = unittest.TestSuite()
    suite.addTest(TestWrapper('JGLGkwargsTest'))
    suite.addTest(TestWrapper('fileReadingTest'))
    suite.addTest(TestWrapper('remoteKGqueryTest'))
    suite.addTest(TestWrapper('javaPythonObjConversionTest'))
    runner = unittest.TextTestRunner()
    runner.run(suite)

if __name__ == '__main__':
    runTests()