import unittest
from py4jps.resources import jpsBaseLib
from os import path
class TestWrapper(unittest.TestCase):

    def fileReading(self):
        jps = jpsBaseLib()
        jps.launchGateway()
        tw_jpsBaseLib_view = jps.createModuleView()
        jps.importPackages(tw_jpsBaseLib_view,'uk.ac.cam.cares.jps.base.util.*')

        FileUtil = tw_jpsBaseLib_view.FileUtil
        file_str = FileUtil.readFileLocally(path.abspath(path.join(path.dirname(__file__),'test_file1.txt')))
        self.assertEqual("test file1", file_str)
        jps.shutdown()

    def remoteKGquery(self):
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

def runTests():
    suite = unittest.TestSuite()
    suite.addTest(TestWrapper('fileReading'))
    suite.addTest(TestWrapper('remoteKGquery'))
    runner = unittest.TextTestRunner()
    runner.run(suite)

if __name__ == '__main__':
    runTests()