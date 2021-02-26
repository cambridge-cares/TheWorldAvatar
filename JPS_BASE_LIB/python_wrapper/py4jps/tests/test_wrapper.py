import unittest
from py4jps.main import Gateway
from os import path



# placeholder class for any unit tests
class TestWrapper(unittest.TestCase):

    def test_1(self):
        gateway = Gateway()
        gateway.start()
        module1_view = gateway.createModuleView()
        gateway.importJava(module1_view,'uk.ac.cam.cares.jps.base.util.*')

        FileUtil = module1_view.FileUtil

        file_str = FileUtil.readFileLocally(path.join(path.dirname(__file__),'test_file1.txt'))
        self.assertEqual("test file1", file_str)
        gateway.shutdown()

def runTests():
    suite = unittest.TestSuite()
    suite.addTest(TestWrapper('test_1'))
    runner = unittest.TextTestRunner()
    runner.run(suite)

if __name__ == '__main__':
    runTests()