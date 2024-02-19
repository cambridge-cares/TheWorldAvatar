import time
import unittest
import sys
from Marie.Util.CommonTools.NLPTools import NLPTools

sys.path.append("..")


class MyTestCase(unittest.TestCase):
    def test_nlp_tool_init_speed(self):
        START_TIME = time.time()
        nlp = NLPTools()
        print(time.time() - START_TIME)



if __name__ == '__main__':
    unittest.main()
