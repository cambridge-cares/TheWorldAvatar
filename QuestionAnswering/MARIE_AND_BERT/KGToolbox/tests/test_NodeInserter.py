import sys
import unittest

sys.path.append("..")

from KGToolbox.CrossGraph.NodeInserter import NodeInserter


class MyTestCase(unittest.TestCase):
    def test_something(self):
        my_node_inserter = NodeInserter()
        self.assertEqual(my_node_inserter.insert_node(), 1)  # add assertion here


if __name__ == '__main__':
    unittest.main()
