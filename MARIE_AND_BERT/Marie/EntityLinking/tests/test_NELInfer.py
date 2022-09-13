import unittest
import sys

sys.path.append("..")

from Marie.EntityLinking.Inference import NELInfer


class MyTestCase(unittest.TestCase):
    def test_initialization(self):

        my_entity_linker = NELInfer('base500.yaml')
        self.assertEqual(True, True)  # add assertion here


if __name__ == '__main__':
    unittest.main()
