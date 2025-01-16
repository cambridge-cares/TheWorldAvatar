import unittest
import sys
from Marie.EntityLinking.Inference import NELInfer

sys.path.append("..")


class MyTestCase(unittest.TestCase):

    def import_entity_linking(self):
        my_nel_infer = NELInfer()
        raw_data = 'what is the molecular mass of benzene'
        rst = my_nel_infer.infer([{"text": raw_data}])
        print(rst)

        self.assertEqual(True, True)


if __name__ == '__main__':
    unittest.main()
