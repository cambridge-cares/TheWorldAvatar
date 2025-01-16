import time
import unittest
import sys
from Marie.CandidateSelection.CandidateSelector import CandidateSelector

sys.path.append("..")


class MyTestCase(unittest.TestCase):
    def test_predict(self):
        # TODO: do a timing
        my_candidate_selector = CandidateSelector()

        START_TIME = time.time()
        candidate_list = ['CID1', 'CID3', 'CID3', 'CID7_compound_complexity', 'CID31_tpsa']
        question = 'what is the tpsa of CID1'
        head_entity = 'CID1'

        rst = my_candidate_selector.predict(head_entity=head_entity, sentence=question, candidate_list=candidate_list)
        END_TIME = time.time()

        TIME_DELTA = END_TIME - START_TIME
        print(rst)
        print('========== TIME USED ===========')
        print(TIME_DELTA)
        # print(head_entity_embedding)

        # h_batch = tf.repeat([head_entity_embedding], repeats= [5], axis=0)
        # print(h_batch)

        # print(tf.repeat([head_entity], range(0, 1000)))
        # self.assertEqual(True, False)  # add assertion here


if __name__ == '__main__':
    unittest.main()
