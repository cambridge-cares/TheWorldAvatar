import json
import time
from pprint import pprint

if __name__ == '__main__':
    from CoordinateAgent import CoordinateAgent
    from dashboard.Messenger import Messenger
else:
    from .CoordinateAgent import CoordinateAgent
    from .dashboard.Messenger import Messenger

class FullTest:

    def __init__(self):
        self.failed_questions = []

    def start(self):
        ca = CoordinateAgent()
        msg = Messenger()
        with open('test_result') as f:
            q_a_pairs = json.loads(f.read())

        with open('test_questions') as f:
            questions = f.readlines()

        # q = 'show me the molecular model of CH2=CHCHO\n'
        #
        # rst = ca.run(q)
        # print(rst)
        msg.starting_a_test()
        for q in questions:
            rst = ca.run(q)
            if type(rst) == type([]):
                rst = rst[0]
            true_rst = q_a_pairs[q]
            if rst != true_rst:
                print('We have a problem')
                # msg.send_error_message('Test on example question %s failed' % q)
                self.failed_questions.append(q)
                # msg.send_error_message(q)
            time.sleep(20)

        msg.finished_a_test(self.failed_questions)

# url
