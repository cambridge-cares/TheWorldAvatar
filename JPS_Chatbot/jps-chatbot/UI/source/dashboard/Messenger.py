import json
import time

import yagmail


class Messenger:

    # seconds passed since epoch

    def __init__(self):
        self.yag = yagmail.SMTP('marie.maintenance.message', 'mariemaintenancepassword')
        self.default_receivers = ['xz378@cam.ac.uk', 'jimmyzhou.ntu@gmail.com','danieln@cmclinnovations.com']
        self.main_developer = ['xz378@cam.ac.uk', 'danieln@cmclinnovations.com']

    def send_test_message(self, receiver):
        self.yag.send(to=receiver, subject='TEST MESSAGE FROM MESSENGER', contents='THIS IS A TEST MESSAGE')

    def starting_a_test(self):
        self.yag.send(to=self.main_developer, subject="Marie is starting a self-test", contents="")

    def finished_a_test(self, failed_questions):
        if len(failed_questions) == 0:
            content = 'All testing questions passed the test'
        else:
            try:
                content = 'The following questions failed the test <br/>' +  json.dumps(failed_questions)
            except:
                content = 'The following questions failed the thest <br/>' + str(failed_questions)
        self.yag.send(to=self.main_developer, subject="Marie fininshed the self-test", contents= content)


    def send_error_message(self, error_message):
        subject = 'Error message from Marie'
        seconds = time.time()
        local_time = time.ctime(seconds)
        content = 'An error occurred during the self-test of Marie <br/> Time: %s <br/> Error message : %s' % (
            local_time, error_message)
        self.yag.send(to=self.default_receivers, subject=subject, contents=content)

    def send_failed_message(self, question):
        seconds = time.time()
        local_time = time.ctime(seconds)
        subject = 'A failed attempt at %s '% local_time
        content = 'A question is not answered <br/> %s ' % question
        self.yag.send(to=self.main_developer, subject=subject, contents=content)
