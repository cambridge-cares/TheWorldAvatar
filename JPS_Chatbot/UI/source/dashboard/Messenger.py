import time

import yagmail


class Messenger:

    # seconds passed since epoch

    def __init__(self):
        self.yag = yagmail.SMTP('marie.maintenance.message', 'somerandompasswordformarie')
        self.default_receivers = ['xz378@cam.ac.uk', 'jimmyzhou.ntu@gmail.com']

    def send_test_message(self, receiver):
        self.yag.send(to=receiver, subject='TEST MESSAGE FROM MESSENGER', contents='THIS IS A TEST MESSAGE')

    def send_error_message(self, error_message):
        subject = 'Error message from Marie'
        seconds = time.time()
        local_time = time.ctime(seconds)
        content = 'An error occurred during the self-test of Marie <br/> Time: %s <br/> Error message : %s' % (
            local_time, error_message)
        self.yag.send(to=self.default_receivers, subject=subject, contents=content)
