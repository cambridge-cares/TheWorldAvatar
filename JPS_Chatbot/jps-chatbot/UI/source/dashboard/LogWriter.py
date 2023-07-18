import time


class LogWriter:

    def __init__(self):
        with open('../question-log.txt', 'w') as f:
            seconds = time.time()
            local_time = time.ctime(seconds)
            f.write('Starting log at %s' % local_time)
            f.close()

    def write_to_log(self, question, message):
        with open('../question-log.txt', 'a') as f:
            f.write('\n' + question + '\n--------------\n')
            f.write(message + '\n')
            f.close()
