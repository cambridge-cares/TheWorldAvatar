import sys


def logging(func):
    import logging
    # log = logging.getLogger()
    # log.setLevel(logging.INFO)

    def wrapper(*args, **kwargs):
        logging.basicConfig(stream=sys.stdout, level=logging.INFO)

        logging.info('Running logger from function {}'.format(func.__name__))
        return func(*args, **kwargs)

    return wrapper


class FakeClass:
    def __init__(self):
        pass

    @logging
    def dosomething(self, input):
        print('Doing something')

    def call_dosomething(self, input):
        self.dosomething(input)


fc = FakeClass()
fc.call_dosomething('Hello')
