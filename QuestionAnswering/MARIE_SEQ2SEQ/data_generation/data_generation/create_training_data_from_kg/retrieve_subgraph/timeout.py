import errno
import os
import signal
import functools


class TimeoutError(Exception):
    pass


def timeout(seconds: int, default_value, error_message=os.strerror(errno.ETIME)):
    def decorator(func):
        def _handle_timeout(signum, frame):
            raise TimeoutError(error_message)

        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            signal.signal(signal.SIGALRM, _handle_timeout)
            signal.alarm(seconds)
            try:
                result = func(*args, **kwargs)
            except TimeoutError:
                result = default_value
            finally:
                signal.alarm(0)
            return result

        return wrapper

    return decorator
