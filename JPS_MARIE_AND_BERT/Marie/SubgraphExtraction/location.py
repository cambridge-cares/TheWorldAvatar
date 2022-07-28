import os


def get_root_dir():
    return os.path.abspath('../..')


def get_parent_dir():
    return os.path.abspath('..')


PARENT_PATH = get_parent_dir()
ROOT_PATH = get_root_dir()
DATA_PATH = os.path.join(PARENT_PATH, 'DATA')

print(DATA_PATH)
