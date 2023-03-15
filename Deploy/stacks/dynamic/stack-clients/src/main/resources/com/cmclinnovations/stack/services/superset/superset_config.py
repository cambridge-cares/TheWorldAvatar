import os


def get_string_from_file(file_path: str) -> str:
    '''Get string from a specified file'''
    with open(file_path, 'r') as file:
        return file.read()


WTF_CSRF_ENABLED = False

PUBLIC_ROLE_LIKE = os.environ.get('PUBLIC_ROLE_LIKE')

APP_NAME = os.environ.get('APP_NAME')

SECRET_KEY = get_string_from_file(os.environ.get('SUPERSET_SECRET_KEY_FILE'))
