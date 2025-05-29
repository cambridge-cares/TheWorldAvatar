import os


def retrieve_default_settings():
    global NAMESPACE, DATABASE

    NAMESPACE = os.getenv("NAMESPACE")
    if NAMESPACE is None:
        NAMESPACE = 'kb'

    DATABASE = os.getenv('DATABASE')
    if DATABASE is None:
        DATABASE = 'postgres'


# run when module is imported
retrieve_default_settings()
