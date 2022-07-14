import configparser

from config.config import PROPERTIES_FILE


def readProperties():
    cp = configparser.ConfigParser()
    with open(PROPERTIES_FILE) as stream:
        cp.read_string("[top]\n" + stream.read())
    readProperties = {pair[0]: pair[1] for pair in cp.items('top')}
    return readProperties
