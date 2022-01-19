import json
import pkg_resources

PREFIX_FILE = pkg_resources.resource_filename(__name__,'prefixes.json')

with open(PREFIX_FILE, 'r') as pfile:
    PREFIXES = json.load(pfile)