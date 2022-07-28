import os
from pprint import pprint

from Utils.universal_locations import RESULT_DIR

print(RESULT_DIR)
result_files = [f for f in os.listdir(RESULT_DIR) if 'Testing' in f]

pprint(result_files)


