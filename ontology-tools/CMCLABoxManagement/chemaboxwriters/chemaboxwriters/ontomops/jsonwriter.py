import json
from chemaboxwriters.common.randomidgenerator import get_random_id
import chemaboxwriters.common.commonvars as commonv
from chemaboxwriters.common import PREFIXES

omops_entry_prefix = PREFIXES["omops_entry_prefix"]

def om_jsonwriter(file_path, random_id=""):

    with open(file_path, 'r') as file_handle:
        data = json.load(file_handle)

    if not random_id:
        random_id = get_random_id()

    data[commonv.ENTRY_UUID] = random_id
    data[commonv.ENTRY_IRI] = omops_entry_prefix+random_id

    return [json.dumps(data)]