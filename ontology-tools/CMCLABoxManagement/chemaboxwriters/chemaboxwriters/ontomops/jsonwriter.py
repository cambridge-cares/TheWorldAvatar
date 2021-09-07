import json
from chemaboxwriters.common.randomidgenerator import get_random_id
import chemaboxwriters.common.commonvars as commonv
from chemaboxwriters.ontomops.prefixes import omops_entry_prefix

def om_jsonwriter(data, calc_id=""):
    data = json.loads(data)

    if not calc_id:
        calc_id = get_random_id()

    data[commonv.ENTRY_UUID] = calc_id
    data[commonv.ENTRY_IRI] = omops_entry_prefix+calc_id

    return json.dumps(data)