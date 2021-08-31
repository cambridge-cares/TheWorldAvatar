import json
from chemaboxwriters.common.randomidgenerator import get_random_id
import chemaboxwriters.common.commonvars as commonv
from chemaboxwriters.ontospecies.prefixes import omops_entry_prefix

def compchem_osjson_abox_from_string(data, calc_id=""):
    data = json.loads(data)

    data_out = {}

    if not calc_id:
        calc_id = get_random_id()

    data_out[commonv.ENTRY_UUID] = calc_id
    data_out[commonv.ENTRY_IRI] = omops_entry_prefix+calc_id

    return json.dumps(data_out)