from chemaboxwriters.kgoperations.querytemplates import get_species_iri
from chemutils.obabelutils import obConvert
from compchemparser.helpers.utils import get_xyz_from_parsed_json
from chemaboxwriters.common.randomidgenerator import get_random_id
import json


def compchem_ocjson_abox_from_string(data, calc_id=""):
    data = json.loads(data)
    xyz = get_xyz_from_parsed_json(data)
    inchi = obConvert(xyz, 'xyz','inchi')
    spec_IRI = get_species_iri(inchi)
    if not calc_id:
        calc_id = get_random_id()

    data['spec_IRI'] = spec_IRI
    data['job_IRI'] = calc_id

    return json.dumps(data)