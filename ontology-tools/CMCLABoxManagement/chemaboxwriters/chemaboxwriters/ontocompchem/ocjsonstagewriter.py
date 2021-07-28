from chemaboxwriters.kgoperations.querytemplates import get_species_iri
from chemutils.obabelutils import obConvert
from compchemparser.helpers.utils import get_xyz_from_parsed_json
from chemaboxwriters.common.randomidgenerator import get_random_id
import json
import chemaboxwriters.common.commonvars as commonv
from  chemaboxwriters.ontocompchem.prefixes import comp_pref
from compchemparser.parsers.ccgaussian_parser import PROGRAM_NAME, \
                                                     PROGRAM_VERSION


def compchem_ocjson_abox_from_string(data, calc_id=""):
    data = json.loads(data)
    xyz = get_xyz_from_parsed_json(data)
    inchi = obConvert(xyz, 'xyz','inchi')
    spec_IRI = get_species_iri(inchi)
    if not calc_id:
        calc_id = get_random_id()

    # at the moment we only support gaussian
    if 'Gaussian' not in data[PROGRAM_NAME]:
        pass
    else:
        jobType = 'G'+data[PROGRAM_VERSION][2:4]
    data[commonv.SPECIES_IRI] = spec_IRI
    data[commonv.ENTRY_IRI] = comp_pref+jobType+'_'+calc_id
    data[commonv.ENTRY_UUID] = calc_id

    return json.dumps(data)