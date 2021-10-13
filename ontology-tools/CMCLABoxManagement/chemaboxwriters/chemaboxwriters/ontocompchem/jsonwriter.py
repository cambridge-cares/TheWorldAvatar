from chemaboxwriters.kgoperations.querytemplates import get_species_iri
import chemutils.obabelutils.obconverter as obconverter
from compchemparser.helpers.utils import get_xyz_from_parsed_json
from chemaboxwriters.common.randomidgenerator import get_random_id
import json
import chemaboxwriters.common.commonvars as commonv
from chemaboxwriters.common import PREFIXES
from compchemparser.parsers.ccgaussian_parser import PROGRAM_NAME, \
                                                     PROGRAM_VERSION
comp_pref = PREFIXES["comp_pref"]

def oc_jsonwriter(data, random_id=""):
    data = json.loads(data)
    xyz = get_xyz_from_parsed_json(data)
    inchi = obconverter.obConvert(xyz, 'xyz','inchi')
    spec_IRI = get_species_iri(inchi)
    if not random_id:
        random_id = get_random_id()

    # at the moment we only support gaussian
    if 'Gaussian' not in data[PROGRAM_NAME]:
        pass
    else:
        jobType = 'G'+data[PROGRAM_VERSION][2:4]
    data[commonv.SPECIES_IRI] = spec_IRI
    data[commonv.ENTRY_IRI] = comp_pref+jobType+'_'+random_id
    data[commonv.ENTRY_UUID] = random_id

    return [json.dumps(data)]