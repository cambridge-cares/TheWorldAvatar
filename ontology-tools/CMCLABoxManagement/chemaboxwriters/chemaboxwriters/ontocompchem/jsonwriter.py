from chemaboxwriters.kgoperations.querytemplates import get_species_iri
import chemutils.obabelutils.obconverter as obconverter
from compchemparser.helpers.utils import get_xyz_from_parsed_json
from chemaboxwriters.common.utilsfunc import get_random_id
import json
import chemaboxwriters.common.globals as globals
from chemaboxwriters.common import PREFIXES
from compchemparser.parsers.ccgaussian_parser import PROGRAM_NAME, \
                                                     PROGRAM_VERSION
comp_pref = PREFIXES["comp_pref"]

def oc_jsonwriter(file_path, random_id="", spec_IRI="", fs_uploads=None, ts_uploads=None):
    with open(file_path, 'r') as file_handle:
        data = json.load(file_handle)

    xyz = get_xyz_from_parsed_json(data)
    inchi = obconverter.obConvert(xyz, 'xyz','inchi')
    if not spec_IRI:
        spec_IRI = get_species_iri(inchi)
    if not random_id:
        random_id = get_random_id()

    # at the moment we only support gaussian
    jobType=''
    if 'Gaussian' in data[PROGRAM_NAME]:
        if PROGRAM_VERSION in data:
            jobType = 'G'+data[PROGRAM_VERSION][2:4]
        else:
            jobType = 'Gxx'
    data[globals.SPECIES_IRI] = spec_IRI
    data[globals.ENTRY_IRI] = comp_pref+jobType+'_'+random_id
    data[globals.ENTRY_UUID] = random_id

    return [json.dumps(data)]