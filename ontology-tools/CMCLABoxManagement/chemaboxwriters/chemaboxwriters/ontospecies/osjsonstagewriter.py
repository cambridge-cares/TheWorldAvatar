from chemutils.obabelutils import obConvert, obGetMolBonds
from chemutils.xyzutils import xyzToAtomsPositions
from compchemparser.helpers.utils import get_xyz_from_parsed_json
from chemaboxwriters.common.randomidgenerator import get_random_id
from compchemparser.parsers.ccgaussian_parser import ATOM_MASSES, FORMAL_CHARGE, ATOM_TYPES, ATOM_MASSES
from compchemparser.helpers.elements_data import get_molwt_from_atom_types
import pubchempy as pcp
import json
import re

charge_re = re.compile('\/p([+-]\d+)')
cas_re = re.compile('\d{2,7}-\d\d-\d')

def compchem_osjson_abox_from_string(data, calc_id=""):
    data = json.loads(data)
    xyz = get_xyz_from_parsed_json(data)
    inchi = obConvert(xyz, 'xyz', 'inchi')
    smiles = obConvert(xyz, 'xyz', 'smi')
    data['inchi'] = inchi
    data['smiles'] = smiles

    if ATOM_MASSES not in data.keys():
        data['molwt'] = get_molwt_from_atom_types(data[ATOM_TYPES])
    else:
        data['molwt'] = sum(data[ATOM_MASSES])

    if FORMAL_CHARGE not in data.keys():
        data[FORMAL_CHARGE] = 0
        formal_charge_re = charge_re.search(inchi)
        if formal_charge_re:
            data[FORMAL_CHARGE] = int(formal_charge_re.groups()[0])

    data['bond_string'] = obGetMolBonds(xyz)
    # add atoms positions!
    data['atoms_can_pos'] = xyzToAtomsPositions(xyz)

    data['alt_label'] = None
    data['casid'] = None

    pubchem_compound = pcp.get_compounds(data['inchi'], 'inchi')
    if pubchem_compound:
        if pubchem_compound[0].synonyms:
            alt_labels = pubchem_compound[0].synonyms[0]
            casid = get_substructure_cas(pubchem_compound[0].synonyms)
            if casid: casid= casid[0]

    data['alt_label'] = alt_labels
    data['casid'] = casid


    if not calc_id:
        calc_id = get_random_id()

    data['job_IRI'] = calc_id

    return json.dumps(data)

def get_substructure_cas(synonyms):
    cas_rns = []
    for syn in synonyms:
        match = re.match('(\d{2,7}-\d\d-\d)', syn)
        if match:
            cas_rns.append(match.group(1))
    return cas_rns