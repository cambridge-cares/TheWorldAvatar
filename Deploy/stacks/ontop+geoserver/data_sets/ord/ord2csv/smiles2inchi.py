import requests
from typing import Optional, Tuple
import json


def pubchem_api(smiles: Optional[str] = None) -> Tuple[str,str]:
    result = {}
    # https://pubchem.ncbi.nlm.nih.gov/rest/pug/<input specification>/<operation specification>/[<output specification>][?<operation_options>]
    pubchem_domain = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/'
    # <input specification>:
    input_domain = 'compound/' # <domain> = substance | compound | assay | gene | protein | pathway | taxonomy | cell | <other inputs>
    input_namespace = 'smiles/' # compound domain <namespace> = cid | name | smiles | inchi | sdf | inchikey | formula | <structure search> | <xref> | listkey | <fast search>
    if smiles is not None:
        input_identifier = smiles 
    else:
        input_identifier='c1ccccc1CCC(O)C' # 'c1ccccc1CCC(O)C' is picked as a default smiles  
    # /<operation specification>
    operation_property = '/property/' # compound domain <operation specification> = record | <compound property> | synonyms | sids | cids | aids | assaysummary | classification | <xrefs> | description | conformers
    operation_cid = '/cids/'
    property_tag = 'InChI/' # <compound property> = property / [comma-separated list of property tags]
    # /<output specification>
    output= 'JSON' # <output specification> = XML | ASNT | ASNB | JSON | JSONP [ ?callback=<callback name> ] | SDF | CSV | PNG | TXT
    InChI_link = pubchem_domain+input_domain+input_namespace+input_identifier+operation_property+property_tag+output
    CID_link = pubchem_domain+input_domain+input_namespace+input_identifier+operation_cid+output
    
    inchi = requests.get(InChI_link)
    file = json.loads(inchi.text)
    return (file['PropertyTable']['Properties'][0]['CID'],
    file['PropertyTable']['Properties'][0]['InChI'])


if __name__ == "__main__":

    for smile in ['C1CCCO1', 'Clc1ccc(S(=O)(=O)F)cc1', 'CN1CCCN2C1=NCCC2', 'c1ccccc1CCC(O)C']:
        CID, InChI = pubchem_api(smile)
        print(CID, InChI)