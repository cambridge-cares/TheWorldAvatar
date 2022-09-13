from mimetypes import suffix_map
import requests
from typing import Optional, Tuple
import json


def pubchem_api(inchi: Optional[str] = None) -> Tuple[str,str]:
    result = {}
    # https://pubchem.ncbi.nlm.nih.gov/rest/pug/<input specification>/<operation specification>/[<output specification>][?<operation_options>]
    pubchem_domain = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/'
    # <input specification>:
    input_domain = 'compound/' # <domain> = substance | compound | assay | gene | protein | pathway | taxonomy | cell | <other inputs>
    input_namespace = 'inchi/' # compound domain <namespace> = cid | name | smiles | inchi | sdf | inchikey | formula | <structure search> | <xref> | listkey | <fast search>
    if inchi is not None:
        input_identifier = inchi 
    else:
        input_identifier='InChI=1/C' # 'c1ccccc1CCC(O)C' is picked as a default InChI  
    # /<operation specification>
    operation_property = '/property/' # compound domain <operation specification> = record | <compound property> | synonyms | sids | cids | aids | assaysummary | classification | <xrefs> | description | conformers
    operation_cid = '/cids/'
    property_tag = 'MolecularFormula,InChIKey,InChI,CanonicalSmiles,ExactMass,MolecularWeight,IsotopeAtomCount,IupacName,CovalentUnitCount,Tpsa/' # <compound property> = property / [comma-separated list of property tags]
    # /<output specification>
    output= 'JSON/' # <output specification> = XML | ASNT | ASNB | JSON | JSONP [ ?callback=<callback name> ] | SDF | CSV | PNG | TXT
    suffix = '?inchi='
    InChI_link = pubchem_domain+input_domain+input_namespace+operation_property+property_tag+output+suffix+input_identifier
    CID_link = pubchem_domain+input_domain+input_namespace+input_identifier+operation_cid+output
    
    data = requests.get(InChI_link)
    file = json.loads(data.text)
    return (file['PropertyTable']['Properties'][0])


if __name__ == "__main__":

    for inchi in ['InChI=1/C10H10/c1-2-6-10-8-4-3-7-9(10)5-1/h1-3,5-7H,4,8H2', 
                  'InChI=1/C10H10/c1-2-3-7-10-8-5-4-6-9-10/h4-6,8-9H,2H2,1H3', 
                  'InChI=1/C10H10/c1-2-8-5-6-9-4-3-7(1)10(8)9/h1-10H']:
        data = pubchem_api(inchi)
        print(inchi)
        print(data)