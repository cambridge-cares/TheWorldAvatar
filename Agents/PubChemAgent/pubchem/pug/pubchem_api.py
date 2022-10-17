import requests
from typing import Optional, Tuple
import json


class pug_api():
    def __init__(self):
        self.input_namespace_dict = {'InChI' : 'inchi/', 'SMILES' : 'smiles/'}
        self.output_suffix_dict = {'InChI' : '?inchi=', 'SMILES' : '?smiles='}

    # HTTP request link builder and executer
    def pug_request(self, key: str, value: str) -> Tuple[str,str]:

        
        # replace the # in the SMILES string with its encoing to correctly send the SMILES string via HTTP request
        if key == 'SMILES': value=value.replace('#','%23')         

        #*************PUG API Definitions****************#

        # https://pubchem.ncbi.nlm.nih.gov/rest/pug/<input specification>/<operation specification>/[<output specification>][?<operation_options>]
        pubchem_domain = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/'
        # <input specification>:
        input_domain = 'compound/' # <domain> = substance | compound | assay | gene | protein | pathway | taxonomy | cell | <other inputs>
        input_namespace = self.input_namespace_dict.get(key) # compound domain <namespace> = cid | name | smiles | inchi | sdf | inchikey | formula | <structure search> | <xref> | listkey | <fast search>
        input_identifier=  value # 'c1ccccc1CCC(O)C' is picked as a default InChI  
        # /<operation specification>
        operation_property = 'property/' # compound domain <operation specification> = record | <compound property> | synonyms | sids | cids | aids | assaysummary | classification | <xrefs> | description | conformers
        operation_property = ''
        property_tag = 'MolecularFormula,InChIKey,InChI,CanonicalSmiles,ExactMass,MolecularWeight,IsotopeAtomCount,IupacName,CovalentUnitCount,Tpsa/' # <compound property> = property / [comma-separated list of property tags]
        property_tag = ''
        # /<output specification>
        output= 'JSON/' # <output specification> = XML | ASNT | ASNB | JSON | JSONP [ ?callback=<callback name> ] | SDF | CSV | PNG | TXT
        suffix = self.output_suffix_dict.get(key)
        link = pubchem_domain+input_domain+input_namespace+operation_property+property_tag+output+suffix+input_identifier

        data = requests.get(link)
        file = json.loads(data.text)

        return file

    # Method for retrieving PubChem properties
    def get_props(self, data : dict) -> dict:
        props_list = data.get('PC_Compounds')[0].get('props')
        props = {}
        for item in props_list:
            if item.get('urn').get('name'): 
                key = str(item.get('urn').get('name'))+' '+str(item.get('urn').get('label'))
            else:
                key = str(item.get('urn').get('label'))
    
            value = [item.get('value')[key] for key in item.get('value').keys()][0]
            props[key] = value
        return props

    # Method for retrieving PubChem CID 
    def get_cid(self, data : dict) -> dict[str, int]:
        id = data.get('PC_Compounds')[0].get('id')
        return id.get('id')

    # Method for retrieving atom IDs
    def get_atoms(self, data : dict) -> dict[str , list] :
        atom_ids = data.get('PC_Compounds')[0].get('atoms')
        return atom_ids

    # Method for retrieving atom bonds
    def get_bonds(data : dict) -> dict[str, list]:
        atom_bonds = data.get('PC_Compounds')[0].get('bonds')
        return atom_bonds 


    #************Future Development***************#
    # Methods for atom coordinations, charges, and counts
    def get_coords(data : dict):
        pass

    def get_charge(data : dict):
        pass

    def get_count(data : dict):
        pass  

if __name__ == "__main__":
    pug_access = pug_api()

    for inchi in ['InChI=1S/C6H6/c1-2-4-6-5-3-1/h1-6H', 
                  'InChI=1/C10H10/c1-2-3-7-10-8-5-4-6-9-10/h4-6,8-9H,2H2,1H3', 
                  'InChI=1/C10H10/c1-2-8-5-6-9-4-3-7(1)10(8)9/h1-10H']:
        data = pug_access.pug_request('InChI', inchi)
        cid = pug_access.get_cid(data)
        props = pug_access.get_props(data)
        atom_id = pug_access.get_atoms(data)

        print(cid['cid'], props['Preferred IUPAC Name'], '\n', atom_id)

    for smiles in ['C1=CC=CC=C1', 
                  'CCC#CC1=CC=CC=C1', 
                  'C#CC1=CC(=C2C=CC3=C(C=C(C4=C3C2=C1C=C4)C#C)C#C)C#C']:
        data = pug_access.pug_request('SMILES', smiles)
        cid = pug_access.get_cid(data)
        props = pug_access.get_props(data)
        print(props)
        print(cid['cid'], props['Preferred IUPAC Name'])