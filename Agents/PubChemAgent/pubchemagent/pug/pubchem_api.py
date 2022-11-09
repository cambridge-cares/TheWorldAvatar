import traceback
import requests
from typing import Optional, Tuple
import json
from unit_parse import parser

class pug_api():
    def __init__(self):
        self.input_namespace_dict = {'InChI' : 'inchi/', 'SMILES' : 'smiles/'}
        self.output_suffix_dict = {'InChI' : '?inchi=', 'SMILES' : '?smiles='}

    # HTTP request link builder and executer
    def pug_request(self, key: str, value: str) -> Tuple[str,str]:

        #*************A Potential Function to Clean Up The Request String **************#        
        # replace the # in the SMILES string with its encoing to correctly send the SMILES string via HTTP request
        if key == 'SMILES': value=value.replace('#','%23')
        # replace the + in the InChI string with its encoing to correctly send the InChI string via HTTP request
        if key == 'InChI' : value=value.replace('+','%2b')         

        #*************PUG API Definitions****************#

        pubchem_domain = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/'
        input_domain = 'compound/' 
        input_namespace = self.input_namespace_dict.get(key)
        input_identifier=  value  
        output= 'JSON/' 
        suffix = self.output_suffix_dict.get(key)
        link = pubchem_domain+input_domain+input_namespace+output+suffix+input_identifier

        data = requests.get(link)
        file = json.loads(data.text)

        return file

    def pug_request_prop_3d(self, cid: dict) -> dict:
        pubchem_domain = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/'
        input_domain = 'compound/cid/' 
        input_identifier = str(cid.get('cid')) + '/'
        output= 'JSON?record_type=3d'
        link = pubchem_domain+input_domain+input_identifier+output

        data = requests.get(link)
        file = json.loads(data.text)

        return file
    
    def pug_request_exp_prop(self, cid: dict) -> dict:

        thermo_list={'Boiling Point', 'Density', 'Flash Point', 'Melting Point', 'Solubility', 
        'Vapor Density', 'Vapor Pressure', 'Viscosity', 'Heat of Combustion', 'Heat of Vaporization', 
        'Henry\'s Law Constant', 'Refractive Index', 'Enthalpy of Sublimation', 'Surface Tension'}
        
        pubchem_domain_full = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/'
        input_domain = 'compound/'
        input_identifier = str(cid.get('cid')) + '/'
      
        # request experimental properties
        exp_prop = {}
        output= 'JSON?heading=Experimental+Properties'
        link = pubchem_domain_full+input_domain+input_identifier+output
        data={}
        data = requests.get(link)
        data = json.loads(data.text)
        if 'Record' in data:
                Reference=data.get('Record').get('Reference')
                if 'Section' in data.get('Record').get('Section')[0].get('Section')[0]:
                    data = data.get('Record').get('Section')[0].get('Section')[0].get('Section')
                    for prop in data:
                        i=1
                        prop_list = prop.get('Information')
                        key = prop.get('TOCHeading')
                        exp_prop[key]={}
                        for item in prop_list:
                            if 'StringWithMarkup' in item.get('Value'):
                                value = item.get('Value').get('StringWithMarkup')
                                for i_value in value:
                                    exp_prop[key][i]={}
                                    ep_string = i_value.get('String')
                                    if key in thermo_list:
                                        ep_string = pug_api.thermo_parser(ep_string)
                            else:
                                value = item.get('Value')
                                exp_prop[key][i]={}
                                ep_string = value.get('Number')
                            reference_num = item.get('ReferenceNumber')
                            for r in Reference:
                                if r.get('ReferenceNumber') == reference_num:
                                    reference=r.get('URL')                                      
                            exp_prop[key][i]['value'] = ep_string
                            exp_prop[key][i]['reference'] = reference
                            i=i+1
        else:
            print("\'Experimental Properties\' do not exist in record")

        return exp_prop

    def thermo_parser(str) -> dict:
        thermo_props = {}
        unit = ''
        q_type = ''
        try:
            result = parser(str)
            if hasattr(result, 'm'):
                quantity = result.m
                if hasattr(result, 'u'):
                    unit = result.u
            else:
                result = parser(str)[0]
                a = 0
                for i in result:
                    if a == 0:
                        if hasattr(result[a], 'u'):
                            quantity = result[a].m
                            unit = result[a].u
                            q_type = result[a]._dimensionality._d
                        else:
                            quantity = result[a] 
                        a = a+1
                    else:
                        ref_quantity = result[a].m
                        ref_unit = result[a].u
                        qref_type = result[a]._dimensionality._d 
                        thermo_props ['ref_value'] = ref_quantity
                        thermo_props ['ref_unit'] = ref_unit  
                        thermo_props ['ref_type'] = qref_type             
            thermo_props ['value'] = quantity
            thermo_props ['unit'] = unit
            thermo_props ['type'] = q_type
        except Exception as exc:
            print(exc)
            print(str)
            thermo_props = str
            return thermo_props
        
        return thermo_props

    def pug_request_sh_prop(self, cid: dict) -> dict:
        
        pubchem_domain_full = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/'
        input_domain = 'compound/'
        input_identifier = str(cid.get('cid')) + '/'
      
        # request experimental properties
        sh_prop = {}
        output= 'JSON?heading=Safety+and+Hazard+Properties'
        link = pubchem_domain_full+input_domain+input_identifier+output
        data={}
        data = requests.get(link)
        data = json.loads(data.text)
        if 'Record' in data:
                Reference=data.get('Record').get('Reference')
                if 'Section' in data.get('Record').get('Section')[0].get('Section')[0]:
                    data = data.get('Record').get('Section')[0].get('Section')[0].get('Section')
                    for prop in data:
                        i=1
                        if 'Information' in prop:
                            prop_list = prop.get('Information')
                            key = prop.get('TOCHeading')
                            sh_prop[key]={}
                            for item in prop_list:
                                if 'StringWithMarkup' in item.get('Value'):
                                    value = item.get('Value').get('StringWithMarkup')
                                    for i_value in value:
                                        sh_prop[key][i]={}
                                        ep_string = i_value.get('String')
                                else:
                                    value = item.get('Value')
                                    sh_prop[key][i]={}
                                    ep_string = value.get('Number')
                                reference_num = item.get('ReferenceNumber')
                                for r in Reference:
                                    if r.get('ReferenceNumber') == reference_num:
                                        reference=r.get('URL')                                      
                                sh_prop[key][i]['value'] = ep_string
                                sh_prop[key][i]['reference'] = reference
                                i=i+1
        else:
            print("\'Safety and Hazard Properties\' do not exist in record")

        return sh_prop

    def pug_request_uses(self, cid: dict) -> dict:
        
        pubchem_domain_full = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/'
        input_domain = 'compound/'
        input_identifier = str(cid.get('cid')) + '/'
        
        list=['Industry+Uses','Consumer+Uses','Household+Products']
        
        # request uses
        uses = {}
        for i_key in list:
            output= 'JSON?heading=' + i_key
            link = pubchem_domain_full+input_domain+input_identifier+output
            data={}
            data = requests.get(link)
            data = json.loads(data.text)
            if 'Record' in data:
                Reference=data.get('Record').get('Reference')
                if 'Section' in data.get('Record').get('Section')[0].get('Section')[0]:
                    data = data.get('Record').get('Section')[0].get('Section')[0].get('Section')[0].get('Information')[0]
                    value = data.get('Value').get('StringWithMarkup')
                    reference_num = data.get('ReferenceNumber')
                    for r in Reference:
                        if r.get('ReferenceNumber') == reference_num:
                            reference=r.get('URL')
                    key = i_key.replace('+',' ')
                    uses[key]={}
                    i=1
                    for i_value in value:
                        uses[key][i]={}
                        use_string = i_value.get('String')
                        uses[key][i]['value'] = use_string
                        uses[key][i]['reference'] = reference
                        i=i+1
                else:
                    print('\'' + i_key.replace('+',' ') + '\'' + " does not exist in record")    
            else:
                print('\'' + i_key.replace('+',' ') + '\'' + " does not exist in record")

        return uses

    # Method for retrieving PubChem properties
    def get_props(self, data : dict) -> dict:
        comp_props = {}
        identifiers = {}
        props_list = data.get('PC_Compounds')[0].get('props')
        count_list = data.get('PC_Compounds')[0].get('count')
        i=0; j=0
        for item in props_list:
            datatype = item.get('urn').get('datatype')
            provenance = item.get('urn').get('source')
            if item.get('urn').get('name'): 
                key = str(item.get('urn').get('name'))+' '+str(item.get('urn').get('label'))
                key_type = str(item.get('urn').get('name'))
            else:
                key = str(item.get('urn').get('label'))
                key_type = ''
            value = [item.get('value')[key] for key in item.get('value').keys()][0]
            if datatype == 1 and ('Mass' not in key) and ('Weight' not in key):
                key = key.replace(key_type + ' ', '')
                identifiers[i]={}
                identifiers[i]['key'] = key.replace(' ', '')
                identifiers[i]['value'] = value
                identifiers[i]['type'] = key_type
                identifiers[i]['provenance'] = provenance
                i = i+1
            else:
                if datatype == 7 and key_type != '':
                    key = key_type
                comp_props[j]={}  
                comp_props[j]['key'] = key.replace(' ', '')
                comp_props[j]['value'] = value
                comp_props[j]['provenance'] = provenance
                j = j+1
        for item in count_list.keys():
                comp_props[j]={}
                value = count_list.get(item) 
                comp_props[j]['key']=item.replace('_', ' ').title().replace(' ' , '') + 'Count'
                comp_props[j]['value']= value
                comp_props[j]['provenance'] = 'PubChem'
                j = j+1

        return comp_props, identifiers

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

    def get_charge(self, data : dict) -> dict[str, int]:
        charge={}
        key = 'charge'
        value = data.get('PC_Compounds')[0].get(key)
        charge[key] = value
        return charge

    def get_count(data : dict):
        pass  

# Testing the module itself
if __name__ == "__main__":
    pug_access = pug_api()

    for inchi in ['InChI=1S/C6H6/c1-2-4-6-5-3-1/h1-6H',
                    "InChI=1S/C23H34BrN3O3/c1-22(2,27-21(29)13-24)16-7-10-23(3,11-8-16)26-14-17(28)15-30-20-6-4-5-19-18(20)9-12-25-19/h4-6,9,12,16-17,25-26,28H,7-8,10-11,13-15H2,1-3H3,(H,27,29)",
                    'InChI=1/C10H10/c1-2-3-7-10-8-5-4-6-9-10/h4-6,8-9H,2H2,1H3', 
                    'InChI=1/C10H10/c1-2-8-5-6-9-4-3-7(1)10(8)9/h1-10H']:
        data = pug_access.pug_request('InChI', inchi)
        props, identifiers = pug_access.get_props(data)
        cid = pug_access.get_cid(data)
        charge = pug_access.get_charge(data)
        data_3d = pug_access.pug_request_prop_3d(cid)
        props_3d, v = pug_access.get_props(data_3d)
        exp_props = pug_access.pug_request_exp_prop(cid)
        uses = pug_access.pug_request_uses(cid)
        sh_props = pug_access.pug_request_sh_prop(cid)
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