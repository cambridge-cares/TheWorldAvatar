import requests
import logging
from typing import Tuple
import json
from unit_parse import parser, logger
from rdkit import Chem
from periodictable import elements
from datetime import date
import re

logging.getLogger('urllib3').setLevel(logging.CRITICAL)
logging.getLogger('pint').setLevel(logging.CRITICAL)

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
        if key == 'InChI' : value=value.replace('+','%2b').replace(';','%3b').replace('(','%28').replace(')','%29').replace('.','%2e').replace(',','%2c') 

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

    def pug_request_prop_3d(self, cid) -> dict:
        pubchem_domain = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/'
        input_domain = 'compound/cid/' 
        input_identifier = str(cid) + '/'
        output= 'JSON?record_type=3d'
        link = pubchem_domain+input_domain+input_identifier+output

        data = requests.get(link)
        file = json.loads(data.text)

        return file
    
        
    def check_preferred(self, cid) -> str:
        pubchem_domain_full = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/'
        input_domain = 'compound/'
        input_identifier = str(cid) + '/'

        output= 'JSON?heading=Names+and+Identifiers'
        link = pubchem_domain_full+input_domain+input_identifier+output
        data={}
        data = requests.get(link)
        data = json.loads(data.text)
        if 'Information' in data.get('Record').get('Section')[0].get('Section')[0]:
            for item in data.get('Record').get('Section')[0].get('Section')[0].get('Information'):
                if item.get('Name') == 'See Also':
                    a = item.get('Value').get('StringWithMarkup')[0]
                    if 'preferred' in a.get('String'):
                        cid = item.get('Value').get('StringWithMarkup')[0].get('Markup')[0].get('URL').partition('compound/')[-1]

        return cid
    
    def pug_request_exp_prop(self, cid) -> dict:

        num_prop_list={'Acid Value', 'Autoignition Temperature', 'Caco2 Permeability', 'Collision Cross Section', 
        'Dissociation Constants', 'LogP', 'LogS', 'Hydrophobicity', 'Isoelectric Point', 'Ionization Potential'}

        thermo_list={'Boiling Point', 'Density', 'Flash Point', 'Melting Point', 'Solubility', 
        'Vapor Density', 'Vapor Pressure', 'Viscosity', 'Heat of Combustion', 'Heat of Vaporization', 
        'Henry\'s Law Constant', 'Enthalpy of Sublimation', 'Surface Tension', 'Optical Rotation'}

        class_list={'Chemical Classes'}
        
        pubchem_domain_full = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/'
        input_domain = 'compound/'
        input_identifier = str(cid) + '/'
      
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
                    i=1
                    for prop in data:
                        prop_list = prop.get('Information')
                        key = prop.get('TOCHeading')
                        if key in num_prop_list or key in thermo_list or key in class_list:
                            for item in prop_list:
                                exp_prop[i]={}
                                if 'StringWithMarkup' in item.get('Value'):
                                    value = item.get('Value').get('StringWithMarkup')
                                    for i_value in value:
                                        ep_string = i_value.get('String')
                                        ep_unit=''
                                        description = ep_string
                                        ep_string = pug_api.string_parser(ep_string)
                                elif 'Number' in item.get('Value'):
                                    value = item.get('Value')
                                    ep_string = value.get('Number')
                                    if value.get('Unit'):
                                        ep_unit = value.get('Unit')
                                    else:
                                        ep_unit=''
                                    description = str(ep_string[0]) + ' ' + ep_unit
                                    ep_string = pug_api.string_parser(description)
                                reference_num = item.get('ReferenceNumber')
                                for r in Reference:
                                    if item.get('Reference'):
                                        reference='DOI:' + item.get('Reference')[0].partition('DOI:')[2]
                                    if r.get('ReferenceNumber') == reference_num:
                                        reference=r.get('URL')   
                                exp_prop[i]['key'] = key.replace(' ', '').replace('/','').replace('\'','')                                       
                                exp_prop[i]['value'] = ep_string
                                exp_prop[i]['description'] = description
                                exp_prop[i]['reference'] = reference
                                exp_prop[i]['dateofaccess'] = date.today()
                                if key in class_list: 
                                    exp_prop[i]['type']='classification'
                                    exp_prop[i]['description']=' '
                                elif key in thermo_list:
                                    exp_prop[i]['type']='thermo_prop'
                                else:
                                    exp_prop[i]['type']='num_prop'
                                i=i+1
        #else:
        #    print("\'Experimental Properties\' do not exist in record")

        return exp_prop

    def string_parser(str) -> dict:
        thermo_props = {}
        unit = ''
        ref_quantity = ''
        ref_unit = ''
        str=pug_api.preprocess(str)
        try:
            logger.setLevel(logging.CRITICAL)
            result = parser(str)
            if hasattr(result, 'm'):
                result = result.to_base_units()
                quantity = result.m
                if hasattr(result, 'u'):
                    unit = result.u
            else:
                result = parser(str)[0]
                a = 0
                for i in result:
                    if a == 0:
                        if hasattr(result[a], 'u'):
                            result[a] = result[a].to_base_units()
                            quantity = result[a].m
                            unit = result[a].u
                        else:
                            quantity = result[a] 
                        a = a+1
                    else:
                        result[a] = result[a].to_base_units()
                        ref_quantity = result[a].m
                        ref_unit = result[a].u            
            thermo_props ['value'] = quantity
            thermo_props ['unit'] = unit
            thermo_props ['ref_value'] = ref_quantity
            thermo_props ['ref_unit'] = ref_unit  
        except Exception as exc:
            print(exc)
            print(str)
            thermo_props = str
            return thermo_props
        
        return thermo_props

    def preprocess(str) -> str:
        str=re.sub("\([0-9]{1,10}.[0-9]{1,10}\)","",str)
        str=re.sub("\([0-9]{1,10}\)","",str)
        str=re.sub("± [0-9]{1,10}.[0-9]{1,10}", "",str)
        str=re.sub("± [0-9]{1,10}", "",str)
        str=re.sub("±[0-9]{1,10}", "",str)
        str=re.sub("X10-", "e-",str)
        str=re.sub("MHz", "megaHz",str)
        str=re.sub("(USCG, 1999)","", str)
        str=re.sub("(NTP, 1992)","", str)
        str=str.partition("[M+")[0]
        str=str.partition("[M-")[0]
        if bool(data_found := re.findall("[-.0-9]{1,6}[- ]{1,3}[-.0-9]{1,6}", str)):
            data_found = data_found[0]
            reduced_range = re.findall("[-]?[.0-9]{1,6}[^0-9-,/; ]{0,10}", data_found)[0]
            str=str.replace(data_found, reduced_range)
        return str

    def pug_request_sh_prop(self, cid) -> dict:
        
        pubchem_domain_full = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/'
        input_domain = 'compound/'
        input_identifier = str(cid) + '/'
      
        # request experimental properties
        sh_prop = {}
        output= 'JSON?heading=Safety+and+Hazard+Properties'
        link = pubchem_domain_full+input_domain+input_identifier+output
        data={}
        data = requests.get(link)
        data = json.loads(data.text)
        if 'Record' in data:
                i=1
                Reference=data.get('Record').get('Reference')
                if 'Section' in data.get('Record').get('Section')[0].get('Section')[0]:
                    data = data.get('Record').get('Section')[0].get('Section')[0].get('Section')
                    for prop in data:
                        if 'Information' in prop:
                            prop_list = prop.get('Information')
                            key = prop.get('TOCHeading')
                            for item in prop_list:
                                if 'StringWithMarkup' in item.get('Value'):
                                    value = item.get('Value').get('StringWithMarkup')
                                    for i_value in value:
                                        ep_string = i_value.get('String')
                                else:
                                    value = item.get('Value')
                                    ep_string = value.get('Number')
                                reference_num = item.get('ReferenceNumber')
                                for r in Reference:
                                    if r.get('ReferenceNumber') == reference_num:
                                        reference=r.get('URL')      
                                sh_prop[i]={} 
                                sh_prop[i]['key'] = key.replace(' ', '').replace('/','')                               
                                sh_prop[i]['value'] = ep_string
                                sh_prop[i]['reference'] = reference
                                i=i+1
        #else:
        #    print("\'Safety and Hazard Properties\' do not exist in record")

        return sh_prop

    # https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/2244/JSON?heading=Hazards+Identification
    # classification

    def pug_request_ghs_classification(self, cid) -> dict:
        
        pubchem_domain_full = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/'
        input_domain = 'compound/'
        input_identifier = str(cid) + '/'
      
        # request experimental properties
        ghs = {}
        output= 'JSON?heading=Hazards+Identification'
        link = pubchem_domain_full+input_domain+input_identifier+output
        data={}
        data = requests.get(link)
        data = json.loads(data.text)
        if 'Record' in data:
                i=1
                Reference=data.get('Record').get('Reference')
                if 'Section' in data.get('Record').get('Section')[0].get('Section')[0]:
                    data = data.get('Record').get('Section')[0].get('Section')[0].get('Section')
                    for prop in data:
                        if prop.get('TOCHeading') == 'GHS Classification':
                            prop_list = prop.get('Information')
                            for item in prop_list:
                                if item.get('Name') == 'GHS Hazard Statements':
                                    key = item.get('Name')
                                    value = item.get('Value').get('StringWithMarkup')
                                    for i_value in value:
                                        ep_string = i_value.get('String')
                                        reference_num = item.get('ReferenceNumber')
                                        for r in Reference:
                                            if r.get('ReferenceNumber') == reference_num:
                                                reference=r.get('URL') 
                                                break     
                                        ghs[i]={} 
                                        ghs[i]['key'] = key.replace(' ', '').replace('/','')  
                                        value = ep_string.partition(' (')[0].partition(':')[0] 
                                        description = ep_string.partition(': ')[2]                            
                                        ghs[i]['value'] = value
                                        ghs[i]['description'] = description
                                        ghs[i]['reference'] = reference
                                        ghs[i]['type'] = 'classification'
                                        i=i+1
                                    break

        return ghs

    def pug_request_uses(self, cid: dict) -> dict:
        
        pubchem_domain_full = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/'
        input_domain = 'compound/'
        input_identifier = str(cid) + '/'
        
        list=['Industry+Uses','Consumer+Uses','Household+Products']
        
        # request uses
        uses = {}
        i=0
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
                    for i_value in value:
                        if 'Information on' not in i_value.get('String'):
                            uses[i]={}
                            uses[i]['key']='Use'
                            use_string = i_value.get('String')
                            uses[i]['value'] = use_string.replace('• ','')
                            uses[i]['provenance'] = reference
                            uses[i]['type']='use'
                            i=i+1
                #else:
                #    print('\'' + i_key.replace('+',' ') + '\'' + " does not exist in record")    
            #else:
                #print('\'' + i_key.replace('+',' ') + '\'' + " does not exist in record")

        return uses

    def pug_request_spectra(self, cid) -> dict:
        
        pubchem_domain_full = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/'
        input_domain = 'compound/'
        input_identifier = str(cid) + '/'
      
        # request experimental properties
        spectra = {}
        spectra_list = {'1D NMR Spectra', '2D NMR Spectra', 'Mass Spectrometry'}
        output= 'JSON?heading=Spectral+Information'
        link = pubchem_domain_full+input_domain+input_identifier+output
        data={}
        data = requests.get(link)
        data = json.loads(data.text)
        if 'Record' in data:
                i=0
                inew = 1
                Reference=data.get('Record').get('Reference')
                if 'Section' in data.get('Record').get('Section')[0]:
                    data = data.get('Record').get('Section')[0].get('Section')
                    for prop in data:
                        if prop.get('TOCHeading') in spectra_list:
                            spectra_type = prop.get('TOCHeading')
                            prop_list = prop.get('Section')
                            for item in prop_list:
                                frequency = ''
                                solvent = ''
                                peaks = ''
                                ionization_mode = ''
                                instrument_type = ''
                                prop_list_2 = item.get('Information')
                                for item2 in prop_list_2:
                                    if inew != i:
                                        spectra[inew]={}
                                        spectra[inew]['type'] = ''
                                        spectra[inew]['key'] = ''
                                        spectra[inew]['peaks'] = ''
                                        spectra[inew]['frequency'] = ''
                                        spectra[inew]['solvent'] = ''
                                        spectra[inew]['ionization_mode'] = ''
                                        spectra[inew]['instrument_type'] = ''
                                        i = inew
                                    if item2.get('Name') and item2.get('Name') == 'Frequency':
                                        frequency = item2.get('Value').get('StringWithMarkup')[0].get('String')
                                        frequency = pug_api.string_parser(frequency)
                                        spectra[i]['frequency']=frequency
                                    if item2.get('Name') and item2.get('Name') == 'Solvent':
                                        solvent = item2.get('Value').get('StringWithMarkup')[0].get('String')
                                        spectra[i]['solvent']=solvent.replace('Water','H2O')
                                    if item2.get('Name') and item2.get('Name') == 'Ionization Mode':
                                        ionization_mode = item2.get('Value').get('StringWithMarkup')[0].get('String')
                                        spectra[i]['ionization_mode']=ionization_mode.lower()
                                    if item2.get('Name') and item2.get('Name') == 'Instrument Type':
                                        instrument_type = item2.get('Value').get('StringWithMarkup')[0].get('String')
                                        spectra[i]['instrument_type']=instrument_type.partition('(')[0]
                                    if item2.get('Name') and ("Shift" in item2.get('Name') or "Top 5 Peaks" in item2.get('Name')):
                                        key = item.get('TOCHeading')
                                        value = item2.get('Value').get('StringWithMarkup')
                                        reference_num = item2.get('ReferenceNumber')
                                        for r in Reference:
                                            if r.get('ReferenceNumber') == reference_num:
                                                reference=r.get('URL') 
                                                break  
                                        x1 = []  
                                        x2 = []
                                        intensity = []
                                        for i_value in value:
                                            peaks = i_value.get('String')    
                                            spectra[i]['key'] = key.replace(' ', '').replace('/','').replace('-','')  
                                            spectra[i]['peaks'] = {}                  
                                            if spectra_type == '1D NMR Spectra':
                                                peaks = peaks.split(', ')
                                                for j in peaks:
                                                    j=j.split(':')
                                                    x1.append(j[0])
                                                    if len(j) == 2:
                                                        intensity.append(j[1])
                                                    else:
                                                        intensity.append('')
                                                spectra[i]['peaks']['x1'] = x1
                                                spectra[i]['peaks']['intensity'] = intensity
                                            elif spectra_type == '2D NMR Spectra':
                                                peaks = peaks.split(', ')
                                                for j in peaks:
                                                    j=j.split(':')
                                                    x1.append(j[0])
                                                    x2.append(j[1])
                                                    if len(j) == 3:
                                                        intensity.append(j[2])
                                                    else:
                                                        intensity.append('')
                                                spectra[i]['peaks']['x1'] = x1
                                                spectra[i]['peaks']['x2'] = x2
                                                spectra[i]['peaks']['intensity'] = intensity
                                            elif spectra_type == 'Mass Spectrometry':
                                                peaks = peaks.split(' ')
                                                x1.append(peaks[0])
                                                if len(peaks) == 2:
                                                    intensity.append(peaks[1])
                                                else:
                                                    intensity.append('')
                                                spectra[i]['peaks']['x1'] = x1
                                                spectra[i]['peaks']['intensity'] = intensity
                                            spectra[i]['reference'] = reference
                                            spectra[i]['type'] = spectra_type.replace(' ', '')
                                            inew=i+1
                    if 'type' in spectra[inew] and spectra[inew]['type'] == '':
                        spectra.popitem()
                                
        return spectra

    def pug_request_element(self, el_num: int) -> dict:
        
        pubchem_domain_full = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/'
        input_domain = 'element/'
        input_identifier = str(el_num) + '/'
        output= 'JSON'

        el_prop_list={'AtomicWeight', 'AtomicNumber', 'ElectronConfiguration',
        'AtomicRadius','OxidationStates','GroundLevel','IonizationEnergy',
        'Electronegativity','ElectronAffinity', 'ElementClassification',
        'ElementPeriodNumber', 'ElementGroupNumber', 'Density',
        'MeltingPoint', 'BoilingPoint', 'IsotopesCount'}
        
        # request uses
        el_props = {}
        i=0
        link = pubchem_domain_full+input_domain+input_identifier+output
        data={}
        data = requests.get(link)
        data = json.loads(data.text)
        if 'Record' in data:
            Reference=data.get('Record').get('Reference')
            if 'Section' in data.get('Record').get('Section')[0]:
                identifiers = data.get('Record').get('Section')[0].get('Section')
                i=1
                for item in identifiers:
                    if 'Information' in item:
                        id_list = item.get('Information')
                        key = item.get('TOCHeading')
                        for item in id_list:
                            el_props[i]={}
                            value = item.get('Value').get('StringWithMarkup')
                            for i_value in value:
                                ep_string = i_value.get('String')
                            reference_num = item.get('ReferenceNumber')
                            for r in Reference:
                                if r.get('ReferenceNumber') == reference_num:
                                    reference=r.get('URL') 
                            el_props[i]['key'] = key.replace(' ', '').replace('/','')                                     
                            el_props[i]['value'] = ep_string
                            el_props[i]['reference'] = reference
                            el_props[i]['type']='identifier'
                            i=i+1
                            break
            if 'Section' in data.get('Record').get('Section')[1]:
                properties = data.get('Record').get('Section')[1].get('Section')
                for item in properties:
                    if 'Information' in item:
                        prop_list = item.get('Information')
                        key = item.get('TOCHeading').replace(' ', '').replace('/','') 
                        if key in el_prop_list:
                            for item in prop_list:
                                el_props[i]={}
                                if 'StringWithMarkup' in item.get('Value'):
                                    value = item.get('Value').get('StringWithMarkup')
                                    for i_value in value:
                                        ep_string = i_value.get('String')
                                        ep_unit = ''
                                        description = ep_string
                                        ep_string = pug_api.string_parser(ep_string)
                                else:
                                    value = item.get('Value')
                                    ep_string = value.get('Number')
                                    ep_unit = value.get('Unit')
                                    description = str(ep_string[0]) + ' ' + ep_unit
                                    ep_string = pug_api.string_parser(description)
                                reference_num = item.get('ReferenceNumber')
                                for r in Reference:
                                    if item.get('Reference'):
                                        reference='DOI:' + item.get('Reference')[0].partition('DOI:')[2]
                                    if r.get('ReferenceNumber') == reference_num:
                                        reference=r.get('URL')    
                                el_props[i]['key'] = key                                    
                                el_props[i]['value'] = ep_string
                                el_props[i]['description'] = description
                                el_props[i]['reference'] = reference
                                el_props[i]['dateofaccess'] = date.today()
                                if key in {'ElectronConfiguration','OxidationStates','GroundLevel'}:
                                    el_props[i]['type']='string_prop'
                                elif key == 'ElementClassification': 
                                    el_props[i]['type']='classification'
                                    el_props[i]['description']=' '
                                elif key in {'Density', 'MeltingPoint', 'BoilingPoint'}:
                                    el_props[i]['type']='thermo_prop'
                                else:
                                    el_props[i]['type']='num_prop'
                                i=i+1

        return el_props

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
            description = [item.get('value')[key] for key in item.get('value').keys()][0]
            if datatype == 1 and ('Mass' not in key) and ('Weight' not in key):
                key = key.replace(key_type + ' ', '')
                identifiers[i]={}
                identifiers[i]['key'] = key.replace(' ', '').replace('/','')
                identifiers[i]['value'] = description
                identifiers[i]['type'] = key_type
                identifiers[i]['reference'] = provenance
                identifiers[i]['type'] = 'identifier'
                i = i+1
            else:
                if datatype == 7 and key_type != '':
                    key = key_type
                comp_props[j]={}  
                comp_props[j]['key'] = key.replace(' ', '').replace('/','').replace('-AA','')
                if key=='SubStructure Keys Fingerprint':
                    comp_props[j]['value']={}
                    comp_props[j]['value']['value'] = description
                    comp_props[j]['value']['unit'] = ''
                else:
                    value = pug_api.string_parser(str(description))
                    comp_props[j]['value'] = value
                comp_props[j]['reference'] = provenance
                comp_props[j]['type'] = 'num_prop'
                comp_props[j]['description'] = description
                comp_props[j]['dateofaccess'] = date.today()
                j = j+1
        for item in count_list.keys():
                comp_props[j]={}
                description = count_list.get(item) 
                value = pug_api.string_parser(str(description))
                comp_props[j]['key']=item.replace('_', ' ').title().replace(' ' , '') + 'Count'
                comp_props[j]['value']= value
                comp_props[j]['reference'] = 'https://pubchem.ncbi.nlm.nih.gov'
                comp_props[j]['type'] = 'num_prop'
                comp_props[j]['description'] = description
                comp_props[j]['dateofaccess'] = date.today()
                j = j+1
        
        # get charge 
        charge = data.get('PC_Compounds')[0].get('charge')
        comp_props[j]={}
        value = pug_api.string_parser(str(charge))
        comp_props[j]['key']='Charge'
        comp_props[j]['value']= value
        comp_props[j]['reference'] = 'https://pubchem.ncbi.nlm.nih.gov'
        comp_props[j]['type'] = 'num_prop'
        comp_props[j]['description'] = charge
        comp_props[j]['dateofaccess'] = date.today()
        j=j+1

        # get cid
        cid = data.get('PC_Compounds')[0].get('id').get('id').get('cid')
        comp_props[j]={}
        comp_props[j]['key']='CID'
        comp_props[j]['value']= cid
        comp_props[j]['reference'] = 'https://pubchem.ncbi.nlm.nih.gov'
        comp_props[j]['type'] = 'identifier'

        return cid, comp_props, identifiers

    # Method for retrieving atom IDs
    def get_structure(self, g_dim, data : dict) -> dict[str , list] :
        geometry = {}
        bonds = {}
        atom_coords = data.get('PC_Compounds')[0].get('coords')[0].get('conformers')[0]
        x=atom_coords.get('x')
        y=atom_coords.get('y')
        z=atom_coords.get('z')
        elem = data.get('PC_Compounds')[0].get('atoms').get('element')

        i=0
        for item in x:
            for el in elements:
                if elem[i]==el.number:
                    elem[i]=el.symbol
            geometry[i] = {}
            geometry[i]['id'] = i+1
            geometry[i]['element'] = elem[i]
            geometry[i]['x'] = x[i]
            geometry[i]['y'] = y[i]
            if g_dim == '2d':
                geometry[i]['z'] = 0
            else:
                geometry[i]['z'] = z[i]

            i=i+1
        
        atom_bonds = data.get('PC_Compounds')[0].get('bonds')
        id1=atom_bonds.get('aid1')
        id2=atom_bonds.get('aid2')
        order=atom_bonds.get('order')
        
        i=0
        for item in id1:
            bonds[i] = {}
            bonds[i]['id1'] = id1[i]
            bonds[i]['id2'] = id2[i]
            bonds[i]['order'] = order[i]

            i=i+1

        return geometry, bonds



# Testing the module itself
if __name__ == "__main__":
    pug_access = pug_api()

    #for el in range(1,118):
    #    data = pug_access.pug_request_element(el)

    for inchi in ['InChI=1S/C2H6O/c1-2-3/h3H,2H2,1H3',
                    'InChI=1S/C6H6/c1-2-4-6-5-3-1/h1-6H',
                    "InChI=1S/C23H34BrN3O3/c1-22(2,27-21(29)13-24)16-7-10-23(3,11-8-16)26-14-17(28)15-30-20-6-4-5-19-18(20)9-12-25-19/h4-6,9,12,16-17,25-26,28H,7-8,10-11,13-15H2,1-3H3,(H,27,29)",
                    'InChI=1/C10H10/c1-2-3-7-10-8-5-4-6-9-10/h4-6,8-9H,2H2,1H3', 
                    'InChI=1/C10H10/c1-2-8-5-6-9-4-3-7(1)10(8)9/h1-10H']:
        data = pug_access.pug_request('InChI', inchi)
        cid, comp_props, identifiers = pug_access.get_props(data)
        spectra = pug_access.pug_request_spectra(cid)
        data_3d = pug_access.pug_request_prop_3d(cid)
        geometry, bonds = pug_access.get_structure('3d', data_3d)
        exp_props = pug_access.pug_request_exp_prop(cid)
        uses = pug_access.pug_request_uses(cid)
        aa=1;    