from generate_data.Q_and_query_template_from_species import *
from generate_data.Q_and_query_template_from_chemclass import *
from generate_data.Q_and_query_template_from_qualifier import *
from generate_data.Q_and_query_template_from_two_qualifier import *
from generate_data.template_utils import get_random_number
import random
import json
from datetime import datetime

delimiter = ''  # GPT best practice '\n\n###\n\n'
end_delimiter = '' # GPT best practice 'END'
timestamp = datetime.now().strftime("%Y-%m-%d_%H.%M.%S")

def create_query_template(filename = 'chatbot/fine_tuning/resources/test.json', N = 3):

    property_list = ['AtomChiralCount', 'AtomChiralDefCount', 'AtomChiralUndefCount', 'BondChiralCount', 'BondChiralDefCount',
                'BondChiralUndefCount', 'CanonicalizedCompound', 'Charge', 'CompoundComplexity', 'CovalentUnitCount',
                'ExactMass', 'HeavyAtomCount', 'HydrogenBondAcceptorCount', 'HydrogenBondDonorCount', 'IsotopeAtomCount', 
                'MolecularWeight', 'MonoIsotopicWeight', 'RotatableBondCount', 'SubStructureKeysFingerprint', 
                'TautomerCount', 'XLogP3', 'AutoignitionTemperature', 'Caco2Permeability', 'CollisionCrossSection',
                'Hydrophobicity', 'IonizationPotential', 'IsoelectricPoint', 'LogP', 'LogS', 'PolarSurfaceArea', 
                'BoilingPoint', 'Density', 'DissociationConstants', 'EnthalpyOfSublimation', 'FlashPoint', 'StandardEnthalpyOfFormation', 
                'HeatOfCombustion', 'HeatOfVaporization', 'HenrysLawConstant', 'MeltingPoint', 'OpticalRotation', 'Solubility',
                'SurfaceTension', 'VaporDensity', 'VaporPressure', 'Viscosity']
    
    identifier_list = ['ChebiID', 'CID', 'EmpiricalFormula', 'InChI', 'InChIKey', 'IUPACName', 'MolecularFormula', 'SMILES']


    json_objects = []
    with open('chatbot/fine_tuning/resources/species.txt') as f:
        species = [line.strip() for line in f.readlines()]
    with open('chatbot/fine_tuning/resources/chemical_classes.txt') as f:
        chemicalclasses = [line.strip() for line in f.readlines()]
    with open('chatbot/fine_tuning/resources/uses.txt') as f:
        uses = [line.strip() for line in f.readlines()]
    
    for i in range(0,N,1):

        s = random.sample(species, 1)[0]
        query_text, prompt = get_chemclass_from_species(s)
        data = write_json(prompt , query_text)
        json_objects.append(data)

        s = random.sample(species, 1)[0]
        query_text, prompt = get_use_from_species(s)
        data = write_json(prompt , query_text)
        json_objects.append(data)

        s = random.sample(species, 1)[0]
        query_text, prompt = get_use_and_chemclass_from_species(s)
        data = write_json(prompt , query_text)
        json_objects.append(data)

        s = random.sample(species, 1)[0]
        query_text, prompt = get_all_properties(s)
        data = write_json(prompt , query_text)
        json_objects.append(data)

        s = random.sample(species, 1)[0]
        query_text, prompt = get_all_identifiers(s)
        data = write_json(prompt , query_text)
        json_objects.append(data)

        c = random.sample(chemicalclasses, 1)[0]
        query_text, prompt = get_species_from_chemclass(c)
        data = write_json(prompt , query_text)
        json_objects.append(data)
    
        u = random.sample(uses, 1)[0]
        query_text, prompt = get_species_from_use(u)
        data = write_json(prompt , query_text)
        json_objects.append(data)

        c = random.sample(chemicalclasses, 1)[0]
        u = random.sample(uses, 1)[0]
        query_text, prompt = get_species_from_chclass_and_use(c,u)
        data = write_json(prompt , query_text)
        json_objects.append(data)

        # on one property

        s = random.sample(species, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt = get_property_from_species(p1,s)
        data = write_json(prompt , query_text)
        json_objects.append(data)

        s1 = random.sample(species, 1)[0]
        s2 = random.sample(species, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt = get_compare_properties(p1,s1,s2)
        data = write_json(prompt , query_text)
        json_objects.append(data)   

        c = random.sample(chemicalclasses, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt = get_property_from_chemclass(p1,c)
        data = write_json(prompt , query_text)
        json_objects.append(data)                     
   
        n = get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_property_L(p1, n)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        n = get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt =  get_species_from_property_H(p1, n)
        data = write_json(prompt, query_text)
        json_objects.append(data)
    
        min = get_random_number(20, 500, 1)[0]
        max = min + get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_property_O(p1, min, max)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        min = get_random_number(20, 500, 1)[0]
        max = min + get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_property_I(p1, min, max)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        n = get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt =  get_species_from_property_A(p1, n)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        n = get_random_number(20, 500, 1)[0]
        c = random.sample(chemicalclasses, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_property_L_and_chclass(p1, c, n)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        n = get_random_number(20, 500, 1)[0]
        c = random.sample(chemicalclasses, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt =  get_species_from_property_H_and_chclass(p1, c, n)
        data = write_json(prompt, query_text)
        json_objects.append(data)
    
        min = get_random_number(20, 500, 1)[0]
        max = min + get_random_number(20, 500, 1)[0]
        c = random.sample(chemicalclasses, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_property_O_and_chclass(p1, c, min, max)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        min = get_random_number(20, 500, 1)[0]
        max = min + get_random_number(20, 500, 1)[0]
        c = random.sample(chemicalclasses, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_property_I_and_chclass(p1, c, min, max)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        n = get_random_number(20, 500, 1)[0]
        c = random.sample(chemicalclasses, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt =  get_species_from_property_A_and_chclass(p1, c, n)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        n = get_random_number(20, 500, 1)[0]
        u = random.sample(uses, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_property_L_and_use(p1, u, n)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        n = get_random_number(20, 500, 1)[0]
        u = random.sample(uses, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt =  get_species_from_property_H_and_use(p1, u, n)
        data = write_json(prompt, query_text)
        json_objects.append(data)
    
        min = get_random_number(20, 500, 1)[0]
        max = min + get_random_number(20, 500, 1)[0]
        u = random.sample(uses, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_property_O_and_use(p1, u, min, max)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        min = get_random_number(20, 500, 1)[0]
        max = min + get_random_number(20, 500, 1)[0]
        u = random.sample(uses, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_property_I_and_use(p1, u, min, max)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        n = get_random_number(20, 500, 1)[0]
        u = random.sample(uses, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        query_text, prompt =  get_species_from_property_A_and_use(p1, u, n)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        s = random.sample(species, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        query_text, prompt = get_two_property_from_species(p1, p2, s)
        data = write_json(prompt , query_text)
        json_objects.append(data)

        c = random.sample(chemicalclasses, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        query_text, prompt = get_two_property_from_chemclass(p1,p2,c)
        data = write_json(prompt , query_text)
        json_objects.append(data) 

        n1 = get_random_number(20, 500, 1)[0]
        n2 = get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_two_property_L_L(p1, p2, n1, n2)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        n1 = get_random_number(20, 500, 1)[0]
        n2 = get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_two_property_L_H(p1, p2, n1, n2)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        n1 = get_random_number(20, 500, 1)[0]
        min2 = get_random_number(20, 500, 1)[0]
        max2 = min + get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_two_property_L_I(p1, p2, n1, min2, max2)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        n1 = get_random_number(20, 500, 1)[0]
        min2 = get_random_number(20, 500, 1)[0]
        max2 = min + get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_two_property_L_O(p1, p2, n1, min2, max2)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        n1 = get_random_number(20, 500, 1)[0]
        n2 = get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_two_property_L_A(p1, p2, n1, n2)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        n1 = get_random_number(20, 500, 1)[0]
        n2 = get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_two_property_H_H(p1, p2, n1, n2)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        n1 = get_random_number(20, 500, 1)[0]
        min2 = get_random_number(20, 500, 1)[0]
        max2 = min + get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_two_property_H_I(p1, p2, n1, min2, max2)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        n1 = get_random_number(20, 500, 1)[0]
        min2 = get_random_number(20, 500, 1)[0]
        max2 = min + get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_two_property_H_O(p1, p2, n1, min2, max2)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        n1 = get_random_number(20, 500, 1)[0]
        n2 = get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_two_property_H_A(p1, p2, n1, n2)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        min1 = get_random_number(20, 500, 1)[0]
        max1 = min + get_random_number(20, 500, 1)[0]
        min2 = get_random_number(20, 500, 1)[0]
        max2 = min + get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_two_property_I_I(p1, p2, min1, max1, min2, max2)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        min1 = get_random_number(20, 500, 1)[0]
        max1 = min + get_random_number(20, 500, 1)[0]
        min2 = get_random_number(20, 500, 1)[0]
        max2 = min + get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_two_property_I_O(p1, p2, min1, max1, min2, max2)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        min1 = get_random_number(20, 500, 1)[0]
        max1 = min + get_random_number(20, 500, 1)[0]
        n2 = get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_two_property_I_A(p1, p2, min1, max1, n2)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        min1 = get_random_number(20, 500, 1)[0]
        max1 = min + get_random_number(20, 500, 1)[0]
        min2 = get_random_number(20, 500, 1)[0]
        max2 = min + get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_two_property_O_O(p1, p2, min1, max1, min2, max2)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        min1 = get_random_number(20, 500, 1)[0]
        max1 = min + get_random_number(20, 500, 1)[0]
        n2 = get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_two_property_O_A(p1, p2, min1, max1, n2)
        data = write_json(prompt, query_text)
        json_objects.append(data)

        n1 = get_random_number(20, 500, 1)[0]
        n2 = get_random_number(20, 500, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        query_text, prompt = get_species_from_two_property_A_A(p1, p2, n1, n2)
        data = write_json(prompt, query_text)
        json_objects.append(data)


        s = random.sample(species, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        p3 = random.sample(property_list, 1)[0]
        query_text, prompt = get_three_property_from_species(p1, p2, p3, species)
        data = write_json(prompt , query_text)
        json_objects.append(data)  

        c = random.sample(chemicalclasses, 1)[0]
        p1 = random.sample(property_list, 1)[0]
        p2 = random.sample(property_list, 1)[0]
        p3 = random.sample(property_list, 1)[0]
        query_text, prompt = get_three_property_from_chemclass(p1,p2,p3,c)
        data = write_json(prompt , query_text)
        json_objects.append(data) 

        i1 = random.sample(identifier_list, 1)[0]
        s = random.sample(species, 1)[0]
        query_text, prompt = get_identifier_from_species(i1, s)
        data = write_json(prompt , query_text)
        json_objects.append(data)
    
    with open(filename, 'w') as f_train:
        json.dump(json_objects, f_train)

    print("all done")

def write_json(prompt, completion):
        data = {
            "prompt": prompt + delimiter,
            "completion": ' ' + completion + end_delimiter
        }
        return data

create_query_template(filename = 'chatbot/fine_tuning/resources/test_' + timestamp + '.json')



