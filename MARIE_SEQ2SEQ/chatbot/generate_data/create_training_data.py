from datetime import datetime
import json
import random
import os
from typing import List

from sklearn.model_selection import train_test_split

from generate_data.Q_and_query_template_from_species import *
from generate_data.Q_and_query_template_from_chemclass import *
from generate_data.Q_and_query_template_from_qualifier import *
from generate_data.Q_and_query_template_from_two_qualifier import *


delimiter = ''  # GPT best practice '\n\n###\n\n'
end_delimiter = '' # GPT best practice 'END'

timestamp = datetime.now().strftime("%Y-%m-%d_%H.%M.%S")
random.seed(2023)


def generate_examples(
    properties: List[str], 
    identifiers: List[str], 
    species: List[str], 
    chemicalclasses: List[str],
    uses: List[str],
    N: int
):
    examples = []

    for _ in range(N):
        # from_species
        s = random.choice(species)
        p1 = random.choice(properties)
        data = get_property_from_species(p1, s)
        examples.append(data)

        s = random.choice(species)
        p1, p2 = random.sample(properties, 2)
        data = get_two_property_from_species(p1, p2, s)
        examples.append(data)

        s = random.choice(species)
        p1, p2, p3 = random.sample(properties, 3)
        data = get_three_property_from_species(p1, p2, p3, s)
        examples.append(data)

        i1 = random.choice(identifiers)
        s = random.choice(species)
        data = get_identifier_from_species(i1, s)
        examples.append(data)

        s = random.choice(species)
        data = get_chemclass_from_species(s)
        examples.append(data)
        
        s = random.choice(species)
        data = get_use_from_species(s)
        examples.append(data)

        s = random.choice(species)
        data = get_use_and_chemclass_from_species(s)
        examples.append(data)

        s = random.choice(species)
        data = get_all_properties(s)
        examples.append(data)

        s = random.choice(species)
        data = get_all_identifiers(s)
        examples.append(data)

        s1, s2 = random.sample(species, 2)
        p1 = random.choice(properties)
        data = get_compare_properties(p1, s1, s2)
        examples.append(data)

        # from_chemclass
        c = random.choice(chemicalclasses)
        p1 = random.choice(properties)
        data = get_property_from_chemclass(p1, c)
        examples.append(data)

        c = random.choice(chemicalclasses)
        p1, p2 = random.sample(properties, 2)
        data = get_two_property_from_chemclass(p1, p2, c)
        examples.append(data)

        c = random.choice(chemicalclasses)
        p1, p2, p3 = random.sample(properties, 3)
        data = get_three_property_from_chemclass(p1, p2, p3, c)
        examples.append(data)

        # from_qualifier
        n = random.randint(20, 500)
        p1 = random.choice(properties)
        data = get_species_from_property_L(p1, n)
        examples.append(data)

        n = random.randint(20, 500)
        p1 = random.choice(properties)
        data = get_species_from_property_H(p1, n)
        examples.append(data)

        minval = random.randint(20, 500)
        maxval = minval + random.randint(20, 500)
        p1 = random.choice(properties)
        data = get_species_from_property_O(p1, minval, maxval)
        examples.append(data)
        
        minval = random.randint(20, 500)
        maxval = minval + random.randint(20, 500)
        p1 = random.choice(properties)
        data = get_species_from_property_I(p1, minval, maxval)
        examples.append(data)

        n = random.randint(20, 500)
        p1 = random.choice(properties)
        data = get_species_from_property_A(p1, n)
        examples.append(data)

        c = random.choice(chemicalclasses)
        data = get_species_from_chemclass(c)
        examples.append(data)

        u = random.choice(uses)
        data = get_species_from_use(u)
        examples.append(data)

        # from_two_qualifier
        n1 = random.randint(20, 500)
        n2 = random.randint(20, 500)
        p1, p2 = random.sample(properties, 2)
        data = get_species_from_two_property_L_L(p1, p2, n1, n2)
        examples.append(data)

        n1 = random.randint(20, 500)
        n2 = random.randint(20, 500)
        p1, p2 = random.sample(properties, 2)
        data = get_species_from_two_property_L_H(p1, p2, n1, n2)
        examples.append(data)

        n1 = random.randint(20, 500)
        minval = random.randint(20, 500)
        maxval = minval + random.randint(20, 500)
        p1, p2 = random.sample(properties, 2)
        data = get_species_from_two_property_L_I(p1, p2, n1, minval, maxval)
        examples.append(data)

        n1 = random.randint(20, 500)
        minval = random.randint(20, 500)
        maxval = minval + random.randint(20, 500)
        p1, p2 = random.sample(properties, 2)
        data = get_species_from_two_property_L_O(p1, p2, n1, minval, maxval)
        examples.append(data)

        n1 = random.randint(20, 500)
        n2 = random.randint(20, 500)
        p1, p2 = random.sample(properties, 2)
        data = get_species_from_two_property_L_A(p1, p2, n1, n2)
        examples.append(data)
        
        n1 = random.randint(20, 500)
        n2 = random.randint(20, 500)
        p1, p2 = random.sample(properties, 2)
        data = get_species_from_two_property_H_H(p1, p2, n1, n2)
        examples.append(data)

        n1 = random.randint(20, 500)
        minval = random.randint(20, 500)
        maxval = minval + random.randint(20, 500)
        p1, p2 = random.sample(properties, 2)
        data = get_species_from_two_property_H_I(p1, p2, n1, minval, maxval)
        examples.append(data)

        n1 = random.randint(20, 500)
        minval = random.randint(20, 500)
        maxval = minval + random.randint(20, 500)
        p1, p2 = random.sample(properties, 2)
        data = get_species_from_two_property_H_O(p1, p2, n1, minval, maxval)
        examples.append(data)

        n1 = random.randint(20, 500)
        n2 = random.randint(20, 500)
        p1, p2 = random.sample(properties, 2)
        data = get_species_from_two_property_H_A(p1, p2, n1, n2)
        examples.append(data)
        
        minval1 = random.randint(20, 500)
        maxval1 = minval1 + random.randint(20, 500)
        minval2 = random.randint(20, 500)
        maxval2 = minval2 + random.randint(20, 500)
        p1, p2 = random.sample(properties, 2)
        data = get_species_from_two_property_I_I(
            p1, p2, minval1, maxval1, minval2, maxval2
        )
        examples.append(data)

        minval1 = random.randint(20, 500)
        maxval1 = minval1 + random.randint(20, 500)
        minval2 = random.randint(20, 500)
        maxval2 = minval2 + random.randint(20, 500)
        p1, p2 = random.sample(properties, 2)
        data = get_species_from_two_property_I_O(
            p1, p2, minval1, maxval1, minval2, maxval2
        )
        examples.append(data)

        minval1 = random.randint(20, 500)
        maxval1 = minval1 + random.randint(20, 500)
        n2 = random.randint(20, 500)
        p1, p2 = random.sample(properties, 2)
        data = get_species_from_two_property_I_A(p1, p2, minval1, maxval1, n2)
        examples.append(data)

        minval1 = random.randint(20, 500)
        maxval1 = minval1 + random.randint(20, 500)
        minval2 = random.randint(20, 500)
        maxval2 = minval2 + random.randint(20, 500)
        p1, p2 = random.sample(properties, 2)
        data = get_species_from_two_property_O_O(
            p1, p2, minval1, maxval1, minval2, maxval2
        )
        examples.append(data)

        minval1 = random.randint(20, 500)
        maxval1 = minval1 + random.randint(20, 500)
        n2 = random.randint(20, 500)
        p1, p2 = random.sample(properties, 2)
        data = get_species_from_two_property_O_A(p1, p2, minval1, maxval1, n2)
        examples.append(data)

        n1 = random.randint(20, 500)
        n2 = random.randint(20, 500)
        p1, p2 = random.sample(properties, 2)
        data = get_species_from_two_property_A_A(p1, p2, n1, n2)
        examples.append(data)

        c = random.choice(chemicalclasses)
        u = random.choice(uses)
        data = get_species_from_chclass_and_use(c, u)
        examples.append(data)

        n = random.randint(20, 500)
        c = random.choice(chemicalclasses)
        p1 = random.choice(properties)
        data = get_species_from_property_L_and_chclass(p1, c, n)
        examples.append(data)

        n = random.randint(20, 500)
        c = random.choice(chemicalclasses)
        p1 = random.choice(properties)
        data = get_species_from_property_H_and_chclass(p1, c, n)
        examples.append(data)

        minval = random.randint(20, 500)
        maxval = minval + random.randint(20, 500)
        c = random.choice(chemicalclasses)
        p1 = random.choice(properties)
        data = get_species_from_property_O_and_chclass(p1, c, minval, maxval)
        examples.append(data)

        minval = random.randint(20, 500)
        maxval = minval + random.randint(20, 500)
        c = random.choice(chemicalclasses)
        p1 = random.choice(properties)
        data = get_species_from_property_I_and_chclass(p1, c, minval, maxval)
        examples.append(data)

        n = random.randint(20, 500)
        c = random.choice(chemicalclasses)
        p1 = random.choice(properties)
        data = get_species_from_property_A_and_chclass(p1, c, n)
        examples.append(data)

        n = random.randint(20, 500)
        u = random.choice(uses)
        p1 = random.choice(properties)
        data = get_species_from_property_L_and_use(p1, u, n)
        examples.append(data)

        n = random.randint(20, 500)
        u = random.choice(uses)
        p1 = random.choice(properties)
        data = get_species_from_property_H_and_use(p1, u, n)
        examples.append(data)

        minval = random.randint(20, 500)
        maxval = minval + random.randint(20, 500)
        u = random.choice(uses)
        p1 = random.choice(properties)
        data = get_species_from_property_O_and_use(p1, u, minval, maxval)
        examples.append(data)

        minval = random.randint(20, 500)
        maxval = minval + random.randint(20, 500)
        u = random.choice(uses)
        p1 = random.choice(properties)
        data = get_species_from_property_I_and_use(p1, u, minval, maxval)
        examples.append(data)

        n = random.randint(20, 500)
        u = random.choice(uses)
        p1 = random.choice(properties)
        data = get_species_from_property_A_and_use(p1, u, n)
        examples.append(data)
    
    return examples


def generate_dataset(
    directory = "chatbot/generate_data/resources/", 
    N_train: int = 30,
    N_dev: int = 3,
    N_test: int = 3,
):
    property_list = [
        'AtomChiralCount', 'AtomChiralDefCount', 'AtomChiralUndefCount', 'BondChiralCount', 'BondChiralDefCount',
        'BondChiralUndefCount', 'CanonicalizedCompound', 'Charge', 'CompoundComplexity', 'CovalentUnitCount',
        'ExactMass', 'HeavyAtomCount', 'HydrogenBondAcceptorCount', 'HydrogenBondDonorCount', 'IsotopeAtomCount', 
        'MolecularWeight', 'MonoIsotopicWeight', 'RotatableBondCount', 'SubStructureKeysFingerprint', 
        'TautomerCount', 'XLogP3', 'AutoignitionTemperature', 'Caco2Permeability', 'CollisionCrossSection',
        'Hydrophobicity', 'IonizationPotential', 'IsoelectricPoint', 'LogP', 'LogS', 'PolarSurfaceArea', 
        'BoilingPoint', 'Density', 'DissociationConstants', 'EnthalpyOfSublimation', 'FlashPoint', 'StandardEnthalpyOfFormation', 
        'HeatOfCombustion', 'HeatOfVaporization', 'HenrysLawConstant', 'MeltingPoint', 'OpticalRotation', 'Solubility',
        'SurfaceTension', 'VaporDensity', 'VaporPressure', 'Viscosity'
    ]

    identifier_list = ['ChebiID', 'CID', 'EmpiricalFormula', 'InChI', 'InChIKey', 'IUPACName', 'MolecularFormula', 'SMILES']

    with open('chatbot/generate_data/resources/species.txt') as f:
        species = [line.strip() for line in f.readlines()]
    with open('chatbot/generate_data/resources/chemical_classes.txt') as f:
        chemicalclasses = [line.strip() for line in f.readlines()]
    with open('chatbot/generate_data/resources/uses.txt') as f:
        uses = [line.strip() for line in f.readlines()]
    
    train_species, test_species = train_test_split(species, test_size=0.1)
    train_chemicalclasses, test_chemicalclasses = train_test_split(chemicalclasses, test_size=0.1)
    train_uses, test_uses = train_test_split(uses, test_size=0.1)

    train_examples = generate_examples(
        properties=property_list, 
        identifiers=identifier_list, 
        species=train_species, 
        chemicalclasses=train_chemicalclasses, 
        uses=train_uses,
        N=N_train,
    )
    dev_examples = generate_examples(
        properties=property_list, 
        identifiers=identifier_list, 
        species=train_species, 
        chemicalclasses=train_chemicalclasses, 
        uses=train_uses,
        N=N_dev,
    )
    test_examples = generate_examples(
        properties=property_list, 
        identifiers=identifier_list, 
        species=test_species, 
        chemicalclasses=test_chemicalclasses, 
        uses=test_uses,
        N=N_test,
    )

    with open(os.path.join(directory, f"train_{timestamp}.json"), "w") as f:
        json.dump(train_examples, f, indent=4)

    with open(os.path.join(directory, f"dev_{timestamp}.json"), "w") as f:
        json.dump(dev_examples, f, indent=4)

    with open(os.path.join(directory, f"test_{timestamp}.json"), "w") as f:
        json.dump(test_examples, f, indent=4)

    print("all done")


def write_json(prompt, completion):
        data = {
            "prompt": prompt + delimiter,
            "completion": ' ' + completion + end_delimiter
        }
        return data

if __name__ == "__main__":
    generate_dataset(
        directory="chatbot/generate_data/resources/",     
        N_train=300,
        N_dev=30,
        N_test=30,
    )
