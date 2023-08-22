from datetime import datetime
import json
import pathlib
import random
import os

import numpy as np
from sklearn.model_selection import train_test_split

from generate_data.example_generator import ExampleGenerator, make_arg_samplers


DIRPATH = pathlib.Path(__file__).parent.resolve()

delimiter = ''  # GPT best practice '\n\n###\n\n'
end_delimiter = '' # GPT best practice 'END'

timestamp = datetime.now().strftime("%Y-%m-%d_%H.%M.%S")

random.seed(2023)
np.random.seed(2023)


def generate_dataset(
    directory="chatbot/generate_data/resources/",
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

    train_example_generators = []
    test_example_generators = []

    train_arg_samplers, test_arg_samplers = make_arg_samplers(
        properties=property_list,
        identifiers=identifier_list,
        species=species,
        chemicalclasses=chemicalclasses,
        uses=uses,
    )

    for dirpath, dirnames, filenames in os.walk(os.path.join(DIRPATH, "templates")):
        if dirnames:
            dirnames.sort()
            continue

        template_name = os.path.basename(dirpath)

        with open(os.path.join(dirpath, "query_template.txt"), "r") as f:
            query_template = f.read().strip()

        with open(os.path.join(dirpath, "query_compact_template.txt"), "r") as f:
            query_compact_template = f.read().strip()

        with open(os.path.join(dirpath, "question_templates.txt"), "r") as f:
            question_templates = [x.strip() for x in f.readlines()]

        train_question_templates, test_question_templates = train_test_split(
            question_templates, test_size=0.1
        )

        train_example_generators.append(ExampleGenerator(
            template_name=template_name,
            query_template=query_template,
            query_compact_template=query_compact_template,
            qn_templates=train_question_templates,
            arg_samplers=train_arg_samplers,
        ))
        test_example_generators.append(ExampleGenerator(
            template_name=template_name,
            query_template=query_template,
            query_compact_template=query_compact_template,
            qn_templates=test_question_templates,
            arg_samplers=test_arg_samplers,
        ))

    train_examples = [next(example_generator) for _ in range(N_train) for example_generator in train_example_generators]
    dev_examples = [next(example_generator) for _ in range(N_dev) for example_generator in test_example_generators]
    test_examples = [next(example_generator) for _ in range(N_test) for example_generator in test_example_generators]

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
        N_train=100,
        N_dev=10,
        N_test=10,
    )
