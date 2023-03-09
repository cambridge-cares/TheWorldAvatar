import os

from ontoagent_generator import OntoAgentGenerator
from location import FILE_DIR
agent = {
    "question_templates":
        ['<attribute> of OPF with donor <species>',
         '<attribute> of <species>'],
    "http_url": "http://kg.cmclinnovations.com:5001/api/model/predict",
    "outputs": [
        {
            "data_name": "pce",
            "data_type": "http://fake_concept_for_power_conversion_efficiency",
            "is_array": False,
            "ner_label": "attribute",
            "has_qualifier": ['']

        }
    ],
    "inputs": [
        {
            "data_name": "smiles",
            "data_type": "http://fake_concept_for_species",
            "is_array": True,
            "ner_label": "species"
        }
    ],

    "qualifiers": [
        {
            "data_name": "x",
            "data_type": "http://fake_concept_for_Nothing",
            "is_array": False,
            "ner_label": "x"
        }
    ]

}
# inchi (I) PCE (O)

# 1. (inchi smiles) SMILES PCE #
# 2.


#  what is [enthalpy of formation] for [ethanol] at 200K
# 1. Rotational constants (), frequency (), energy level ()  Thermo (O)

# Donor's SMILES + InChi

og = OntoAgentGenerator('PCE_Agent')
og.create_instance(agent)
og.this_agent.save(os.path.join(FILE_DIR, 'PCE_Agent.owl'), format='rdfxml')