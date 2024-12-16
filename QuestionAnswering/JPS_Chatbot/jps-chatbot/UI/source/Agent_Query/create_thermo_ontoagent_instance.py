from ontoagent_generator import OntoAgentGenerator
from location import FILE_DIR
import os


agent = {
    "question_templates":
        ['<attribute> of <species>', '<attribute> <species> at <qualifier>',
         '<attribute> of <species> at <qualifier> and <qualifier>',
         '<species> singlequotesign <attribute> at <qualifier>',
         '<species> singlequotesign <attribute> at <qualifier> and <qualifier>'],
    "http_url": " http://127.0.0.1:5000/thermo_agent",
    "outputs": [
        {
            "data_name": "ThermalProperty",
            "data_type": "http://fake_concept_for_general_thermal_property",
            "is_array": True,
            "ner_label": "attribute",
            "has_qualifier": ["temperature"]
        },
        {
            "data_name": "HeatCapacity",
            "data_type": "http://fake_concept_for_heat_capacity",
            "is_array": True,
            "ner_label": "attribute",
            "has_qualifier": ["temperature"]
        },
        {
            "data_name": "HeatCapacityAtConstVolume",
            "data_type": "http://fake_concept_for_heat_capacity_at_constant_volume",
            "is_array": False,
            "ner_label": "attribute",
            "has_qualifier": ["temperature"]
        },
        {
            "data_name": "HeatCapacityAtConstPressure",
            "data_type": "http://fake_concept_for_heat_capacity_at_constant_pressure",
            "is_array": False,
            "ner_label": "attribute",
            "has_qualifier": ["temperature"]
        },
        {
            "data_name": "InternalEnergy",
            "data_type": "http://fake_concept_for_InternalEnergy",
            "is_array": False,
            "ner_label": "attribute",
            "has_qualifier": ["temperature", "pressure"]
        },
        {
            "data_name": "enthalpy",
            "data_type": "http://fake_concept_for_enthalpy",
            "is_array": False,
            "ner_label": "attribute",
            "has_qualifier": ["temperature"]
        },        {
            "data_name": "entropy",
            "data_type": "http://fake_concept_for_entropy",
            "is_array": False,
            "ner_label": "attribute",
            "has_qualifier": ["temperature", "pressure"]
        },        {
            "data_name": "GibbsEnergy",
            "data_type": "http://fake_concept_for_GibbsEnergy",
            "is_array": False,
            "ner_label": "attribute",
            "has_qualifier": ["temperature", "pressure"]
        }

    ],
    "inputs": [
        {
            "data_name": "species",
            "data_type": "http://fake_concept_for_species",
            "is_array": False,
            "ner_label": "species"
        }
    ],

    "qualifiers": [
        {
            "data_name": "temperature",
            "data_type": "http://fake_concept_for_temperature",
            "is_array": False,
            "ner_label": "temperature"
        },
        {
            "data_name": "pressure",
            "data_type": "http://fake_concept_for_pressure",
            "is_array": False,
            "ner_label": "pressure"
        }
    ]
}

# we have a bunch of outputs

og = OntoAgentGenerator('Thermo_Agent')
og.create_instance(agent)
og.this_agent.save(os.path.join(FILE_DIR, 'Thermo_Agent.owl'), format='rdfxml')

