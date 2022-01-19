from UI.source.Agent_query.ontoagent_generator import OntoAgentGenerator

agent = {
    "http_url": "http://somewhereincmcl.com/pce",
    "outputs": [
        {
            "data_name": "power conversion efficiency",
            "data_type": "http://fake_concept_for_power_conversion_efficiency",
            "is_array": False,
            "ner_label": "attribute"
        }
    ],
    "inputs": [
        {
            "data_name": "species",
            "data_type": "http://fake_concept_for_species",
            "is_array": False,
            "ner_label": "species"
        },
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
og.this_agent.save('PCE_Agent.owl', format='rdfxml')