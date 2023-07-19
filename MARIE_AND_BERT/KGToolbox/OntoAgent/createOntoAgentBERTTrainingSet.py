import itertools
import json
import os
import random
import re
from pprint import pprint

"""
enthalpy
thermo property
internal energy
heat capacity at constant pressure
heat capacity at constant volume
entropy
heat capacity
gibbs energy
power conversion efficiency
enthalpy
thermo property
"""
import pandas as pd

from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.location import DATA_DIR


def split_camel_case(camelCase):
    return ' '.join(re.findall(r'[A-Z](?:[a-z]+|[A-Z]*(?=[A-Z]|$))', camelCase)).strip().lower()


def extend_to_length(my_list, length):
    return my_list
    # return list(itertools.islice(itertools.cycle(my_list), length))


class OntoAgentBERTTrainingSetCreator:
    # the BERT model is expected to produce the 80 dim embedding of 1. relation embedding 2. agent embedding

    def __init__(self):
        self.base_dir = os.path.join(DATA_DIR, "CrossGraph/agents")
        self.agents = ["ontopceagent", "ontothermoagent"]
        self.agent_info_dict = self.load_agent_info_from_agent_path(self.agents)
        self.qualifier_text_dict = {
            #  "Temperature": ["at %s degrees", "at %s K", "at temperature of %s degrees", "at temperature of %s K"],
            "Temperature": ["%s"],
            #   "Pressure": ["at pressure of %s Pa", "at %s Pa", "at %s atm", "at pressure of %s atm"]
            "Pressure": ["%s"]
        }

        self.additional_text = {"gibbs energy": "gibbs free energy",
                                "thermo property": "thermal property",
                                "internal energy": "energy on a microscopic scale",
                                "power conversion efficiency": "pce",
                                "enthalpy": "standard enthalpy of formation",
                                "heat capacity": "thermal capacity",
                                "heat capacity at constant volume": "heat capacity with fixed volume",
                                "heat capacity at constant pressure": "heat capacity with fixed pressure",
                                "entropy": "entropy"

                                }
        self.random_text = ["somethimg something", "while this is totally not relevant", "i don't think so",
                            "reactions", "molecular weight", "boiling point", "molecular weight", "chemical formula",
                            "reaction produces"]
        self.agent_name_iri_dict = {"ontopceagent": "Service_eb2cb048d3454897a0baf1a5de24282ed4290f8d",
                                    "ontothermoagent": "Service_e3868d6a4367539d8e265e790e76a6f0511c1464"}

    def load_agent_info_from_agent_path(self, agent_name_list):
        agent_info_dict = {}
        for agent_name in agent_name_list:
            agent_data_dir = os.path.join(self.base_dir, agent_name)
            agent_info = json.loads(open(os.path.join(agent_data_dir, "info_dict.json")).read())
            agent_info_dict[agent_name] = agent_info
        return agent_info_dict

    def create_random_neg_questions(self):
        random_question_list = []
        for random_text in self.random_text:
            random_question_list.append((random_text.strip(), 0, 0, "none"))

        return random_question_list

    def make_qualifier_component(self, qualifier_list):
        qualifier_combination_list = [list(set(itertools.combinations(qualifier_list, n))) for n in
                                      range(len(qualifier_list) + 1)]
        qualifier_text_list = []
        for qualifier_combination in qualifier_combination_list:
            for qualifier in qualifier_combination:
                qualifier_component_list = []
                for qualifier_key in qualifier:
                    numerical_value = random.randint(0, 10)
                    tmp = []
                    for qualifier_part in self.qualifier_text_dict[qualifier_key]:
                        qualifier_part = qualifier_part % numerical_value
                        tmp.append(qualifier_part)
                    qualifier_component_list.append(tmp)
                combined_qualifier_components = [" and ".join(comb) for comb in
                                                 list(set(itertools.product(*qualifier_component_list)))]

                qualifier_text_list += combined_qualifier_components

        return qualifier_text_list

    def create_question(self, agent_info, agent_name, ):

        agent_path = os.path.join(self.base_dir, agent_name)
        current_agent_file_loader = FileLoader(full_dataset_dir=agent_path)
        entity2idx, _, _, _ = current_agent_file_loader.load_index_files()

        agent_iri = self.agent_name_iri_dict[agent_name]
        question_list = []
        question_template = "%s %s"
        output_list = [(split_camel_case(output), output) for output in agent_info["output"]]
        output_list_appended = [(self.additional_text[split_camel_case(output)], output) for output in
                                agent_info["output"]]
        output_list += output_list_appended
        qualifier_list = agent_info["qualifier"]
        qualifier_text_list = self.make_qualifier_component(qualifier_list)
        for output, output_iri in output_list:
            for qualifier_text in qualifier_text_list:
                # question = question_template % (output, qualifier_text)
                question = question_template % (output, "")
                # question, agent_iri, output_iri, agent_name
                question_list.append((question.strip(), entity2idx[agent_iri], entity2idx[output_iri], agent_name))
        return question_list

    def run(self):
        all_question_list = []
        max_length = 0
        for agent_name, agent_info in self.agent_info_dict.items():
            question_list = self.create_question(agent_info, agent_name)
            if len(question_list) > max_length:
                max_length = len(question_list)

            if agent_name == "ontopceagent":
                question_list = question_list * 10
            all_question_list.append(question_list)

#         all_question_list = [extend_to_length(question_list, max_length) for question_list in all_question_list]
        # all_question_list = [extend_to_length(question_list, max_length) for question_list in all_question_list]
        all_question_list = (list(itertools.chain.from_iterable(all_question_list)))
        random_question_list = self.create_random_neg_questions() * 50
        all_question_list += random_question_list

        # new_role = ("something random",	-1, -1,	"ontothermoagent")
        # all_question_list.append(new_role)
        df = pd.DataFrame(all_question_list)
        # question agent rel
        df.columns = ["question", "agent", "rel", "agent_name"]
        df.to_csv(os.path.join(self.base_dir, "score_model_training.tsv"), sep="\t")


if __name__ == "__main__":
    my_creator = OntoAgentBERTTrainingSetCreator()
    my_creator.run()
