import sys
sys.path.append("")
import json
import os
import pandas as pd
import numpy as np
from Marie.Util.location import DATA_DIR
from Marie.Util.CommonTools.FileLoader import FileLoader

class AgentMatrixGenerator():

    def __init__(self, agent):
        self.agent = agent
        self.full_dataset_dir = os.path.join(DATA_DIR, "CrossGraph", "agents", self.agent)
        self.agent_info_path = os.path.join(self.full_dataset_dir, 'info_dict.json')

        if os.path.exists(self.full_dataset_dir):
            self.ent_embedding = pd.read_csv(os.path.join(self.full_dataset_dir, 'ent_embedding.tsv'), sep='\t',
                                            header=None)
            self.file_loader = FileLoader(full_dataset_dir=self.full_dataset_dir)
            self.entity2idx, self.idx2entity, self.rel2idx, self.idx2rel = self.file_loader.load_index_files()
            
        self.pce = False
        if self.agent == 'ontopceagent':
            self.pce = True
            ontothermoagent_dataset_dir = os.path.join(DATA_DIR, "CrossGraph", "agents", "ontothermoagent")
            ontothermoagent_file_loader = FileLoader(full_dataset_dir=ontothermoagent_dataset_dir)
            self.thermo_entity2idx, self.thermo_idx2entity, self.thermo_rel2idx, self.thermo_idx2rel = ontothermoagent_file_loader.load_index_files()
            self.thermo_ent_embedding = pd.read_csv(os.path.join(ontothermoagent_dataset_dir, 'ent_embedding.tsv'), sep='\t', header=None)       

    def create_agent_matrices(self):
        agent_matrices = {}

        if os.path.exists(self.agent_info_path):
            with open(self.agent_info_path, 'r') as f:
                data = json.load(f)

            inputs = data['input']
            outputs = data['output']

            for input in inputs:
                for output in outputs:
                    if self.pce:
                        agent_matrices[output] = np.array([list(self.thermo_ent_embedding.iloc[self.thermo_entity2idx[input]]), list(self.ent_embedding.iloc[self.entity2idx[output]]) ])
                    else:
                        agent_matrices[output] = np.array([list(self.ent_embedding.iloc[self.entity2idx[input]]), list(self.ent_embedding.iloc[self.entity2idx[output]]) ])
        else:
            agent_matrices['dummyOutput'] = np.ones((2, 40))
            return agent_matrices

        return agent_matrices

if __name__ == '__main__':
    my_matcher = AgentMatrixGenerator(agent='ontothermoagent')
    print(my_matcher.create_agent_matrices())