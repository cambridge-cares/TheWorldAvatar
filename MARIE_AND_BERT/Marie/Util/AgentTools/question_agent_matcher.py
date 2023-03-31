import sys
sys.path.append("")
import os
import torch 
import pandas as pd
import numpy as np
from Marie.Util.location import DATA_DIR
from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.AgentTools.agent_matrix_generator import AgentMatrixGenerator
from Marie.Util.Models.TransERelPredictionModel import TransERelPredictionModel
from Marie.Util.CommonTools.NLPTools import NLPTools

class QuestionAgentMatcher():

    def __init__(self, question):
        
        #Use TransERelPredictionModel to obtain the embedding of the relation in the given question
        my_model = TransERelPredictionModel(device=torch.device("cpu"),
                                        dataset_dir=os.path.join(DATA_DIR, "CrossGraph/agents"), dim=40,
                                        mode="agent")
        my_model.load_model("bert_ontoagent")
        nlp_tool = NLPTools()
        _, tokenzied_question = nlp_tool.tokenize_question(question=question, repeat_num=0)
        output_emb = my_model.predict(question=tokenzied_question)
        self.question_rel_embedding = output_emb.view(-1).tolist()

        #List of all agents
        self.agents = ['ontothermoagent', 'ontopceagent', 'chem1agent', 'chem2agent', 'chem3agent']

        #Create the input-output configuration matrices for each agent
        self.agent_matrices = {}
        for agent in self.agents:
            matrix_generator = AgentMatrixGenerator(agent=agent)
            self.agent_matrices[agent] = matrix_generator.create_agent_matrices()

        #Define the input embedding from ThermoAgent used by both PCE and ThermoAgent
        # TODO: Removed hardcoded agent input after combining the embeddings of all the ontoagents.
        self.agent_input = "Species"
        agent_dataset_dir = os.path.join(DATA_DIR, "CrossGraph", "ontothermoagent")
        agent_ent_embedding = pd.read_csv(os.path.join(agent_dataset_dir, 'ent_embedding.tsv'), sep='\t',
                                         header=None)
        agent_file_loader = FileLoader(full_dataset_dir=agent_dataset_dir)
        agent_entity2idx, agent_idx2entity, agent_rel2idx, agent_idx2rel = agent_file_loader.load_index_files()
        self.question_entity_embedding = agent_ent_embedding.iloc[agent_entity2idx[self.agent_input]]
        
        self.run()

    def run(self):
        question_matrix = torch.Tensor(np.array([self.question_entity_embedding, self.question_rel_embedding]))       
        agent_scores = {}
        for agent in self.agents:
            agent_matrices = self.agent_matrices[agent]
            agent_score = []
            for matrix in agent_matrices:
                row_score = []
                matrix = torch.Tensor(matrix)
                for i in range(matrix.shape[0]):
                    sim_scores=[]
                    for j in range(question_matrix.shape[0]):
                        similarity = torch.nn.functional.cosine_similarity(matrix[i], question_matrix[j], dim=0).item()
                        if similarity < 0:
                            sim_scores.append(0)
                        elif similarity > 1:
                            sim_scores.append(1)
                        else:
                            sim_scores.append(similarity)
                    row_score.append(max(sim_scores))
                agent_score.append(np.average(row_score))
            agent_scores[agent] = max(agent_score)
        
        print(agent_scores)
        result = max(agent_scores.items(), key=lambda x: x[1])

        print(result[0])
        return result
    
if __name__ == '__main__':
    # agents = ['ontopceagent', 'ontothermoagent', 'chem1agent', 'chem2agent', 'chem3agent']
    # entity_domain = 'ontospecies'
    # input = 'Species'
    # rel_embedding = [-0.0279401578009128, -0.0229824669659137, 0.2679637968540191, -0.1346896737813949, -0.078743889927864, -0.2619713246822357, 0.2464446872472763, 0.0643166303634643, -0.1888458281755447, 0.0859422758221626, 0.0403431616723537, -0.277249664068222, -0.1264708042144775, 0.072978638112545, 0.0415104962885379, 0.163387268781662, 0.2007157504558563, -0.2151760756969452, -0.2424642890691757, 0.1162319034337997, -0.0781556814908981, -0.1324226558208465, -0.030205700546503, 0.0272158700972795, -0.2757452726364136, -0.0948667675256729, 0.0298788473010063, -0.0635488927364349, 0.0718482732772827, 0.0287968646734952, -0.214311271905899, -0.159093752503395, -0.1585248559713363, -0.2487790584564209, 0.2782934308052063, 0.0718457624316215, 0.2232714891433715, -0.0952865183353424, -0.0452908724546432, -0.1267632693052292]
    question = "what is the power conversion efficiency of benzene"
    my_matcher = QuestionAgentMatcher(question=question)