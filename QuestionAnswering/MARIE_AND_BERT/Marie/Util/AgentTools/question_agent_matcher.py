import sys

sys.path.append("")
import os
import torch
import pandas as pd
import numpy as np
from Marie.Util.location import DATA_DIR
from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.AgentTools.agent_matrix_generator import AgentMatrixGenerator
from Marie.Util.AgentTools.agent_invoker import AgentInvoker
from Marie.Util.Models.TransERelPredictionModel import TransERelPredictionModel
from Marie.Util.CommonTools.NLPTools import NLPTools


class QuestionAgentMatcher():

    def __init__(self, tokenizer_name="bert-base-uncased"):

        # Use TransERelPredictionModel to obtain the embedding of the relation in the given question
        self.my_model = TransERelPredictionModel(device=torch.device("cpu"),
                                            dataset_dir=os.path.join(DATA_DIR, "CrossGraph/agents"), dim=40,
                                            mode="agent")
        self.my_model.load_model("bert_ontoagent")
        self.nlp_tool = NLPTools(tokenizer_name=tokenizer_name)


        # List of all agents
        self.agents = ['ontothermoagent', 'ontopceagent', 'chem1agent', 'chem2agent', 'chem3agent', 'NotAnAgent']

        # Create the input-output configuration matrices for each agent
        self.agent_matrices = {}
        for agent in self.agents:
            matrix_generator = AgentMatrixGenerator(agent=agent)
            self.agent_matrices[agent] = matrix_generator.create_agent_matrices()

        # Define the input embedding from ThermoAgent used by both PCE and ThermoAgent
        # TODO: Removed hardcoded agent input after combining the embeddings of all the ontoagents.
        self.agent_input = "Species"
        agent_dataset_dir = os.path.join(DATA_DIR, "CrossGraph", "agents", "ontothermoagent")
        agent_ent_embedding = pd.read_csv(os.path.join(agent_dataset_dir, 'ent_embedding.tsv'), sep='\t',
                                          header=None)
        agent_file_loader = FileLoader(full_dataset_dir=agent_dataset_dir)
        self.agent_entity2idx, agent_idx2entity, agent_rel2idx, agent_idx2rel = agent_file_loader.load_index_files()
        self.question_entity_embedding = agent_ent_embedding.iloc[self.agent_entity2idx[self.agent_input]]

    def run(self, question):

        _, tokenzied_question = self.nlp_tool.tokenize_question(question=question, repeat_num=0)
        output_emb = self.my_model.predict(question=tokenzied_question)
        question_rel_embedding = output_emb.view(-1).tolist()

        question_matrix = torch.Tensor(np.array([self.question_entity_embedding, question_rel_embedding]))
        agent_scores = {}
        for agent in self.agents:
            agent_matrices = self.agent_matrices[agent]
            agent_score = {}
            for output, matrix in agent_matrices.items():
                row_score = []
                matrix = torch.Tensor(matrix)
                for i in range(matrix.shape[0]):
                    sim_scores = []
                    for j in range(question_matrix.shape[0]):
                        similarity = torch.nn.functional.cosine_similarity(matrix[i], question_matrix[j], dim=0).item()
                        if similarity < 0:
                            sim_scores.append(0)
                        elif similarity > 1:
                            sim_scores.append(1)
                        else:
                            sim_scores.append(similarity)
                    row_score.append(max(sim_scores))
                agent_score[output] = np.average(row_score)
            # print(agent, agent_score)
            agent_scores[agent] = max(agent_score.items(), key=lambda x: x[1])
        # print(agent_scores)

        agent_result = max(agent_scores, key=lambda x: agent_scores[x][1])

        if agent_result == 'NotAnAgent':
            print('No relevant agent found')
            return None, None
        else:
            return agent_result, agent_scores[agent_result][0]


if __name__ == '__main__':
    # agents = ['ontopceagent', 'ontothermoagent', 'chem1agent', 'chem2agent', 'chem3agent']
    # question = "What reaction produces H2 + OH"
    question = "What is the molecular weight of ch4"
    my_matcher = QuestionAgentMatcher()
    agent, output = my_matcher.run(question=question)
    print("Output: ", output)
    print("Agent : ", agent)

    if (agent != None):
        my_invoker = AgentInvoker(agent=agent, output=output,
                                  species_iri="http://www.theworldavatar.com/kg/ontospecies/Species_8270caad-fec5-4331-9243-fb4e37edc10e")
        print(my_invoker.result)
