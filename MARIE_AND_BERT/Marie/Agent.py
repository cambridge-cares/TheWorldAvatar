# the root interface for agent calling ... 


# AgentInterface(question) -> agent results 

# agent interface: ChemicalNEL(question) -> quailfier, species_iri 
# agent matcher : question -> relation prediction + similarity calculation -> agent_iri/agent_name and requested output -> agent interface
# agent invoker : agent_iri, qualifer, species_iri, requested output -> agent results after filtering (in case of thermo agent) -> agent inferface -> CrossGraphQAEngine
import os
import sys

from Marie.EntityLinking.ChemicalNEL import ChemicalNEL
from Marie.Util.CommonTools import NumericalTools

sys.path.append("")
from Marie.Util.AgentTools.agent_invoker import AgentInvoker
from Marie.Util.AgentTools.question_agent_matcher import QuestionAgentMatcher


class AgentInterface():

    def __init__(self, question, tokenizer_name="bert-base-uncased"):
        self.question = question
        self.tokenizer_name = tokenizer_name
        self.pce_nel = ChemicalNEL(dataset_name=os.path.join("ontoagent", "pceagent"), enable_class_ner=False)
        self.thermo_nel = ChemicalNEL(dataset_name=os.path.join("ontoagent", "thermoagent"), enable_class_ner=False)

        self.nel_dict = {"thermoagent": self.thermo_nel, "pceagent": self.pce_nel}

        # self.species_iri, self.qualifier = ChemicalNEL(question)

    def parse_species_iri_and_qualifier(self, question, agent_name):
        # e.g. what is the pce of TiO2
        # e.g. what is the heat capacity of benzene at 100 degrees
        # e.g. thermal properties of CH4
        nel = self.nel_dict[agent_name]
        mention = nel.get_mention(question=question)
        species_iri = nel.find_cid(mention)[1]
        qualifiers = NumericalTools.qualifier_value_extractor(question)
        return species_iri, qualifiers

    def run(self):
        # Perform question-agent matching
        my_matcher = QuestionAgentMatcher(question=self.question, tokenizer_name=self.tokenizer_name)
        # The agent to be called and the output requested from that agent
        agent, output = my_matcher.run()

        # Invoke the agent found
        if (agent != None):
            my_invoker = AgentInvoker(agent=agent, output=output,
                                      species_iri="http://www.theworldavatar.com/kb/ontospecies/Species_2fb70cfe-6707-4d6c-b977-bae913c4878f")
            if my_invoker.result is None:
                print("Agent cannot be invoked")
                return None
            else:
                print(my_invoker.result)
                return my_invoker.result
        else:
            return None


if __name__ == "__main__":
    question = "entropy"
    agentInterface = AgentInterface(question)
    agentInterface.run()
