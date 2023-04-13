# the root interface for agent calling ... 


# AgentInterface(question) -> agent results 

# agent interface: ChemicalNEL(question) -> quailfier, species_iri 
# agent matcher : question -> relation prediction + similarity calculation -> agent_iri/agent_name and requested output -> agent interface
# agent invoker : agent_iri, qualifer, species_iri, requested output -> agent results after filtering (in case of thermo agent) -> agent inferface -> CrossGraphQAEngine
import os
import sys

sys.path.append("")
from Marie.EntityLinking.ChemicalNEL import ChemicalNEL
from Marie.Util.CommonTools import NumericalTools
from Marie.Util.AgentTools.agent_invoker import AgentInvoker
from Marie.Util.AgentTools.question_agent_matcher import QuestionAgentMatcher


class AgentInterface():

    def __init__(self, tokenizer_name="bert-base-uncased"):
        self.tokenizer_name = tokenizer_name
        self.pce_nel = ChemicalNEL(dataset_name=os.path.join("ontoagent", "pceagent"), enable_class_ner=False)
        self.thermo_nel = ChemicalNEL(dataset_name=os.path.join("ontoagent", "thermoagent"), enable_class_ner=False)
        self.nel_dict = {"ontothermoagent": self.thermo_nel, "ontopceagent": self.pce_nel}

    def parse_species_iri_and_qualifier(self, question, agent_name):
        nel = self.nel_dict[agent_name]
        mention = nel.get_mention(question=question)
        species_iri = nel.find_cid(mention)[1]
        return species_iri

    def run(self, question):
        # perform qualifier extraction and removal
        qualifier, filtered_question = NumericalTools.qualifier_value_extractor(question)
        print("filtered question", filtered_question)
        # Perform question-agent matching
        my_matcher = QuestionAgentMatcher(question=filtered_question, tokenizer_name=self.tokenizer_name)
        # The agent to be called and the output requested from that agent
        agent, output = my_matcher.run()

        # Invoke the agent found
        if (agent != None):
            species_iri = self.parse_species_iri_and_qualifier(filtered_question, agent)
            my_invoker = AgentInvoker(agent=agent, output=output, species_iri=species_iri, qualifier=qualifier)
            if my_invoker.result is None:
                print("Agent cannot be invoked")
                return None
            else:
                return my_invoker.result
        else:
            return None


if __name__ == "__main__":
    question = "what is the enthalpy of C3H4O"
    agentInterface = AgentInterface()
    result = agentInterface.run(question)
    print(result)
