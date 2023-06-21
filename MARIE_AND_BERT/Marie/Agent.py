# the root interface for agent calling ... 


# AgentInterface(question) -> agent results 

# agent interface: ChemicalNEL(question) -> quailfier, species_iri 
# agent matcher : question -> relation prediction + similarity calculation -> agent_iri/agent_name and requested output -> agent interface
# agent invoker : agent_iri, qualifer, species_iri, requested output -> agent results after filtering (in case of thermo agent) -> agent inferface -> CrossGraphQAEngine
import json
import os
import sys
import time

sys.path.append("")

from Marie.EntityLinking.IRILookup import IRILookup
from Marie.Util.CommonTools import NumericalTools
from Marie.Util.AgentTools.agent_invoker import AgentInvoker
from Marie.Util.AgentTools.question_agent_matcher import QuestionAgentMatcher
from Marie.EntityLinking.ChemicalNEL import ChemicalNEL


class AgentInterface:

    def __init__(self, tokenizer_name="bert-base-uncased", nel=None):
        self.tokenizer_name = tokenizer_name
        self.nel = nel
        self.my_matcher = QuestionAgentMatcher(tokenizer_name=self.tokenizer_name)
        self.pce_nel = IRILookup(dataset_name=os.path.join("ontoagent", "pceagent"), enable_class_ner=False, nel=nel)
        self.thermo_nel = IRILookup(dataset_name=os.path.join("ontoagent", "thermoagent"), enable_class_ner=False,
                                    nel=nel)
        self.nel_dict = {"ontothermoagent": self.thermo_nel, "ontopceagent": self.pce_nel}
        self.available_agents = list(set(self.nel_dict.keys()))
        self.stop_words = ["what", "are", "is", "find", "all", "the"]
        self.agent_invoker = AgentInvoker()

    def text_filtering(self, question):
        tokens = question.strip().split(" ")
        tokens = [t for t in tokens if t.lower() not in self.stop_words]
        return " ".join(tokens)

    def parse_species_iri_and_qualifier(self, mention, agent_name):
        print("========================= parse species iri and qualifier =======================")
        print("agent name", agent_name)
        agent_nel = self.nel_dict[agent_name]
        species_iri = agent_nel.find_cid(mention)[1]
        print("species iri", species_iri)
        return species_iri

    def run(self, question, mention=None):
        # perform qualifier extraction and removal
        question = self.text_filtering(question).lower()
        qualifier, filtered_question = NumericalTools.qualifier_value_extractor(question)
        mention = self.pce_nel.get_mention(filtered_question)
        print("mention:", mention)

        if mention:
            filtered_question = filtered_question.replace(mention, "")

        # Perform question-agent matching
        # The agent to be called and the output requested from that agent
        agent, output = self.my_matcher.run(question=filtered_question)
        species_iri = self.parse_species_iri_and_qualifier(mention=mention, agent_name=agent)

        # Invoke the agent found
        if (agent != None) and agent in self.available_agents:
            # my_invoker = AgentInvoker(agent=agent, output=output, species_iri=species_iri, qualifier=qualifier)
            result = self.agent_invoker.run(agent=agent, output=output, species_iri=species_iri, qualifier=qualifier)
            if result is None:
                print("Agent cannot be invoked")
                return ["EMPTY"], [-999], ["EMPTY"]
            else:
                # return my_invoker.result
                return [json.dumps(result)], [1.0], [agent]
        else:
            return ["EMPTY"], [-999], ["EMPTY"]


if __name__ == "__main__":
  
    cn = ChemicalNEL()
    agentInterface = AgentInterface(nel=cn)
    START_TIME = time.time()

    text = ""
    while text != "quit":
        question = input("Question:")
        result = agentInterface.run(question)
        print(result)
        print(time.time() - START_TIME)


    # Example Question: "what is the Heat Capacity of C3H4O"