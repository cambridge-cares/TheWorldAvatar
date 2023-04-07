# the root interface for agent calling ... 


# AgentInterface(question) -> agent results 

# agent interface: ChemicalNEL(question) -> quailfier, species_iri 
# agent matcher : question -> relation prediction + similarity calculation -> agent_iri/agent_name and requested output -> agent interface
# agent invoker : agent_iri, qualifer, species_iri, requested output -> agent results after filtering (in case of thermo agent) -> agent inferface -> CrossGraphQAEngine 

import sys

sys.path.append("")
from Marie.Util.AgentTools.agent_invoker import AgentInvoker
from Marie.Util.AgentTools.question_agent_matcher import QuestionAgentMatcher


class AgentInterface():

    def __init__(self, question, tokenizer_name="bert-base-uncased"):
        self.question = question
        self.tokenizer_name = tokenizer_name
        # self.species_iri, self.qualifier = ChemicalNEL(question)

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
