import pandas as pd
import os
from Marie.PubChem import PubChemEngine
from Marie.OntoChemistry import OntoChemistryEngine
from Marie.Util.location import DATA_DIR


class CrossGraphDatasetCreator:

    def __init__(self):
        self.pubchem_engine = PubChemEngine()
        self.ontochemistry_engine = OntoChemistryEngine()

    def get_answsers(self, question: str, head: str, domain: int):
        """
        Get the answers from all engines, extract the list of top-k answers with scores
        :param question: question without the head entity in string
        :param head: head IRI in string
        :return: answer sets from both/all engines
        """
        head_onto = '1b206169-7539-3491-85ec-a40dfe351a2a'
        head_pubchem = 'CID1'
        if domain == 0:  # this is a pubchem answer, create a fake ontochemistry head
            head_pubchem = head
        else:
            head_onto = head

        pubchem_answers, pubchem_scores = self.pubchem_engine.find_answers(question=question, head_entity=head_pubchem)
        pubchem_scores = [10 - s for s in pubchem_scores]

        onto_answers, onto_scores = self.ontochemistry_engine.run(question=question, head_entity=head_onto)
        return pubchem_answers, pubchem_scores, onto_answers, onto_scores

    def create_question_set(self):
        df_pubchem = pd.read_csv(os.path.join(DATA_DIR, 'CrossGraph', 'pubchem_cross_score.tsv'), sep='\t', index_col=0)
        df_onto = pd.read_csv(os.path.join(DATA_DIR, 'CrossGraph', 'ontochemistry_cross_score.tsv'), sep='\t',
                              index_col=0)
        # sample 110 from df_onto
        df_onto = df_onto.sample(n=110)
        df_onto = df_onto.reset_index(drop=True)

        df = pd.concat([df_onto, df_pubchem])
        df = df.reset_index(drop=True)
        df.to_csv(os.path.join(DATA_DIR, 'CrossGraph', 'all_cross_score.tsv'), sep='\t')

    """
    Dataset: (question, head ,domain, (true_answer,domain_code) , [fake_answer, domain_code]))
    """

    def create_cross_training_set(self):
        df_cross = pd.read_csv(os.path.join(DATA_DIR, 'CrossGraph', 'all_cross_score.tsv'), sep='\t', index_col=0)
        row_list = []
        for idx, row in df_cross.iterrows():
            question, head, domain, answer = row
            domain = int(domain)
            pubchem_answers, pubchem_scores, onto_answers, onto_scores = \
                self.get_answsers(question=question, head=head, domain=domain)

            all_answers = pubchem_answers + onto_answers
            """
            # situation 1: only one engine gives the true answer ... DO: FN set contains only scores from the other 
            # situation 2: both engines give the true answer ... DO: create two pairs, make sure each answer outranks FN from the other set  
            # situation 3: none of the engines gives the true answer ... DO: ignore this case 
            """
            if answer not in all_answers:  # situtation 3
                print("-----------------")
                print("question", question)
                print("answer", answer)
                print("all answers", all_answers)
                print("None of the engines returned the true answer, abort the mission")
            else:
                print("Situation 2,3")
                true_domain = None
                fake_domain = None

                if domain == 0:  # pubchem
                    fake_score = [onto_scores[onto_answers.index(o_a)] for o_a in onto_answers]
                    true_score = [pubchem_scores[pubchem_answers.index(answer)] for i in range(len(fake_score))]
                    true_domain = 0
                    fake_domain = 1
                else:  # ontochemistry
                    fake_score = [pubchem_scores[pubchem_answers.index(p_a)] for p_a in pubchem_answers]
                    true_score = [onto_scores[onto_answers.index(answer)] for i in range(len(fake_score))]
                    true_domain = 1
                    fake_domain = 0

                true_domain_list = [true_domain for i in range(len(fake_score))]
                fake_domain_list = [fake_domain for i in range(len(fake_score))]
                training_pair = (question, head, true_score, true_domain_list, fake_score, fake_domain_list)
                row_list.append(training_pair)

        df = pd.DataFrame(row_list)
        df.columns = ['question', 'head', 'true_score', 'true_domain', 'fake_score', 'fake_domain']
        df.to_csv(os.path.join(DATA_DIR, 'CrossGraph', 'cross_graph_pairs.tsv'), sep='\t')

#
# answers, scores = pubchem_engine.find_answers(question='charge', head_entity='CID1')
# scores = [10 - s for s in scores]
# print(answers)
# print(scores)
#
# answers, scores = ontochemistry_engine.run(question='charge', head_entity='1b206169-7539-3491-85ec-a40dfe351a2a')
# print(answers)
# print(scores)
if __name__ == '__main__':
    my_dataset_creator = CrossGraphDatasetCreator()
    my_dataset_creator.create_cross_training_set()
