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
        samples_size = min(len(df_pubchem), len(df_onto))

        df_onto = df_onto.sample(n=samples_size)
        df_onto = df_onto.reset_index(drop=True)

        df_pubchem = df_pubchem.sample(n=samples_size)
        df_pubchem = df_pubchem.reset_index(drop=True)

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
            if answer not in all_answers:  # situation 3
                print("-----------------")
                print("question", question)
                print("answer", answer)
                print("all answers", all_answers)
                print("None of the engines returned the true answer, abort the mission")
            else:
                print("Situation 2,3")

                # normalize the scores, average or max ...
                # avg = sum(pubchem_scores) / len(pubchem_scores)
                avg = max(pubchem_scores)
                pubchem_scores = [p_s / avg for p_s in pubchem_scores]

                avg = max(onto_scores)
                onto_scores = [o_s / avg for o_s in onto_scores]

                if domain == 0:  # pubchem
                    fake_score = [onto_scores[onto_answers.index(o_a)] for o_a in onto_answers]
                    true_score = [pubchem_scores[pubchem_answers.index(answer)] for i in range(len(fake_score))]
                else:  # ontochemistry
                    fake_score = [pubchem_scores[pubchem_answers.index(p_a)] for p_a in pubchem_answers]
                    true_score = [onto_scores[onto_answers.index(answer)] for i in range(len(fake_score))]

                true_domain = domain
                fake_domain = 1 - domain

                for true_score, fake_score in zip(true_score, fake_score):
                    training_pair = (question, head, true_score, true_domain, fake_score, fake_domain)
                    row_list.append(training_pair)

        df = pd.DataFrame(row_list)
        df.columns = ['question', 'head', 'true_score', 'true_domain', 'fake_score', 'fake_domain']
        df.to_csv(os.path.join(DATA_DIR, 'CrossGraph', 'cross_graph_pairs.tsv'), sep='\t')


if __name__ == '__main__':
    my_dataset_creator = CrossGraphDatasetCreator()
    my_dataset_creator.create_question_set()
    my_dataset_creator.create_cross_training_set()
