import json
import os
import pickle
import time
import sys
import traceback
from datetime import datetime as dt
sys.path.append("../..")
import torch
from transformers import BertTokenizer
from Marie.SubgraphExtraction.SubgraphExtractor import SubgraphExtractor
from Marie.Util.Models.TransEScoreModel import TransEScoreModel
from Marie.Util.location import DATA_DIR
from Marie.EntityLinking.ChemicalNEL import ChemicalNEL
from Marie.Util.Logging import MarieLogger


class PubChemEngine:

    def __init__(self):
        self.dataset_dir = "CrossGraph/pubchem"
        self.subgraph_extractor = SubgraphExtractor(dataset_name='pubchem10000')
        self.chemical_nel = ChemicalNEL(dataset_name="pubchem")
        self.marie_logger = MarieLogger()
        self.marie_logger.info("6. Basic NEL test is running")
        if self.chemical_nel.find_cid("CO2")[1] == "CID280":
            self.marie_logger.info("7. Basic NEL test is passed")
        else:
            self.marie_logger.critical(" ERROR: 7. Basic NEL test failed")
        self.marie_logger.info("8. Done initializing NEL ")
        '''Find the device available for running the model'''
        # use_cuda = torch.cuda.is_available()
        # self.device = torch.device("cuda" if use_cuda else "cpu")
        self.device = torch.device("cpu")
        '''Load pickles for idx - label and label - idx transformation '''
        i2e_file = open(os.path.join(DATA_DIR, self.dataset_dir, 'idx2entity.pkl'), 'rb')
        self.idx2entity = pickle.load(i2e_file)
        e2i_file = open(os.path.join(DATA_DIR, self.dataset_dir, 'entity2idx.pkl'), 'rb')
        self.entity2idx = pickle.load(e2i_file)

        self.marie_logger.info(f'9. Hosting device is using: {self.device} ===============')
        '''Initialize the scoring model'''
        self.score_model_name = 'bert_embedding_10000'
        self.score_model = TransEScoreModel(device=self.device, model_name=self.score_model_name,
                                            dataset_dir=self.dataset_dir, dim=20)
        self.score_model = self.score_model.to(self.device)
        '''Initialize tokenizer'''
        self.tokenizer = BertTokenizer.from_pretrained('bert-base-cased')
        self.max_length = 12
        self.confidence_factor = 2
        self.marie_logger.info("10. Done initializing Scoring model ")

    def test_entity_linking(self):
        test_list = ['CO2', 'carbon dioxide', 'benzene']
        for target in test_list:
            rst = self.chemical_nel.find_cid(target)
            if rst is None:
                return "<div  style='color:red;'>Entity linking is NOT functioning</div>"
        return "<div style='color:green;'>Entity linking is functioning</div>"

    def test_score_model(self):
        test_list = [('molar mass', 'CID1'), ('iupac name', 'CID2')]
        for q, h in test_list:
            rst = self.find_answers(question=q, head_entity=h)
            if rst is None:
                return f"<div style='color:red;'>Score model is NOT functioning {q, h}</div>"
        return "<div style='color:green;'>Score model is functioning</div>"

    def test_value_lookup(self):
        test_list = ['CID21_iupac_name', 'CID3106_count_hydrogen_bond_acceptor']
        for n in test_list:
            rst = self.value_lookup(n)
            if rst is None:
                return f"<div style='color:red;'>Value lookup is NOT functioning for {n}</div>"
        return "<div style='color:green;'>Value lookup is functioning</div>"

    def value_lookup(self, node_name):
        return self.subgraph_extractor.value_lookup(node_name=node_name)

    def prepare_prediction_batch(self, question, head_entity, candidate_entities):
        """
        :param question: question in text
        :param head_entity: head entity index
        :param candidate_entities: list of candidate entity index
        :return: Ranked list of candidate entities
        """
        self.marie_logger.info(f" - Preparing prediction batch")
        candidate_entities = torch.LongTensor(candidate_entities).to(self.device)
        self.marie_logger.info(f" - Candidate entities: {candidate_entities}")
        repeat_num = len(candidate_entities)
        tokenized_question_batch = self.tokenize_question(question, repeat_num)
        self.marie_logger.info(f" - Question tokenized {question}")
        head_entity_batch = torch.LongTensor([head_entity]).repeat(repeat_num).to(self.device)
        self.marie_logger.info(f" - Head entity index {head_entity}")
        prediction_batch = {'question': tokenized_question_batch, 'e_h': head_entity_batch, 'e_t': candidate_entities}
        self.marie_logger.info(f" - Prediction batch is prepared")
        return prediction_batch

    def tokenize_question(self, question, repeat_num):
        """
        :param question: question in text
        :param repeat_num:
        :return:
        """
        tokenized_question = self.tokenizer(question,
                                            padding='max_length', max_length=self.max_length, truncation=True,
                                            return_tensors="pt")
        attention_mask, input_ids = tokenized_question['attention_mask'], tokenized_question['input_ids']
        attention_mask_batch = attention_mask.repeat(repeat_num, 1).to(self.device)
        input_ids_batch = input_ids.repeat(repeat_num, 1).to(self.device)
        return {'attention_mask': attention_mask_batch, 'input_ids': input_ids_batch}

    def find_answers(self, question: str, head_entity: str, head_name : str, k=5):
        """
        :param head_name:
        :param question: question in string format
        :param head_entity: the CID string for the head entity
        :return: score of all candidate answers
        """
        try:
            self.marie_logger.info(f" received question: {question}")
            candidates = self.subgraph_extractor.retrieve_subgraph(head_entity)
            self.marie_logger.info(f" candidates: {candidates}")
            pred_batch = self.prepare_prediction_batch(question, self.entity2idx[head_entity], candidates)
            scores = self.score_model.predict(pred_batch).cpu()
            self.marie_logger.info(f" prediction scores {scores}")
            _, indices_top_k = torch.topk(scores, k=k, largest=False)
            labels_top_k = [self.idx2entity[candidates[index]] for index in indices_top_k]
            scores_top_k = [scores[index].item() for index in indices_top_k]
            targets_top_k = [head_name] * len(scores)
            return labels_top_k, scores_top_k, targets_top_k
        except:
            self.marie_logger.error('The attempt to find answer failed')
            self.marie_logger.error(traceback.format_exc())
            return traceback.format_exc()

    def extract_head_ent(self, _question):
        self.marie_logger.info("extracting head entity")
        return self.chemical_nel.find_cid(question=_question)

    def process_answer(self, answer_list, nel_confidence, mention_string, name, score_list):
        result_list = []
        self.marie_logger.info(f'=========== processing candidate answers ==============')
        for answer, score in zip(answer_list, score_list):
            self.marie_logger.info(f'The answer:\t\t {answer}')
            self.marie_logger.info(f'NEL confidence:\t\t  {nel_confidence}')
            self.marie_logger.info(f'Mentioned string:\t\t {mention_string}')
            self.marie_logger.info(f'Name in Dictionary:\t\t {name}')
            combined_confidence = round(min(1, nel_confidence * (1 / score) * self.confidence_factor), 2)
            self.marie_logger.info(f'Confidence:\t\t {combined_confidence}')
            self.marie_logger.info(f'-------------------------')
            row = {"answer": self.value_lookup(answer), "from node": answer, "mention": mention_string,
                   "name": name, "confidence": combined_confidence}
            result_list.append(row)
        return result_list

    def run(self, question):
        """
        :param question:
        :return:
        """
        # get the mention,
        try:
            nel_confidence, cid, mention_string, name = self.extract_head_ent(question)
        except TypeError:
            self.marie_logger.error(f"Error - Could not recognise any target from the question: "
                                    f"{question} from {__name__}.{self.run.__name__}")

            return {"Error": "No target can be recognised from this question"}

        question = self.remove_head_entity(question, mention_string)
        answer_list, score_list, target_list = self.find_answers(question=question, head_entity=cid, head_name=name)
        max_score = max(score_list)
        score_list = [(max_score + 1 - s) for s in score_list]
        return answer_list, score_list, target_list
        # return self.process_answer(answer_list, nel_confidence, mention_string, name, score_list)

    def remove_head_entity(self, _question, _head_entity):
        return _question.replace(_head_entity, '').strip()

    def self_inspection(self):
        self.marie_logger.info("Running self inspection")
        wrong_questions = []
        test_questions = json.loads(open(os.path.join(DATA_DIR, 'Test/test_question_and_answers.json')).read())
        for question, target_answer in test_questions.items():
            answer = self.run(question=question)[0]
            self.marie_logger.info(answer)
            self.marie_logger.info('=========')
            self.marie_logger.info(target_answer)
            del target_answer["confidence"]
            del answer["confidence"]
            if answer != target_answer:
                tmp = {"question": question, "answer": answer, "target_answer": target_answer}
                wrong_questions.append(json.dumps(tmp, indent=4))

        if len(wrong_questions) == 0:
            return "<h2>The test is fully passed!</h2> <br/>" + str(dt.now())
        else:
            return f"<h2>The test failed! {len(wrong_questions)} out of {len(test_questions)} </h2> <br/> The failed questions are: <br/>" + "<hr/><br/> - ".join(
                wrong_questions)


if __name__ == '__main__':
    my_pubchem_engine = PubChemEngine()
    START_TIME = time.time()
    question = 'what is the exact mass of C6H6'
    answer = my_pubchem_engine.run(question)
    print(answer)
    print(time.time() - START_TIME)
