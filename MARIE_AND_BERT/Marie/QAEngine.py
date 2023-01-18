import os
import traceback
import torch

from Marie.Util.CommonTools.NLPTools import NLPTools
from Marie.Util.NHopExtractor import HopExtractor
from Marie.EntityLinking.ChemicalNEL import ChemicalNEL
from Marie.Util.Logging import MarieLogger
from Marie.Util.Models.TransEScoreModel import TransEScoreModel
from Marie.Util.location import DATA_DIR
from Marie.Util.CommonTools.FileLoader import FileLoader


class QAEngine:
    def __init__(self, dataset_dir, dataset_name, embedding="transe", dim=20, dict_type="json"):
        self.marie_logger = MarieLogger()
        self.model_name = f"bert_{dataset_name}"
        self.dataset_dir = dataset_dir
        self.dataset_name = dataset_name
        self.file_loader = FileLoader(full_dataset_dir=os.path.join(DATA_DIR, self.dataset_dir),
                                      dataset_name=self.dataset_name)
        self.subgraph_extractor = HopExtractor(dataset_dir=self.dataset_dir, dataset_name=self.dataset_name)
        self.chemical_nel = ChemicalNEL(dataset_name=self.dataset_name)
        self.device = torch.device("cpu")
        '''Load pickles for idx - label and label - idx transformation '''
        # ============================= Load files ======================================================
        self.entity2idx, self.idx2entity, self.rel2idx, self.idx2rel = self.file_loader.load_index_files()
        self.value_dictionary = self.file_loader.load_value_dict(dict_type=dict_type)
        # ===============================================================================================
        if embedding == "transe":
            self.score_model = TransEScoreModel(device=self.device, model_name=self.model_name,
                                                dataset_dir=self.dataset_dir, dim=dim)
            self.score_model = self.score_model.to(self.device)

        '''Initialize tokenizer'''
        self.nlp = NLPTools()
        # self.tokenizer = BertTokenizer.from_pretrained('bert-base-cased')
        # self.max_length = 12

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
        tokenized_question_batch = self.nlp.tokenize_question(question, repeat_num)
        self.marie_logger.info(f" - Question tokenized {question}")
        head_entity_batch = torch.LongTensor([head_entity]).repeat(repeat_num).to(self.device)
        self.marie_logger.info(f" - Head entity index {head_entity}")
        prediction_batch = {'question': tokenized_question_batch, 'e_h': head_entity_batch, 'e_t': candidate_entities}
        self.marie_logger.info(f" - Prediction batch is prepared")
        return prediction_batch

    # def tokenize_question(self, question, repeat_num):
    #     """
    #     :param question: question in text
    #     :param repeat_num:
    #     :return:
    #     """
    #     tokenized_question = self.nlp.tokenizer(question,
    #                                         padding='max_length', max_length=self.max_length, truncation=True,
    #                                         return_tensors="pt")
    #     attention_mask, input_ids = tokenized_question['attention_mask'], tokenized_question['input_ids']
    #     attention_mask_batch = attention_mask.repeat(repeat_num, 1).to(self.device)
    #     input_ids_batch = input_ids.repeat(repeat_num, 1).to(self.device)
    #     return {'attention_mask': attention_mask_batch, 'input_ids': input_ids_batch}

    def find_answers(self, question: str, head_entity: str, head_name: str, k=5):
        """
        :param head_name: the standard name of the head entity
        :param k: number of results to return
        :param question: question in string format
        :param head_entity: the CID string for the head entity
        :return: score of all candidate answers
        """
        # question = question.replace("'s ", " # ")

        try:
            self.marie_logger.info(f" received question: {question}")
            candidates = self.subgraph_extractor.extract_neighbour_from_idx(self.entity2idx[head_entity])
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
            # return traceback.format_exc()
            return ["EMPTY"], [-999], ["EMPTY"]

    def extract_head_ent(self, mention):
        self.marie_logger.info(f"extracting head entity: {mention}")
        return self.chemical_nel.find_cid(mention=mention)

    def value_lookup(self, node):
        if node in self.value_dictionary:
            return self.value_dictionary[node]
        else:
            return "NODE HAS NO VALUE"

    def process_answer(self, answer_list, nel_confidence, mention_string, name, score_list):
        result_list = []
        self.marie_logger.info(f'=========== processing candidate answers ==============')
        for answer, score in zip(answer_list, score_list):
            self.marie_logger.info(f'The answer:\t\t {answer}')
            self.marie_logger.info(f'NEL confidence:\t\t  {nel_confidence}')
            self.marie_logger.info(f'Mentioned string:\t\t {mention_string}')
            self.marie_logger.info(f'Name in Dictionary:\t\t {name}')
            combined_confidence = round(min(1, nel_confidence * (1 / score)), 2)
            self.marie_logger.info(f'Confidence:\t\t {combined_confidence}')
            self.marie_logger.info(f'-------------------------')
            row = {"answer": answer, "from node": answer, "mention": mention_string,
                   "name": name, "confidence": combined_confidence}
            result_list.append(row)
        return result_list

    def run(self, question, head=None, mention=None):
        """
        :param mention:
        :param head: directly give a head for testing and evaluation purpose.
        :param question:
        :return:
        """
        if mention is None:
            mention = self.chemical_nel.get_mention(question=question)
        try:
            nel_confidence, cid, mention_string, name = self.extract_head_ent(mention)
            if head is not None:
                cid = head
        except TypeError:
            self.marie_logger.error(f"Error - Could not recognise any target from the question: "
                                    f"{question} from {__name__}.{self.run.__name__}")

            # return {"Error": "No target can be recognised from this question"}
            return ["EMPTY"], [-999], ["EMPTY"]

        try:
            question = self.nlp.remove_head_entity(question, mention_string).lower()
        except AttributeError:
            return ["EMPTY"], [-999], ["EMPTY"]

        self.marie_logger.info(f"Question with head entity ({mention_string}) removed {question}")
        answer_list, score_list, target_list = self.find_answers(question=question, head_entity=cid, head_name=name)
        max_score = max(score_list)
        score_list = [(max_score + 1 - s) for s in score_list]
        return answer_list, score_list, target_list
        # return self.process_answer(answer_list, nel_confidence, mention_string, name, score_list)
