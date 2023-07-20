import json
import os, sys
import random

from Marie.EntityLinking.IRILookup import IRILookup

sys.path.append("..")
from torch import no_grad
import torch
from Marie.Util.NHopExtractor import HopExtractor
from Marie.Util.Logging import MarieLogger
from Marie.Util.Models.TransEAScoreModel import TransEAScoreModel
from Marie.Util.CommonTools import NumericalTools
from Marie.Util.CommonTools.NLPTools import NLPTools
from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.location import DATA_DIR


class InputDict:

    def __init__(self):
        self.question, self.target, self.instance_list, self.mention = None, None, None, None
        self.single_question_embedding, self.pred_idx, self.numerical_operator = None, None, None
        self.instance_label = None
        self.numerical_string = ""
        self.numerical_value = None


class TestData:
    def __init__(self):
        self.true_head, self.true_rel, self.true_tail_list, self.true_mention = None, None, None, None


class QAEngineNumerical:

    def text_filtering(self, question):
        # question = question.replace("(", "").replace(")", "")
        stop_words = ["find", "all", "species", "what", "is", "the",
                      "show", "give", "that", "can", "be", "which",
                      "list", "are", "shaped", "shape", "created", "give", "type", "types"]
        stop_words_units = ["g/mol", "dalton", "degrees", "degree", "celsius", "g", "mn/m"]
        stop_words += stop_words_units
        question_tokens = [token for token in question.split(" ") if token.lower() not in stop_words]
        question = " ".join(question_tokens)
        return question

    def __init__(self, dataset_dir, dataset_name, nel=None, embedding="transe", dim=20, dict_type="json", largest=False,
                 test=False, operator_dict=None, value_dict_name="wikidata_numerical_value_new.json",
                 seperated_model=False, mode="transe", enable_class_ner=False, ontology=None, operator_dim=3,
                 numerical_scale_factor=1.0, bert_tokenizer_name="bert-base-cased"):
        self.input_dict = InputDict()
        self.test_dict = TestData()
        self.mode = mode
        self.numerical_scale_factor = numerical_scale_factor
        self.enable_class_ner = enable_class_ner
        self.marie_logger = MarieLogger()
        self.test = test
        self.seperated_model = seperated_model
        self.largest = largest
        self.dataset_dir = dataset_dir
        self.dataset_name = dataset_name
        self.ontology = ontology
        self.model_name = f"bert_{self.dataset_name}"
        self.subgraph_extractor_numerical = HopExtractor(dataset_dir=self.dataset_dir, dataset_name=self.dataset_name,
                                                         numerical=True)
        self.subgraph_extractor = HopExtractor(dataset_dir=self.dataset_dir, dataset_name=self.dataset_name,
                                               numerical=False)
        self.device = torch.device("cpu")
        # ==================================== load all the files needed ===============================
        self.file_loader = FileLoader(full_dataset_dir=os.path.join(DATA_DIR, self.dataset_dir),
                                      dataset_name=self.dataset_name)
        self.entity2idx, self.idx2entity, self.rel2idx, self.idx2rel = self.file_loader.load_index_files()
        self.all_heads_indices = self.file_loader.load_all_heads_tensor()
        self.triples = self.file_loader.load_all_triples()
        self.value_dictionary = self.file_loader.load_value_dict(dict_type=dict_type,
                                                                 file_name=value_dict_name)
        # ===============================================================================================
        self.operator_dict = operator_dict
        self.operator_dim = len(self.operator_dict)
        if nel is None:
            from Marie.EntityLinking.ChemicalNEL import ChemicalNEL
            nel = ChemicalNEL()

        if self.enable_class_ner:
            self.nel = IRILookup(dataset_name=ontology, enable_class_ner=enable_class_ner, nel=nel)
        else:
            self.nel = IRILookup(dataset_name=dataset_name, enable_class_ner=enable_class_ner, nel=nel)
        self.nlp = NLPTools(tokenizer_name=bert_tokenizer_name)
        self.o_p_dict = FileLoader.create_o_p_dict(self.triples)
        if embedding == "transe":
            self.score_model = TransEAScoreModel(device=self.device,
                                                 dataset_dir=self.dataset_dir, dim=dim, output_dim=self.operator_dim)
            self.score_model = self.score_model.to(self.device)
            if self.seperated_model:
                self.operator_model = TransEAScoreModel(device=self.device,
                                                        dataset_dir=self.dataset_dir, dim=dim,
                                                        output_dim=self.operator_dim)
                self.operator_model.load_model(f"bert_{self.dataset_name}_operator")
                self.score_model.load_model(f"bert_{self.dataset_name}_predict")

            else:
                self.score_model.load_model(f"bert_{self.dataset_name}")

        '''Initialize tokenizer'''

    def get_numerical_operator(self, tokenized_question, single_question_embedding):
        # ====================================== get numerical operator ====================================
        if self.seperated_model:
            single_question_embedding_operator = self.operator_model.get_question_embedding(question=tokenized_question)
            numerical_operator = self.operator_model.get_numerical_operator(
                single_question_embedding_operator)
            self.input_dict.numerical_operator = numerical_operator
        else:
            numerical_operator = self.score_model.get_numerical_operator(single_question_embedding)
        predicted_operator_idx = torch.argmax(numerical_operator).item()
        numerical_operator = self.operator_dict[predicted_operator_idx]
        self.input_dict.numerical_operator = numerical_operator

    def value_lookup(self, node):
        if node in self.value_dictionary:
            return self.value_dictionary[node]
        else:
            # return "NODE HAS NO VALUE"
            return node

    def prepare_prediction_batch_numerical(self, heads, question_embedding):
        """
        :param question_embedding:
        :param heads:
        :return: the rel embedding predicted, the attr embedding predicted, the numerical operator
        """
        heads_batch = []
        tails_batch = []
        split_indices = []

        for head in heads:
            # find the neighbours of this head
            # append the neighbours to tail_batch
            neighbours = self.subgraph_extractor.extract_neighbour_from_idx(head)
            # neighbours = self.subgraph_extractor_numerical.extract_neighbour_from_idx(head)
            heads_batch += [head] * len(neighbours)
            tails_batch += neighbours
            split_indices.append(len(tails_batch))
        self.marie_logger.info(f" - Preparing prediction batch")
        prediction_batch = {'e_h': torch.LongTensor(heads_batch),
                            'e_t': torch.LongTensor(tails_batch), 'single_question': question_embedding}
        self.marie_logger.info(f" - Prediction batch is prepared")

        # tail_labels = [self.idx2entity[t] for t in tails_batch]
        # head_labels = [self.idx2entity[h] for h in heads_batch]

        return prediction_batch, torch.LongTensor(tails_batch), split_indices[:-1], torch.LongTensor(heads_batch)

    def filter_heads_by_numerical(self):
        """
        :return: heads that fulfill the numerical condition given
        """
        if self.enable_class_ner:
            try:
                heads = self.all_heads_indices[self.input_dict.target]
            except:
                heads = self.all_heads_indices
        else:
            heads = self.all_heads_indices

        # if len(heads) > 50:
        #     heads = random.sample(heads, 50)
        predicted_attr = self.score_model.get_attribute_prediction(self.input_dict.single_question_embedding)
        attr_batch = predicted_attr.repeat(len(heads), 1)
        numerical_values, _ = self.score_model.get_numerical_prediction(heads, attr_batch)
        numerical_values = numerical_values * self.numerical_scale_factor
        if self.input_dict.numerical_operator == "larger":
            indices = (numerical_values > self.input_dict.numerical_value)
            indices = indices.nonzero().squeeze(1)
        elif self.input_dict.numerical_operator == "smaller":
            indices = (numerical_values < self.input_dict.numerical_value)
            indices = indices.nonzero().squeeze(1)
        elif self.input_dict.numerical_operator == "about":
            indices = torch.abs(numerical_values - self.input_dict.numerical_value)
            # _, indices = torch.topk(indices, k=min(10, len(indices)), largest=False)
            _, indices = torch.topk(indices, k=len(indices), largest=False)
        else:
            return []
        # select the heads using the indices
        # if len(indices) > 20:
        #     indices = indices[0:20]

        heads = heads[indices]
        return heads.tolist(), numerical_values[indices].tolist()

    def prepare_prediction_batch(self, head, question_embedding, pred_p_label):
        """
        :param head: single IRI of the head entity
        :param question_embedding: one embedding of the question
        :return: prediction_batch (dict), tails: list
        """
        try:
            head_idx = self.entity2idx[head]
        except TypeError:
            head_idx = self.entity2idx[head[0]]
        candidates = self.subgraph_extractor.extract_neighbour_from_idx(head_idx)
        # temporary code for debugging before the ontospecies embedding is fixed
        selected_candidates = []
        for t_idx in candidates:
            triple_str = f"{head_idx}_{pred_p_label}_{t_idx}"
            if self.subgraph_extractor.check_triple_existence(triple_str=triple_str):
                selected_candidates.append(t_idx)

        candidates = selected_candidates
        candidate_labels = [self.idx2entity[c] for c in candidates]
        print("Candidate labels for normal question", candidate_labels)
        self.marie_logger.info(f" - Preparing prediction batch")
        candidate_entities = torch.LongTensor(candidates).to(self.device)
        repeat_num = len(candidates)
        head_entity_batch = torch.LongTensor([head_idx]).repeat(repeat_num).to(self.device)
        self.marie_logger.info(f" - Head entity index {head_idx}")
        prediction_batch = {'single_question': question_embedding, 'e_h': head_entity_batch, 'e_t': candidate_entities}
        self.marie_logger.info(f" - Prediction batch is prepared")
        if len(candidates) == 0:
            return None, None
        else:
            return prediction_batch, candidates

    def prepare_multi_target_prediction_batch(self):
        # 1. for each instance_list element (e.g., Assembly model 1,2,3), get create a matrix (AM --rel--> MoPs )
        instance_list = self.input_dict.mention[1]
        target = self.input_dict.mention[0]
        instance_list = torch.LongTensor([self.entity2idx[i] for i in instance_list])
        candidates = self.all_heads_indices[target]
        # candidates repeat num is len(instance_list )
        candidates_repeat_num = len(instance_list)
        heads_repeat_num = len(candidates)
        # repeat for the tails
        candidate_entity_batch = candidates.repeat(candidates_repeat_num).to(self.device)
        # interleave repeat for the heads
        split_indices = [indice for indice in range(heads_repeat_num, len(candidate_entity_batch), heads_repeat_num)]
        head_entity_batch = torch.repeat_interleave(instance_list, heads_repeat_num)
        prediction_batch = {'single_question': self.input_dict.single_question_embedding,
                            'e_h': head_entity_batch, 'e_t': candidate_entity_batch}

        return prediction_batch, candidate_entity_batch, split_indices, head_entity_batch

    def answer_multi_target_question(self):
        print("================== Processing multi-target question ==============")
        # Show all the MOPs which have Cuboctahedron shape.
        # 1. for each instance_list element (e.g., Assembly model 1,2,3), get create a matrix (AM --rel--> MoPs )
        # _, pred_p_idx = self.score_model.get_relation_prediction(self.input_dict.single_question_embedding)

        idx_to_replace = [23, 8, 12, 13, 18, 0]
        self.input_dict.instance_label = self.input_dict.mention[2]

        if self.input_dict.instance_label:
            self.input_dict.instance_label = self.input_dict.instance_label.lower()
            self.input_dict.question = self.input_dict.question.lower().replace(self.input_dict.instance_label, "")

        scores_top_k = []
        targets_top_k = []
        labels_top_k = []
        prediction_batch, candidates, split_indices, head_entity_batch = self.prepare_multi_target_prediction_batch()
        triple_scores, pred_p_idx = self.score_model.get_scores(prediction_batch, mode=self.mode)
        if self.input_dict.mention[0] == "mops":
            if pred_p_idx in idx_to_replace:
                pred_p_idx = 1
        # pred_p_label = self.idx2rel[pred_p_idx]
        for head, tail, score in zip(head_entity_batch, candidates, triple_scores):
            triple_string = f"{head}_{pred_p_idx}_{tail}"
            triple_exist = self.subgraph_extractor.check_triple_existence(triple_string)
            if triple_exist:
                labels_top_k.append(self.idx2entity[tail.item()])
                targets_top_k.append(self.idx2entity[head.item()])
                scores_top_k.append(score.item())

        # labels_top_k = [self.idx2entity[t.item()] for t in targets_top_k]
        # labels_top_k = targets_top_k
        numerical_top_k = ["EMPTY"] * len(labels_top_k)

        return labels_top_k, scores_top_k, targets_top_k, numerical_top_k, "multi-target"

    def answer_numerical_question(self):
        print("============================ NUMERICAL QUESTION ==================================")
        numerical_operator = self.input_dict.numerical_operator
        numerical_string = self.input_dict.numerical_string
        numerical_value = self.input_dict.numerical_value
        # question = self.input_dict.question
        # question = question.replace("'s ", " # ").replace(numerical_string, "")
        single_question_embedding = self.input_dict.single_question_embedding
        filtered_heads, filtered_numerical_values = self.filter_heads_by_numerical()
        # filtered_heads_labels = [self.idx2entity[h] for h in filtered_heads]

        if len(filtered_heads) == 0: return [], [], [], [], "normal"
        prediction_batch, tails, split_indices, filtered_heads_tensor = self.prepare_prediction_batch_numerical(
            heads=filtered_heads,
            question_embedding=single_question_embedding)

        predicted_tails = []
        predicted_tails_filtered = []
        predicted_values_list = []
        score_list = []
        target_list = []
        with no_grad():
            triple_scores, pred_p_idx = self.score_model.get_scores(prediction_batch, mode=self.mode)
            pred_p_label = self.idx2rel[pred_p_idx]
            for sub_score_list, sub_tail_list, head, predicted_numerical_value in zip(
                    torch.tensor_split(triple_scores, split_indices),
                    torch.tensor_split(tails, split_indices),
                    filtered_heads, filtered_numerical_values):
                top_scores, indices_top_k = torch.topk(sub_score_list, k=len(sub_score_list),
                                                       largest=self.largest)
                top_k_tails = [sub_tail_list[idx] for idx in indices_top_k]
                head_label = self.idx2entity[head]
                for tail in top_k_tails:
                    tail = tail.item()
                    tail_label = self.idx2entity[tail]
                    triple_str = f"{head}_{pred_p_idx}_{tail}"
                    # triple_label = f"{self.idx2entity[head]}_{self.idx2rel[pred_p_idx]}_{self.idx2entity[tail]}"
                    triple_exist = self.subgraph_extractor_numerical.check_triple_existence(triple_str)
                    # triple_exist = self.subgraph_extractor.check_triple_existence(triple_str)
                    if triple_exist:
                        if tail_label in self.value_dictionary:
                            try:
                                value = float(self.value_dictionary[tail_label])
                            except ValueError:
                                value = float(self.value_dictionary[tail_label].split(" ")[0].strip())

                            predicted_values_list.append(self.value_dictionary[tail_label])
                            required_value = self.input_dict.numerical_value
                            if self.input_dict.numerical_operator == "larger":
                                if value > required_value:
                                    predicted_tails.append(f"{value}")
                            elif self.input_dict.numerical_operator == "smaller":
                                if value < required_value:
                                    predicted_tails.append(f"{value}")
                            elif self.input_dict.numerical_operator == "about":
                                predicted_tails.append(f"{value}")
                        else:
                            predicted_tails.append(tail_label)

                        # predicted_values_list.append(self.value_lookup(tail_label))
                        target_list.append(head_label)
                        score_list.append(1)

                    # if self.test:
                    #     labels_top_k =
                    #     if numerical_value != "NODE HAS NO VALUE":
                    #         predicted_p = self.o_p_dict[labels_top_k]
                    #         if predicted_p == self.test_dict.true_rel:
                    #             predicted_tails_filtered.append(labels_top_k)
                    #             predicted_values_list.append(predicted_numerical_value)

        if self.test:
            return numerical_operator, predicted_tails, predicted_tails_filtered, predicted_values_list, pred_p_label
        else:
            print("=========== NUMERICAL RESULTS ===========")
            print(predicted_tails, score_list, target_list, predicted_values_list, "numerical")
            print("=========================================")
            return predicted_tails, score_list, target_list, predicted_values_list, "numerical"

    def answer_normal_question(self):
        print("=================== Processing NORMAL question ===========================")
        print("======= INPUT DICT MENTION ======")
        print("question:", self.input_dict.question)
        print(self.input_dict.mention)
        if isinstance(self.input_dict.mention, str):
            mention = self.input_dict.mention
        else:
            mention = self.input_dict.mention[2]
        single_question_embedding = self.input_dict.single_question_embedding
        _, head, mention_str, head_key = self.nel.find_cid(mention)
        print("mention:", mention)
        print("head", head)
        print("=================================")

        _, pred_p_idx = self.score_model.get_relation_prediction(question_embedding=single_question_embedding)
        pred_p_label = self.idx2rel[pred_p_idx]
        print("predicted relation label: ", pred_p_label)
        prediction_batch, candidates = self.prepare_prediction_batch(head=head,
                                                                     question_embedding=single_question_embedding,
                                                                     pred_p_label=pred_p_idx)

        if candidates is None:
            return None, None, None, None, "normal"


        scores, _ = self.score_model.get_scores(prediction_batch, mode=self.mode)
        _, indices_top_k = torch.topk(scores, k=min(5, len(scores)), largest=self.largest)

        selected_candidates = []

        labels_top_k = [self.idx2entity[candidates[index]] for index in indices_top_k]




        scores_top_k = [scores[index].item() for index in indices_top_k]
        targets_top_k = [head_key] * len(scores_top_k)
        numericals_top_k = [self.value_lookup(l) for l in labels_top_k]
        labels_top_k = numericals_top_k
        # try:
        #     mention = self.nel.get_mention(question=question)
        #     if mention == "":
        #         _, tokenized_question = self.nlp.tokenize_question(question=question, repeat_num=1)
        #         single_question_embedding = self.score_model.get_question_embedding(question=tokenized_question)
        #         return self.answer_numerical_question()
        # except IndexError:
        #     _, tokenized_question = self.nlp.tokenize_question(question=question, repeat_num=1)
        #     single_question_embedding = self.score_model.get_question_embedding(question=tokenized_question)
        #     return self.answer_numerical_question()
        #
        # question = question.replace(mention, "")
        # _, tokenized_question = self.nlp.tokenize_question(question=question, repeat_num=1)
        # single_question_embedding = self.score_model.get_question_embedding(question=tokenized_question)
        # _, head, mention_str, head_key = self.nel.find_cid(mention)
        # print("In normal question, found head", head)
        # if head is None:
        #     _, tokenized_question = self.nlp.tokenize_question(question=question, repeat_num=1)
        #     single_question_embedding = self.score_model.get_question_embedding(question=tokenized_question)
        #     return self.answer_numerical_question()
        #
        # _, pred_p_idx = self.score_model.get_relation_prediction(question_embedding=single_question_embedding)
        # pred_p_label = self.idx2rel[pred_p_idx]
        # prediction_batch, candidates = self.prepare_prediction_batch(head=head,
        #                                                              question_embedding=single_question_embedding,
        #                                                              pred_p_label=pred_p_label)
        #
        # scores, _ = self.score_model.get_scores(prediction_batch, mode=self.mode)
        # _, indices_top_k = torch.topk(scores, k=min(5, len(scores)), largest=self.largest)
        # labels_top_k = [self.idx2entity[candidates[index]] for index in indices_top_k]
        # scores_top_k = [scores[index].item() for index in indices_top_k]
        # targets_top_k = [head_key] * len(scores)
        # print(f"predicted predicate: {self.idx2rel[pred_p_idx]}")
        # print(f"predicted head: {head}")
        return labels_top_k, scores_top_k, targets_top_k, numericals_top_k, "normal"

    def rule_based_question_classification(self):
        """
        :return: Which question answering to go first, whether enable testing (for evaluation)
        """
        mention = self.input_dict.mention
        has_target, has_instance_list = False, False
        numerical_value, numerical_string = NumericalTools.numerical_value_extractor(
            question=self.input_dict.question)
        is_numerical = numerical_value is not None
        # if the question doesn't contain any numerical value, it is is_numerical is false
        if type(mention) == type(()):
            # if the mention is a tuple, it contains both target and instances_list
            has_target, has_instance_list = mention[0], mention[1]

        self.input_dict.numerical_string = numerical_string
        self.input_dict.numerical_value = numerical_value
        return is_numerical, has_target, has_instance_list, numerical_string, numerical_value
        # return is_numerical, has_target, has_target, numerical_string, numerical_value

    def find_answers(self):
        """
        The find answer method handles both normal questions and numerical questions for wikidata
        :param question: question in string format
        :return: score of all candidate answers
        """
        # TODO: 1. put the mechanism for question answering sequence on top
        # TODO: 2. wrap question answer mechanism into functions
        print("========== input dict question =======")
        print("question before filtering", self.input_dict.question)
        mention = self.nel.get_mention(self.input_dict.question)
        self.input_dict.mention = mention
        self.input_dict.question = self.text_filtering(self.input_dict.question)
        print(self.input_dict.question)
        print(mention)
        if isinstance(mention, str):
            self.input_dict.question = self.input_dict.question.replace(mention, "")
        else:
            mention = mention[2]
            if mention:
                self.input_dict.question = self.input_dict.question.replace(mention, "")
        print("filtered question:", self.input_dict.question)
        print("======================================")

        _, tokenized_question = self.nlp.tokenize_question(question=self.input_dict.question, repeat_num=1)
        # replace mention

        single_question_embedding = self.score_model.get_question_embedding(question=tokenized_question)
        self.input_dict.single_question_embedding = single_question_embedding
        self.get_numerical_operator(tokenized_question=tokenized_question,
                                    single_question_embedding=single_question_embedding)

        is_numerical, has_target, has_instance_list, numerical_string, numerical_value = \
            self.rule_based_question_classification()
        self.marie_logger.info(f"numerical operator: {self.input_dict.numerical_operator}")
        # has_target and has_instance_list -> answer_multi_target_question
        # has_target and is_numerical -> answer numerical question
        # not has_target, try to get the instance_list[0] -> answer normal question

        if has_target and has_instance_list:
            return self.answer_multi_target_question()
        else:
            if has_target and is_numerical:
                self.input_dict.target = self.input_dict.mention[0]
                return self.answer_numerical_question()
            else:

                if self.input_dict.numerical_operator == "none":
                    if self.ontology.lower() == "ontomops":
                        return ["EMPTY"], [-999], ["EMPTY"], ["EMPTY"], "normal"
                    else:
                        return self.answer_normal_question()
                else:
                    return ["EMPTY"], [-999], ["EMPTY"], ["EMPTY"], "normal"

            # return self.answer_numerical_question()

    def process_answer(self, answer_list, mention_string, score_list, answer_type, numerical_list):
        if answer_type == "normal":
            if len(mention_string) > 0:
                mention_string = mention_string[0]
            else:
                mention_string = "EMPTY"
        result_list = []
        for answer, score in zip(answer_list, score_list):
            row = {"answer": answer, "from node": answer, "mention": mention_string, "numerical": numerical_list}
            result_list.append(row)
        return result_list

    def run(self, question, mention=None):
        """
        :param mention:
        :param head: directly give a head for testing and evaluation purpose.
        :param question:
        :return:
        """
        self.input_dict.__init__()
        self.input_dict.question = question
        self.marie_logger.info("=============== new question ============")
        self.marie_logger.info(f"Received question: {question}")
        if mention is not None and (mention != ""):
            self.input_dict.mention = mention

        answer_list, score_list, target_list, numerical_list, answer_type = self.find_answers()
        if score_list:
            if len(score_list) == 0:
                return [], [], [], [], "normal"

        else:
            return [], [], [], [], "normal"
        max_score = max(score_list)
        score_list = [(max_score + 1 - s) for s in score_list]
        return answer_list, score_list, target_list, numerical_list, answer_type


if __name__ == "__main__":
    ontology = "OntoMoPs"
    sub_ontology = "numerical_with_implicit"
    dataset_dir = os.path.join(DATA_DIR, f"CrossGraph/{ontology}/{sub_ontology}")
    my_engine = QAEngineNumerical(dataset_dir, sub_ontology, dim=50, test=False, value_dict_name="node_value_dict.json",
                                  seperated_model=True, operator_dict={0: "smaller", 1: "larger", 2: "none"},
                                  mode="transr", enable_class_ner=True)
    rst = my_engine.run("MoPs with molecular weight more than 10")
    # rst = my_engine.run("Starting with Oh symmetry point group, list all the MOPs")
    # print(rst)
