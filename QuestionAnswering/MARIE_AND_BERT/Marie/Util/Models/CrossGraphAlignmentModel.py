import os

import torch
from torch import nn, no_grad, unsqueeze
from torch.nn.functional import one_hot
from transformers import BertModel, BertTokenizer, AdamW

from Marie.CandidateSelection.location import DATA_DIR
from Marie.Util.location import PRETRAINED_DIR


class CrossGraphAlignmentModel(nn.Module):

    def __init__(self, device, dim = 9):
        super(CrossGraphAlignmentModel, self).__init__()
        self.device = device
        self.bert = BertModel.from_pretrained(PRETRAINED_DIR,  local_files_only=True)
        print('CrossGraphAlignment modelloaded')
        # self.bert_reduction_layer = nn.Linear(768, 512)
        self.bert_reduction_layer = nn.Linear(768, dim)
        # self.bert_reduction_layer_2 = nn.Linear(512, 5)
        # self.bert_reduction_layer_3 = nn.Linear(256, 5)
        # self.bert_reduction_layer_4 = nn.Linear(128, 64)
        # self.bert_reduction_layer_5 = nn.Linear(64, 32)
        # self.bert_reduction_layer_6 = nn.Linear(32, 5)
        self.domain_factor_layer = nn.Linear(dim, 1)
        self.domain_question_factor_layer = nn.Linear(dim, 1)
        self.criterion = nn.MarginRankingLoss(margin=1, reduction='none').to(self.device)
        self.sigmoid = nn.Sigmoid()
        self.softmax = nn.Softmax()
        self.relu = nn.ReLU()

    def adjust_score(self, triple):
        """
        Score adjustment has 3 inputs: question, score, domain
        The purpose is to train a model that
        :param triple:
        :return:
        """
        question = triple[0]
        question_vector = self.process_question(question).to(self.device)

        # =============== get domain - question factor ==================
        # domain_question_vector = torch.cat([question_vector, domain], dim=1).to(self.device)
        # domain_question_factor = self.sigmoid(self.domain_question_factor_layer(question_vector).squeeze(-1))
        # domain_question_factor = torch.softmax(question_vector.squeeze(-1),
        #                                       dim=1)  #
        domain_question_factor = self.sigmoid(question_vector.squeeze(-1)).to(self.device)
        return domain_question_factor

    def process_question(self, question):
        max_len = 12
        input_ids = torch.reshape(question['input_ids'], (-1, max_len))
        attention_mask = torch.reshape(question['attention_mask'], (-1, max_len))
        pooled_output = self.bert(input_ids=input_ids.to(self.device),
                                  attention_mask=attention_mask.to(self.device),
                                  return_dict=False)[1].to(self.device)

        question_vector = self.bert_reduction_layer(pooled_output).to(self.device)
        # question_vector = self.relu(question_vector).to(self.device)
        # question_vector = self.bert_reduction_layer_2(question_vector).to(self.device)
        # question_vector = self.relu(question_vector).to(self.device)
        # question_vector = self.bert_reduction_layer_3(question_vector).to(self.device)
        # question_vector = self.relu(question_vector).to(self.device)
        # question_vector = self.bert_reduction_layer_4(question_vector).to(self.device)
        # question_vector = self.relu(question_vector).to(self.device)
        # question_vector = self.bert_reduction_layer_5(question_vector).to(self.device)
        # question_vector = self.relu(question_vector).to(self.device)
        # question_vector = self.bert_reduction_layer_6(question_vector).to(self.device)
        return question_vector

    def loss(self, positive_distances, negative_distances):
        target = torch.tensor([1], dtype=torch.long, device=self.device)
        return self.criterion(positive_distances, negative_distances, target).to(self.device)

    def forward(self, true_answer):
        """
        The purpose is to make sure the true answer out ranks fake answers after the score adjustment
        The loss function is MarginRank
        :param true_answer: (question, score, domain)
        :return:
        """

        true_domain = torch.FloatTensor(true_answer[2]).to(self.device)
        # true_domain = unsqueeze(true_domain, 1)
        pred_domain = self.adjust_score(true_answer).to(self.device)
        return nn.BCELoss()(pred_domain,
                            true_domain), pred_domain  # this is a multiclass classification

    def predict_domain(self, question):
        pred_domain = self.adjust_score(question).to(self.device)
        pred_domain = pred_domain > 0.5
        pred_domain = pred_domain.type(torch.LongTensor)
        return pred_domain

    def predict(self, question):
        """
        :param triple: (question, score, domain)
        :return:
        """
        # score = triple[1]
        question = [question]
        pred_domain = self.adjust_score(question).to(self.device)
        pred_domain = pred_domain > 0.5
        pred_domain = pred_domain.type(torch.LongTensor)
        # pred_domain = 2 - pred_domain
        # domain = triple[2].to(self.device)
        # factor = torch.eq(pred_domain, domain).type(torch.LongTensor)
        # factor = torch.sum(factor, dim=1)
        # factor = factor.reshape(-1, 5)

        return pred_domain


if __name__ == "__main__":
    from Marie.Util.CommonTools.NLPTools import NLPTools

    nlp = NLPTools(tokenizer_name="bert-base-uncased")
    my_alignment_model = CrossGraphAlignmentModel(device="cpu")
    dataset_path = os.path.join(DATA_DIR, "CrossGraph/cross_graph_model_with_all_9")
    my_alignment_model.load_state_dict(torch.load(dataset_path, map_location="cpu"))
    questions = ["molar mass",
                 "boiling point",
                 "geometry",
                 "vapour pressure",
                 "chemical structure",
                 "inventor",
                 "some random bullshit",
                 "flash point",
                 "location discovery"]
    for q in questions:
        _, tokenized_q = nlp.tokenize_question(q, 1)
        pred_domain = my_alignment_model.predict_domain([tokenized_q])

        print(f"Question: {q} \n Predicted domain: {pred_domain}")
        print(pred_domain.repeat(5, 1))
