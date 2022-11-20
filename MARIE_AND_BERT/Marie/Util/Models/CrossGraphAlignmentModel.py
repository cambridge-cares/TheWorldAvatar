import torch
from torch import nn, no_grad
from torch.nn.functional import one_hot
from transformers import BertModel, BertTokenizer, AdamW


class CrossGraphAlignmentModel(nn.Module):

    def __init__(self, device):
        super(CrossGraphAlignmentModel, self).__init__()
        self.device = device
        self.bert = BertModel.from_pretrained('bert-base-uncased')
        # self.bert_reduction_layer = nn.Linear(768, 512)
        self.bert_reduction_layer = nn.Linear(768, 512)
        self.bert_reduction_layer_2 = nn.Linear(512, 256)
        self.bert_reduction_layer_3 = nn.Linear(256, 4)
        self.domain_factor_layer = nn.Linear(4, 1)
        self.domain_question_factor_layer = nn.Linear(4, 1)
        self.criterion = nn.MarginRankingLoss(margin=1, reduction='none').to(self.device)
        self.sigmoid = nn.Sigmoid()
        self.relu = nn.ReLU()

    def adjust_score(self, triple):
        """
        Score adjustment has 3 inputs: question, score, domain
        The purpose is to train a model that
        :param triple:
        :return:
        """
        question = triple[0]
        domain = triple[2].to(self.device)
        domain = one_hot(domain, num_classes=4).type(torch.FloatTensor).to(self.device)
        question_vector = self.process_question(question)

        # =============== get domain - question factor ==================
        # domain_question_vector = torch.cat([question_vector, domain], dim=1).to(self.device)
        domain_question_factor = self.sigmoid(self.domain_question_factor_layer(question_vector).squeeze(-1))

        return domain_question_factor

    def process_question(self, question):
        max_len = 12
        input_ids = torch.reshape(question['input_ids'], (-1, max_len))
        attention_mask = torch.reshape(question['attention_mask'], (-1, max_len))
        pooled_output = self.bert(input_ids=input_ids.to(self.device),
                                  attention_mask=attention_mask.to(self.device),
                                  return_dict=False)[1].to(self.device)

        question_vector = self.bert_reduction_layer(pooled_output).to(self.device)
        question_vector = self.relu(question_vector).to(self.device)
        question_vector = self.bert_reduction_layer_2(question_vector).to(self.device)
        question_vector = self.relu(question_vector).to(self.device)
        question_vector = self.bert_reduction_layer_3(question_vector).to(self.device)

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
        true_domain = true_answer[2].type(torch.FloatTensor).to(self.device)
        pred_domain = self.adjust_score(true_answer).to(self.device)
        return nn.BCELoss()(pred_domain, true_domain), pred_domain

    def predict(self, triple):
        """
        :param triple: (question, score, domain)
        :return:
        """
        pred_domain = self.adjust_score(triple).to(self.device)
        score = triple[1]
        domain = triple[2]
        diff = torch.abs(pred_domain - domain)
        return score + (1 - diff)
