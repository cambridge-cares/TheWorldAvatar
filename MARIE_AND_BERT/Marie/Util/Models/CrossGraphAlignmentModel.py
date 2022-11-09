import torch
from torch import nn, no_grad
from transformers import BertModel, BertTokenizer, AdamW


class CrossGraphAlignmentModel(nn.Module):

    def __init__(self, device):
        super(CrossGraphAlignmentModel, self).__init__()
        self.device = device
        self.bert = BertModel.from_pretrained('bert-base-uncased')
        self.bert_reduction_layer = nn.Linear(768, 2)
        self.score_factor_layer = nn.Linear(4, 1)
        self.criterion = nn.MarginRankingLoss(margin=2, reduction='none').to(self.device)

    def adjust_score(self, triple):
        max_len = 12
        question = triple[0]
        score = triple[1].to(self.device)
        domain = triple[2].to(self.device)
        input_ids = torch.reshape(question['input_ids'], (-1, max_len))
        attention_mask = torch.reshape(question['attention_mask'], (-1, max_len))
        pooled_output = self.bert(input_ids=input_ids.to(self.device),
                                  attention_mask=attention_mask.to(self.device),
                                  return_dict=False)[1].to(self.device)

        question_vector = self.bert_reduction_layer(pooled_output).to(self.device)
        print('question shape', question_vector)
        # question_vector = question_vector.repeat_interleave(5).to(self.device)
        # print('question shape', question_vector)
        # question_vector = torch.transpose(question_vector, 0, -1)
        # print('question shape', question_vector)
        # question_vector = torch.stack([question_vector[0], question_vector[1]])
        # print('question shape', question_vector)

        score_and_domain = torch.stack([torch.transpose(score, 0, -1), torch.transpose(domain, 0, -1)], dim=0)
        score_and_domain = torch.transpose(score_and_domain, 0, 1)
        concat_vector = torch.cat([question_vector, score_and_domain], dim=-1).type(torch.FloatTensor).to(self.device)
        adjusted_score = self.score_factor_layer(concat_vector).to(self.device).squeeze(1)
        return adjusted_score

    def loss(self, positive_distances, negative_distances):
        target = torch.tensor([1], dtype=torch.long, device=self.device)
        return self.criterion(positive_distances, negative_distances, target).to(self.device)

    def forward(self, true_answer, fake_answer):
        """
        The purpose is to make sure the true answer out ranks fake answers after the score adjustment
        The loss function is MarginRank
        :param true_answer: (question, score, domain)
        :param fake_answer: (question, score, domain)
        :return:
        """
        true_answer_score = self.adjust_score(true_answer).to(self.device)
        fake_answer_score = self.adjust_score(fake_answer).to(self.device)

        return self.loss(true_answer_score, fake_answer_score).to(self.device)
