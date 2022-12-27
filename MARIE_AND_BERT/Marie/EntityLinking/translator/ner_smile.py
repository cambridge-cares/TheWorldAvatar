import torch
import transformers



class NerSMILE():
    def __init__(self, config):
    #config parameters
    #read model
        model = transformers.BertForTokenClassification.from_pretrained('bert-base-uncased',  num_labels = 3)
        model = torch.nn.DataParallel(model)
        model.load_state_dict(torch.load(config['model_path']),strict=False)
        self.model = model
        self.config = config



    def extractSMILE(self, sentence_list):
        results = self.get_predictions(self.model, sentence_list)
        return results

    def prediction_fn(self, model, tokenized_sub_sentence):

        tkns = tokenized_sub_sentence
        indexed_tokens = self.config['tokenizer'].convert_tokens_to_ids(tkns)
        segments_ids = [0] * len(indexed_tokens)

        tokens_tensor = torch.tensor([indexed_tokens]).to(self.config['device'])
        segments_tensors = torch.tensor([segments_ids]).to(self.config['device'])

        model.eval()
        with torch.no_grad():
            logit = model(tokens_tensor,
                          token_type_ids=None,
                          attention_mask=segments_tensors)

            logit_new = logit[0].argmax(2).detach().cpu().numpy().tolist()
            prediction = logit_new[0]

            kword = ''
            kword_list = []

            for k, j in enumerate(prediction):
                if (len(prediction)>1):

                    if (j!=0) & (k==0):
                        #if it's the first word in the first position
                        #print('At begin first word')
                        begin = tkns[k]
                        kword = begin

                    elif (j!=0) & (k>=1) & (prediction[k-1]==0):
                        #begin word is in the middle of the sentence
                        begin = tkns[k]
                        previous = tkns[k-1]

                        if begin.startswith('##'):
                            kword = previous + begin[2:]
                        else:
                            kword = begin

                        if k == (len(prediction) - 1):
                            #print('begin and end word is the last word of the sentence')
                            kword_list.append(kword.rstrip().lstrip())

                    elif (j!=0) & (k>=1) & (prediction[k-1]!=0):
                        # intermediate word of the same keyword
                        inter = tkns[k]

                        if inter.startswith('##'):
                            kword = kword + "" + inter[2:]
                        else:
                            kword = kword + "" + inter


                        if k == (len(prediction) - 1):
                            #print('begin and end')
                            kword_list.append(kword.rstrip().lstrip())

                    elif (j==0) & (k>=1) & (prediction[k-1] !=0):
                        # End of a keywords but not end of sentence.
                        kword_list.append(kword.rstrip().lstrip())
                        kword = ''
                        inter = ''
                else:
                    if (j!=0):
                        begin = tkns[k]
                        kword = begin
                        kword_list.append(kword.rstrip().lstrip())

        return kword_list

    #data format?
    def get_predictions(self, model, sentence_list):
        results = []
        for id, sentence in enumerate(sentence_list):
            current_id_predictions = []
            tokenized_sub_sentence = self.config['tokenizer'].tokenize(sentence)

            if len(tokenized_sub_sentence) == 0:
                # If the tokenized sentence are empty
                sub_sentence_prediction_kword_list = []

            elif len(tokenized_sub_sentence) <= 512:#TODO:ignore sentences that are too long for now
                # If the tokenized sentence are less than 512
                sub_sentence_prediction_kword_list = self.prediction_fn(model, tokenized_sub_sentence)

            if len(sub_sentence_prediction_kword_list) !=0:
                print(sub_sentence_prediction_kword_list)
                current_id_predictions = current_id_predictions + self.kword2original(sentence, sub_sentence_prediction_kword_list)

            results.append([x for x in list(set(current_id_predictions))])
        return results

    def kword2original(self, sentence, kwords):
        orginal = []
        for kword in kwords:
            start = sentence.lower().find(kword)
            end = start + len(kword)
            ori = sentence[start:end]
            orginal.append(ori)
        return orginal