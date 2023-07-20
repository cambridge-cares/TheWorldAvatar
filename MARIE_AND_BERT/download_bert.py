from transformers import BertModel
print("========= Initializing BERT in advance =========")
print("Downloading BERT pretrained model")
bert = BertModel.from_pretrained('bert-base-cased')
print("Done downloading BERT pretrained model")