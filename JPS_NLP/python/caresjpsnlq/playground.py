import NLP_CompareString


score = NLP_CompareString.phraseSimilarity('area Total', 'area Total')
score2 = NLP_CompareString.phraseSimilarity('area Total', 'area')

print(' ', score)
print('' , score2)