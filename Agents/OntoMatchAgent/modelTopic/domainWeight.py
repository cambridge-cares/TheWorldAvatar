#test file, please ignore
#do a excluding screening
from modelTopic import topicModel
from getOwlClassNames import getOwlClassNames
from spiral import ronin

tm = topicModel('../model/model30t30pautoa.gensim', addrDict='../model/dictionary.gensim', mode='loadModel')
tm.printTopics()
# now infer a new document

with open('../../powerplantclasses.txt', 'r') as f:
    file1 = f.read()

topicsIn1 = tm.inferNewDoc([file1], 0.01)
with open('../../dbpediaclasses.txt', 'r') as f:
    file2 = f.read()

topicsIn2 = tm.inferNewDoc([file1, file2])

print('dddd' in tm.dictionary.token2id)
print('____________________________________')
thres = 0.01
#get all words from topics above thres & topic not belong to target & topic word not in target


topicIds1 = [k for (k,p) in topicsIn1]
topicIds2 = [k for (k,p) in topicsIn2]
topicsInter = [b for b in topicsIn1 if b[0] in topicIds2]

for tpid in topicIds1:
    ls = [(tm.id2Word(id),p) for id, p in tm.ldamodels.get_topic_terms(tpid)]
    print(str(ls))
print('____________________________________')
for tpid in topicIds2:
    ls = [(tm.id2Word(id),p) for id, p in tm.ldamodels.get_topic_terms(tpid)]
    print(str(ls))
wordIds = set()
for tpid, p in topicsInter:
    wordIds.update([id for id, p in tm.ldamodels.get_topic_terms(tpid)])
#interDomainWords = [tm.id2Word(wid) for wid in list(wordIds)]
#make a dict of words

