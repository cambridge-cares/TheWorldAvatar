from nltk import word_tokenize, pos_tag
from nltk.corpus import wordnet as wn
import json


def penn_to_wn(tag):
    if (tag.startswith('N')): return 'n'
    if (tag.startswith('V')): return 'v'
    if (tag.startswith('J')): return 'a'
    if (tag.startswith('R')): return 'r'
    return None


def tagged_to_synset(word, tag):
    wn_tag = penn_to_wn(tag)
    if wn_tag is None:
        return None

    try:
        return wn.synsets(word, wn_tag)[0]
    except:
        return None


def phraseSimilarity(phrase1, phrase2):
    # tokenize and tag the phrases first

    phrase1 = pos_tag(word_tokenize(phrase1))
    phrase2 = pos_tag(word_tokenize(phrase2))

    synsets1 = [tagged_to_synset(*tagged_word) for tagged_word in phrase1]
    synsets2 = [tagged_to_synset(*tagged_word) for tagged_word in phrase2]

    score, count = 0.0, 0

    for synset in synsets1:
        # Get the similarity value of the most similar word in the other sentence
        count = count + 1
        temp = []
        for ss in synsets2:

            if (ss and synset):
                if (synset.path_similarity(ss)):
                    temp.append(synset.path_similarity(ss))

        if (len(temp) > 0):
            best_score = max(temp)
        else:
            best_score = 0
        score += best_score

    averageScore = (score / count)
    return averageScore


# with open('predicate.json') as file:
#     json = json.loads(file.read())
#
# predicateArray = json['results']['bindings']
# for predicate in predicateArray:
#     label = predicate['v']['value']
#     print('label --- ', label)
#     s = phraseSimilarity(label, 'size')
#     print(s)
# phraseSimilarity('Big Bang Theory','Big Bang')
