from nltk import word_tokenize, pos_tag
from nltk.corpus import wordnet as wn
import json
import random

from difflib import SequenceMatcher


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


def fast_phraseSimilarity(phrase1, phrase2):
    return SequenceMatcher(None, phrase1, phrase2).ratio() + random.randint(1, 21) * 0.0000001


def phraseSimilarity(phrase1, phrase2):
    # tokenize and tag the phrases first

    original_phrase1 = phrase1
    original_phrase2 = phrase2
    phrase1 = pos_tag(word_tokenize(phrase1.lower()))
    phrase2 = pos_tag(word_tokenize(phrase2.lower()))

    synsets1 = [tagged_to_synset(*tagged_word) for tagged_word in phrase1]
    synsets2 = [tagged_to_synset(*tagged_word) for tagged_word in phrase2]

    none_flag = False
    for syn1 in synsets1:
        for syn2 in synsets2:
            if (not syn1) or (not syn2):
                none_flag = True

    if none_flag:
        return fast_phraseSimilarity(original_phrase1, original_phrase2)

    score, count = 0.0, 0

    length = max(len(synsets1), len(synsets2))
    for synset in synsets1:
        # Get the similarity value of the most similar word in the other sentence
        temp = []
        for ss in synsets2:
            count = count + 1
            if (ss and synset):
                similarity = synset.path_similarity(ss)
                # print('syn:' ,synset)
                # print('ss:', ss)
                # print('similarity', similarity)
                if (similarity):
                    temp.append(similarity)

        if (len(temp) > 0):
            best_score = max(temp)

        else:
            best_score = 0

        score += best_score

    averageScore = (score / length)
    return averageScore + random.randint(1, 21) * 0.0000001

#
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
