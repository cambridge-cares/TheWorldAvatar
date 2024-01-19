import nltk
from nltk.corpus import *
from nltk import *


class UnigramChunker(nltk.ChunkParserI):
    def __init__(self, train_sents):  # [_code-unigram-chunker-constructor]
        train_data = [[(t, c) for w, t, c in nltk.chunk.tree2conlltags(sent)]
                      for sent in train_sents]
        self.tagger = nltk.UnigramTagger(train_data)  # [_code-unigram-chunker-buildit]

    def parse(self, sentence):  # [_code-unigram-chunker-parse]
        pos_tags = [pos for (word, pos) in sentence]
        tagged_pos_tags = self.tagger.tag(pos_tags)
        chunktags = [chunktag for (pos, chunktag) in tagged_pos_tags]
        conlltags = [(word, pos, chunktag) for ((word, pos), chunktag)
                     in zip(sentence, chunktags)]
        return nltk.chunk.conlltags2tree(conlltags)
