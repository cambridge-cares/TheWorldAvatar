import config
import json
from word2number import w2n


class PreProcessor:
    def __init__(self):
        # initiate the PreProcessor, load dictionaries for different purposes
        self.tagging_rectifier_dictionary = config.read_file(config.TAGGING_RECTIFIER_DICTIONARY)
        self.key_word_rectifier_dictionary = config.read_file(config.KEY_WORD_RECTIFIER_DICTIONARY)

    def replace_special_words(self, sentence):
        original_sentence = sentence
        sentence = PreProcessor.replace_word_with_digits(sentence)
        for key, value in self.key_word_rectifier_dictionary.items():
            sentence = sentence.replace(key, value).strip()

        return {'sentence': sentence, 'original_sentence': original_sentence}

    def recify_tagging_result(self, tagged):
        _temp = []
        for tag in tagged:
            if tag[0].lower().strip() in self.tagging_rectifier_dictionary.keys():
                new_item = (tag[0], self.tagging_rectifier_dictionary[tag[0].lower().strip()])
                _temp.append(new_item)
            else:
                _temp.append(tag)
        return _temp

    def filter_tokens_result(self, tokens):
        for t in tokens:
            if t.lower().strip() == 'both':
                tokens.remove(t)

        return tokens

    @classmethod
    def replace_word_with_digits(cls, sentence):
        temp = []
        for word in sentence.split(' '):
            try:
                new_word = w2n.word_to_num(word)
                temp.append(str(new_word))
            except ValueError:
                temp.append(word)

        return ' '.join(temp)
