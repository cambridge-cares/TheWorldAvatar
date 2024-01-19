import NLTK_Chunker
from nltk.corpus import conll2000
import nltk
import json
import random
import re
from word2number import w2n
import time
import math
from nltk.tag import StanfordNERTagger
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords
from nltk.tree import *

import JPS_NLQ_test


class Chunker:
    def __init__(self):
        self.map_of_tokens = {}
        self.list_of_subjects_candidate = []
        self.list_of_predicates_candidate = []
        self.list_of_NIN_pairs = []
        self.list_of_subjects_from_NIN_pairs = []

        self.collection_of_cascading_subjects = set()
        self.index_map = {}

        self.original_sentence = ''

        self.target_class = ''
        self.expected_class = ''
        self.general_condition = ''
        self.general_question = False

        self.condition_object = {'condition_target': '', 'JJS': '', 'RBS': '', 'Types': [], 'value': ''}
        self.hmq_object = {}
        self.SMA_Object = {'SMA': False, 'Target': ''}

        self.list_of_function_indicators = []
        self.list_of_index_removed = []
        self.list_of_suspicious_verb = []
        self.connect_words = ['as well as', 'and']
        self.stop_words = ['the', 'that', 'people', 'both', 'place', 'many']
        self.predefined_stop_words = ['are', '', 'of', 'was', 'is', 'were', 'did', 'does', 'do', 'when', 'what',
                                      'where',
                                      'which', 'who', 'many', 'much', 'how', 'has', 'have', 'than',
                                      'would', 'will', 'there', 'here', 'that', 'ever', 'both', 'in', 'as', 'well',
                                      'place', 'and']
        self.jjr_stop_words = ['higher', 'shorter', 'taller', 'lower']

        self.general_question_key_words = ['is', 'were', 'was', 'do', 'does', 'did', 'are']
        self.condition_filter_stop_words = []

    def filter_named_entity(self, recognized_ne_array, tree):
        joined_tree = []
        if recognized_ne_array:
            the_indicator_ne = False
            the_indicator_tree = False
            # print('Type of the tree: ', type(tree))
            for word in tree:
                if word[1] is not 'POS':
                    if word[0].strip().lower() == 'did' or word[0].strip().lower() == 'the':
                        if word[0].strip().lower() == 'the':
                            the_indicator_tree = True
                    else:
                        joined_tree.append(word[0])

            joined_tree = ' '.join(joined_tree).strip()

            # At the end of the funciton, make each element in recognized_ne_array in to phrase.

            for recognized_ne in recognized_ne_array:
                joined_ne = []
                for ne_word in recognized_ne:
                    if ne_word[0].strip().lower() == 'did' or word[0].strip().lower() == 'the':
                        if word[0].strip().lower() == 'the':
                            the_indicator_ne = True
                    else:
                        joined_ne.append(ne_word[0])

                joined_ne = ' '.join(joined_ne)

                if joined_ne.strip() in joined_tree.strip():
                    suspicious_verb = re.sub(joined_ne, '', joined_tree)

                    if '  ' not in suspicious_verb:
                        self.list_of_predicates_candidate.append(suspicious_verb.strip())
                        self.list_of_suspicious_verb.append(suspicious_verb.strip())

                        if the_indicator_ne:
                            joined_ne = 'the ' + joined_ne.strip()

                        if the_indicator_tree:
                            joined_tree = 'the ' + joined_tree.strip()

                        if joined_tree.strip() == joined_ne.strip():
                            self.list_of_subjects_candidate.append(joined_tree)
                        else:
                            self.list_of_subjects_candidate.append([joined_tree, joined_ne])

            return {'filtered': (joined_tree.strip() == joined_ne.strip()), 'joined_tree': joined_tree}

    def join_tree_into_phrases(self, tree_leaves, removeStopWords):
        temp = []
        stop = set(stopwords.words('english'))
        stop.update(['did', 'do', 'does', 'what', 'which', 'both'])
        non_stop = ['of', 'in', 'the']
        if removeStopWords:
            for word in tree_leaves:
                if (word[0].lower().strip() not in stop) or (word[0] in non_stop):
                    temp.append(word[0])
            return ' '.join(temp)
        else:
            for word in tree_leaves:
                temp.append(word[0])

            return ' '.join(temp)

    def remove_suspicious_from_triple(self):

        new_list_of_NIN_pairs = []
        not_none_indicator = False
        for triple in self.list_of_NIN_pairs:
            _triple = triple['triple']
            _type = triple['type']
            new_triple = ()
            for i, phrase in enumerate(_triple):
                temp = []
                if phrase:
                    if type(phrase) == type('x'):
                        not_none_indicator = True
                        for word in phrase.split(' '):
                            if word not in self.list_of_suspicious_verb and (word.strip().lower() != 'many'):
                                temp.append(word.strip())
                        new_phrase = ' '.join(temp).strip().replace("'s ", '')
                        new_triple += (new_phrase,)
            if not_none_indicator:
                new_list_of_NIN_pairs.append({'type': _type, 'triple': new_triple})

        if not_none_indicator:
            self.list_of_NIN_pairs = new_list_of_NIN_pairs

    def remove_redundant_from_predicates(self):
        new_list = []
        # print('original predicates list', self.list_of_predicates_candidate)
        for predicates in self.list_of_predicates_candidate:  # you are expecting tuples
            if type(predicates) == type(('a', 'b')):
                # print('predicates tuple --', predicates[0])
                if (predicates[0].strip().lower() not in self.predefined_stop_words) and (
                        predicates[0].strip().lower() not in new_list):
                    new_list.append(predicates[0])
            else:
                # print('predicates string - ', predicates)
                if (predicates.strip().lower() not in self.predefined_stop_words) and (
                        predicates.strip().lower() not in new_list):
                    new_list.append(predicates)

        self.list_of_predicates_candidate = new_list

    def remove_redundant_in_sub_from_triple(self):
        # remove the and strip the subjects terms
        # compare with tuples in triples
        # TODO: Compare word in triple with words in sub list, if it is a list, replace word in triple with
        for pair_index, pair in enumerate(self.list_of_NIN_pairs):
            triple = pair['triple']
            for word_index, word_in_triple in enumerate(triple):
                # print('word_in_triple', word_in_triple)
                # word in triple doesn't contain 'the' but sub_list does
                for element_in_sub_list in self.list_of_subjects_candidate:
                    # 2 options, list and string
                    if type(element_in_sub_list) == type([]):  # it is a list
                        there_is_duplication = False
                        for word_in_sub in element_in_sub_list:
                            # remove 'the' first
                            first_word = word_in_sub.split(' ')[0].strip().lower()
                            if first_word == 'the':
                                word_in_sub_with_out_the = ' '.join(word_in_sub.split(' ')[1:]).strip()
                                if word_in_triple.strip().split(' ')[0].strip() == 'the':
                                    word_in_triple = word_in_triple[4:]
                                there_is_duplication = (
                                        word_in_sub_with_out_the == word_in_triple.strip() or there_is_duplication)

                            else:
                                if word_in_triple.strip().split(' ')[0].strip() == 'the':
                                    word_in_triple = word_in_triple[4:]
                                _word_in_sub_with_the = ' '.join(word_in_sub.split(' ')).strip()

                                there_is_duplication = (
                                        _word_in_sub_with_the == word_in_triple.strip() or there_is_duplication)

                        # replace the word in the triple with
                        if there_is_duplication:
                            self.list_of_subjects_candidate.remove(element_in_sub_list)
                            the_list = element_in_sub_list
                            _triple = self.list_of_NIN_pairs[pair_index]['triple']
                            _triple_list = list(_triple)
                            _triple_list[word_index] = the_list
                            # print(there_is_duplication)
                            self.list_of_NIN_pairs[pair_index]['triple'] = tuple(_triple_list)

                    # the element is a string
                    else:
                        first_word = element_in_sub_list.split(' ')[0].strip().lower()
                        _element_in_sub_list = element_in_sub_list.replace("'s ", '')
                        if first_word == 'the':
                            word_in_sub_with_out_the = ' '.join(_element_in_sub_list.split(' ')[1:]).strip()
                            if word_in_sub_with_out_the == word_in_triple.strip():
                                # remove element_in_sub_list from the list:
                                self.list_of_subjects_candidate.remove(element_in_sub_list)
                        else:
                            if _element_in_sub_list == word_in_triple.strip():
                                # print(' Found one: ', _element_in_sub_list, word_in_triple.strip())
                                self.list_of_subjects_candidate.remove(element_in_sub_list)

    def clean_triple_list(self):
        # To remove suspicious verbs from the triples list
        # join cascading triple (triple of triple)
        # remove "'s" from the triple
        # TODO: Join triples and add them to the subjects
        self.remove_suspicious_from_triple()

        subjects_list = []
        objects_list = []
        levels_list = []

        if len(self.list_of_NIN_pairs) > 1:

            for pair in self.list_of_NIN_pairs:
                if type(pair['triple'][0]) == type([]):  # triple element is a list
                    subjects_list.append(pair['triple'][0][0].strip())
                else:
                    subjects_list.append(pair['triple'][0].strip())

                if type([]) == type(pair['triple'][1]):  # triple element is a list
                    objects_list.append(pair['triple'][1][0].strip())
                else:
                    objects_list.append(pair['triple'][1].strip())

                levels_list.append(3)
        else:

            for pair in self.list_of_NIN_pairs:
                _type = pair['type']
                if _type == 'POS':
                    if pair['triple'][0] and pair['triple'][1]:
                        sub_phrase = pair['triple'][0].strip() + "'s " + pair['triple'][1].strip()
                        self.list_of_subjects_from_NIN_pairs.append(sub_phrase)
                else:
                    if pair['triple'][0] and pair['triple'][1]:
                        sub_phrase = pair['triple'][1].strip() + " of " + pair['triple'][0].strip()
                        self.list_of_subjects_from_NIN_pairs.append(sub_phrase)

        for sub_idx, sub in enumerate(subjects_list):
            for obj_idx, obj in enumerate(objects_list):
                if sub == obj:  # sub is the following triple, with an idx sub_idx
                    levels_list[sub_idx] -= 1
                    levels_list[obj_idx] += 1

        for sub_idx_1, sub_1 in enumerate(subjects_list):
            for sub_idx_2, sub_2 in enumerate(subjects_list):
                if (sub_1 == sub_2) and (sub_idx_1 is not sub_idx_2):
                    type_of_sub_1 = self.list_of_NIN_pairs[sub_idx_1]['type']
                    type_of_sub_2 = self.list_of_NIN_pairs[sub_idx_2]['type']
                    if type_of_sub_1 == 'POS':
                        # print('In POS ---', 'sub_idx_2', sub_idx_2, 'sub_idx_1', sub_idx_1)
                        if sub_idx_2 > sub_idx_1:
                            levels_list[sub_idx_1] += 1
                            levels_list[sub_idx_2] -= 1
                        else:
                            levels_list[sub_idx_2] += 1
                            levels_list[sub_idx_1] -= 1

        # print('levels_list', levels_list)
        # print('self.list_of_NIN_pairs', self.list_of_NIN_pairs)

        if len(levels_list) == 2:
            if levels_list[0] == levels_list[1]:
                print('same level')
            else:
                print('\t\t|1st:', [NIN_pair['triple'] for _, NIN_pair in
                                    sorted(zip(levels_list, self.list_of_NIN_pairs), reverse=True)])
                print('\t\t|2nd:',
                      [NIN_pair['triple'] for _, NIN_pair in sorted(zip(levels_list, self.list_of_NIN_pairs))])

    def remove_redundant_from_sub(self):
        list_of_list = []
        set_of_string = set()
        new_list = []
        for element in self.list_of_subjects_candidate:
            if type(element) == type([]):  # this is a list
                if element not in list_of_list:
                    list_of_list.append(element)

            else:  # this is a string
                if element != '':
                    set_of_string.add(element)

        # check if the string exist in any of the lists and remove them
        for string in set_of_string:
            redundant_flag = False
            for _list in list_of_list:
                if string in _list:
                    redundant_flag = True
            if not redundant_flag:
                new_list.append(string)

        new_list += list_of_list

        self.list_of_subjects_candidate = new_list

    def condition_filter(self, tagged_sentence, sentence):
        # print('tagged sentence condition filter\n\t', tagged_sentence, '\nwith sentence --- ', sentence)
        grammar = '''
                    value_condition: {<VBP|VBZ|VBD|VB><JJR><IN><CD|NN>+<NNS>|<VBP|VBZ|VBD|VB>*<DT>*<NN|NNS|NNP><JJR><IN><NN|CD|NNS>+}
                    time_condition: {<IN><CD>}
                    dt_rbs: {<DT><RBS><JJ>*<NN|NNS|NNP>*}
                    dt_jjs: {<DT><JJS><JJ>*<NN|NNS|NNP>*}
                '''
        cp = nltk.RegexpParser(grammar)
        result = cp.parse(tagged_sentence)
        # result.draw()
        # print()
        for tree in result:
            if type(tree) == nltk.Tree:
                if tree.label() == 'value_condition':
                    # print('\n\n\t\t================= value_condition Filter =================')
                    self.condition_component_scraper(tree.leaves())
                    self.filter_out_tokens(tree.leaves())
                    self.add_word_to_condition_filter_stop_words(tree.leaves())

                    # print('\t\t========================================================\n\n')

                elif tree.label() == 'time_condition':
                    # print('\n\n\t\t================= time_condition Filter =================')
                    self.time_condition_component_scraper(
                        tree.leaves())
                    self.filter_out_tokens(tree.leaves())
                    self.add_word_to_condition_filter_stop_words(tree.leaves())

                    # print('\t\t========================================================\n\n')

                elif tree.label() == 'dt_rbs':
                    # print('\n\n\t\t================= dt_rbs Filter =================')
                    self.rbs_condition_component_scraper(
                        tree.leaves())
                    self.filter_out_tokens(tree.leaves())
                    self.add_word_to_condition_filter_stop_words(tree.leaves())
                    # print('\t\t========================================================\n\n')

                elif tree.label() == 'dt_jjs':
                    # print('\n\n\t\t================= dt_jjs Filter =================')
                    self.jjs_condition_component_scraper(
                        tree.leaves())
                    self.filter_out_tokens(tree.leaves())
                    self.add_word_to_condition_filter_stop_words(tree.leaves())

                    # print('\t\t========================================================\n\n')

    def add_word_to_condition_filter_stop_words(self, tree_leaves):
        # received a tree
        for word in tree_leaves:
            self.condition_filter_stop_words.append(word[0].strip())

    #         self.condition_object = {'condition_target': '', 'JJS': '', 'RBS': '', 'Time': '', 'Types': [], 'value': 1}

    def time_condition_component_scraper(self, tree_leaves):
        # print('\t\t', tree_leaves)
        grammar = '''YEAR :{<CD>}'''
        cp = nltk.RegexpParser(grammar)
        result = cp.parse(tree_leaves)
        # result.draw()
        for tree in result.subtrees(filter=lambda x: x.label() == 'YEAR'):
            # print('\t\ttime condition : ', tree)
            self.condition_object['Time'] = tree.leaves()[0][0]
            self.condition_object['Types'].append('time')

    def rbs_condition_component_scraper(self, tree_leaves):

        grammar = '''RBS:    {<RBS>}
                     Target: {<JJ>*<NN|NNS|NNP>+}
                        '''
        cp = nltk.RegexpParser(grammar)
        result = cp.parse(tree_leaves)
        self.condition_object['Types'].append('RBS')
        for tree in result.subtrees(filter=lambda x: x.label() == 'RBS'):
            # print('\t\tRBS : ', tree)
            self.condition_object['RBS'] = tree.leaves()[0][0]
        for tree in result.subtrees(filter=lambda x: x.label() == 'Target'):
            # print('\t\tTarget : ', tree)
            self.condition_object['condition_target'] = tree.leaves()

        # print('\t\t', tree_leaves)

    def jjs_condition_component_scraper(self, tree_leaves):

        grammar = '''JJS:    {<JJS>}
                             Target: {<JJ>*<NN|NNS|NNP>+}
                                '''
        cp = nltk.RegexpParser(grammar)
        result = cp.parse(tree_leaves)
        self.condition_object['Types'].append('JJS')

        for tree in result.subtrees(filter=lambda x: x.label() == 'JJS'):
            # print('\t\tJJS : ', tree)
            _JJS = tree.leaves()[0][0]
            self.condition_object['JJS'] = tree.leaves()[0][0]

        for tree in result.subtrees(filter=lambda x: x.label() == 'Target'):
            # print('\t\tTarget : ', tree)
            _Target = tree.leaves()[0][0]
            self.condition_object['condition_target'] = _Target

        phrase_to_be_filtered = _JJS + ' ' + _Target
        self.condition_filter_stop_words.append(phrase_to_be_filtered)
        # print('\t\t', tree_leaves)

    def condition_component_scraper(self, tree_leaves):
        # The to do list here: 1. replace all number words with digits
        grammar = '''JJR: {<JJR>}
                     CD: {<CD>+<NN|NNS|NNP*>+|<CD>+}
                     NP: {<NNS|NN|NNP>}   
                        '''
        cp = nltk.RegexpParser(grammar)
        result = cp.parse(tree_leaves)
        # result.draw()
        JJR = ([word for word, pos in result.pos() if pos == 'JJR'])
        # With in the CD tree, put all the cascading number and time them together.
        CDs = ([word for word, pos in result.pos() if pos == 'CD'])
        NPs = ([word for word, pos in result.pos() if pos == 'NP'])
        # print('=== JJR ===:', JJR)
        self.condition_object['Types'].append('value')
        self.condition_object['JJR'] = JJR
        self.condition_object['NP'] = NPs

        value = 1
        original_value = ''
        there_is_value = False
        unit_target = ['']
        for cd_index, cd_word in enumerate(CDs):
            if cd_word[1] == 'CD':
                there_is_value = True
                # print('=== CD ====:', cd_word)
                if cd_index == 0:
                    original_value = cd_word[0]
                value *= float(cd_word[0])
            elif cd_word[1] == 'NNS':
                unit_target = cd_word
                # print('=== Unit/Target ===:', cd_word)
                self.condition_object['condition_target'] = cd_word

        if there_is_value:
            # print('===VALUE===:', value)
            self.condition_object['value'] = value

        # print('=== NP ====:', NPs)
        # print('====Original Value====')
        phrase_to_filter = original_value + ' ' + unit_target[0]
        self.condition_filter_stop_words.append(phrase_to_filter.lower())

    def replace_word_with_digits(self, sentence):
        temp = []
        for word in sentence.split(' '):
            try:
                new_word = w2n.word_to_num(word)
                temp.append(str(new_word))
            except ValueError:
                temp.append(word)

        return ' '.join(temp)

    def function_indicator_filter(self, tagged_sentence, sentence):
        # 1. Filter out SMA (Show me all, Give me all , List me all )
        # print('Tagged Sentence', tagged_sentence)
        # print('------------------------------')
        if tagged_sentence:
            first_word_in_sentence = tagged_sentence[0][0].lower().strip()
            if first_word_in_sentence in self.general_question_key_words:
                print('\t\t------------ GQ -------------')
                print('\t\tIdentified a general question')
                self.general_question = True
                print('\t\t-----------------------------')
        if 'list' in sentence.lower():
            grammar = '''
                    SMA:
                        }<VB>+<IN>+{
                        {<VB><PRP><PDT>*<DT>*<NN>*<IN>*<DT>*}
                        }<JJ><NN>*{
                    WHJ:{<IN>*<WP|WRB|JJ|WDT><RB|JJ>*<NN|NNS>*<VBP|VBD|VBZ|VB>*<DT>*}
                '''
        else:
            grammar = '''
                    SMA:
                        }<VB>+<IN>+{
                        {<VB><PRP><PDT>*<DT>*}
                    WHJ:{<IN>*<WP|WRB|JJ|WDT><RB|JJ>*<NN|NNS|NNP>*<VBP|VBD|VBZ|VB>*<DT>*}
                '''
        cp = nltk.RegexpParser(grammar)
        result = cp.parse(tagged_sentence)
        # result.draw()
        for tree in result:
            if type(tree) == nltk.Tree:
                if tree.label() == 'SMA' and len(tree.leaves()) > 1:
                    # print('Leaves:', tree.leaves())

                    joined_phrase = self.join_tree_into_phrases(tree.leaves(), False)
                    print('--------------------------------------------------------')
                    print('Inside SMA: new sentence |', sentence.replace(joined_phrase, ''), ' |')
                    self.SMA_Object['Target'] = sentence.replace(joined_phrase, '')
                    self.SMA_Object['SMA'] = True
                    print('--------------------------------------------------------')

                    return sentence.replace(joined_phrase, '')

                elif tree.label() == 'WHJ' and len(tree.leaves()) > 1:
                    if not ((tree.leaves()[0][1] == 'JJ') and ((tree.leaves()[1][1] == 'NN') or (
                            tree.leaves()[1][1] == 'NNS')) and (len(tree.leaves()) == 2)):
                        # print('\n\t\tWHJ: ', self.join_tree_into_phrases(tree.leaves(), False))
                        self.scrape_out_predicates_and_target_classes_out_of_whj(tree.leaves())
                        self.filter_out_tokens(tree.leaves())
                        # return sentence.replace(joined_phrase, '')

    def scrape_out_predicates_and_target_classes_out_of_whj(self, tree_leaves):
        scrape_grammar = '''

                      target_class:
                        {<NN|NNP|NNS>+}

                      general_question:
                        {<WP|WRB><VBD>*<JJ>*}

                      class_question:
                        {<WDT>}  

                      condition:
                        {<JJ>}

                      predicate:
                        {<VBD|VBZ|VBP>} 


                          '''
        cp = nltk.RegexpParser(scrape_grammar)
        result = cp.parse(tree_leaves)
        stop = set(stopwords.words('english'))
        stop.update(['did', 'do', 'does', 'which'])
        # result.draw()
        # print('\t\t================= WHJ Scraper =================')
        for tree in result:
            if type(tree) == nltk.Tree:
                if tree.label() == 'predicate':
                    # print('\t\tpredicate: ', tree.leaves())
                    print()
                elif tree.label() == 'condition':
                    # print('\t\tcondition: ', tree.leaves())
                    self.general_condition = tree_leaves
                    self.filter_out_tokens(tree.leaves())

                elif tree.label() == 'target_class':
                    # print('\t\ttarget_class: ', tree.leaves())
                    self.target_class = tree.leaves()
                    self.filter_out_tokens(tree.leaves())
                    self.question_type_recogizer(tree_leaves)

                elif tree.label() == 'class_question':
                    # print('\t\tclass_question: ', tree.leaves())
                    self.filter_out_tokens(tree.leaves())
                    self.question_type_recogizer(tree_leaves)

                elif tree.label() == 'general_question':
                    # print('\t\tgeneral_question: ', tree.leaves())
                    self.identify_hm_question(tree.leaves())
                    self.question_type_recogizer(tree_leaves)
                    self.filter_out_tokens(tree.leaves())

        # print('\t\t===============================================\n\n')
        # TODO: identify what answer does it expect
        # result.draw()

    def identify_hm_question(self, tree_leaves):
        temp = []
        temp2 = []
        for word in tree_leaves:
            temp.append(word[0].strip().lower())
            temp2.append(word[1].strip())
        phrase = ''.join(temp).strip()
        pos = ''.join(temp2).strip()
        if phrase == 'howmany':
            self.hmq_object = {'type': 'howmany', 'HM': True}
        elif pos == 'WRBJJ':
            self.hmq_object = {'type': 'howjj', 'HM': True, 'JJ': temp[1].strip().lower()}

    def question_type_recogizer(self, tree_leaves):

        # print('\n\n\t\t\t\t=========== question_type_recogizer ==============')
        for word in tree_leaves:
            if word[0].strip().lower() == 'when':
                #    print('\t\t\t\tYou are expecting a date')
                self.expected_class = 'Date'
            elif word[0].strip().lower() == 'who':
                #   print('\t\t\t\tYou are expecting a person')
                self.expected_class = 'Person'

            elif word[0].strip().lower() == 'whom':
                #  print('\t\t\t\tYou are expecting a person')
                self.expected_class = 'Person'

            elif word[0].strip().lower() == 'where':
                # print('\t\t\t\tYou are expecting a place')
                self.expected_class = 'Place'

            elif word[0].strip().lower() == 'what':
                # print('\t\t\t\tYou are expecting a thing')
                self.expected_class = 'Thing'

        # print('\t\t\t\t==================================================\n\n')

    def filter_out_tokens(self, leaves):
        for word in leaves:
            self.stop_words.append(word[0].lower().strip())

    def clean_a_list(self, _list):

        # print('in clean a list', _list)
        stop = set(stopwords.words('english'))
        stop.update(['did', 'do', 'does', 'which', 'that', 'place'])
        new_list = []
        new_list_2 = []
        for item in _list:
            if type(item) == type(_list):
                # this is a list of tuples
                if item[0]:
                    if type(item[0]) == type(('a', 'b')):
                        a_phrase = (self.join_tree_into_phrases(item, True))
                        if (a_phrase not in new_list) and len(item) > 0:
                            new_list.append(a_phrase)
                    else:
                        new_list_2.append(item)



            elif type(item) == str:
                if (item not in new_list) and (item.strip().lower() not in stop) and len(item) > 0:
                    new_list.append(item)

        for phrase in new_list:
            array = phrase.split(' ')
            temp = []
            for word in array:
                if word.strip().lower() not in self.stop_words:
                    temp.append(word.strip())
            new_phrase = ' '.join(temp)
            new_list_2.append(new_phrase.strip())

        # print('new_list_2', new_list_2)
        return new_list_2

    def join_triple_of_triple(self):
        return ''

    def make_map_of_tokens(self, _tokens):
        # print(_tokens)
        map = {}
        for _i, token in enumerate(_tokens):
            map[token] = _i
        return map

    def remove_redundant_in_triple_from_condition_filter(self):
        # print('condition_filter_stop_words', self.condition_filter_stop_words)
        for triple_index, triple in enumerate(self.list_of_NIN_pairs):
            raw_triple = triple
            triple = triple['triple']
            for element_index, triple_element in enumerate(triple):
                # if triple_element is a list, else it is a string
                if type(triple_element) == type(('a', 'b')):
                    # print('tuple', triple_element)
                    print('')
                elif type(triple_element) != type([]):
                    # print(type(triple_element), triple_element)
                    if element_index == 1 and (
                            triple_element.strip().lower() == 'more' or triple_element.strip().lower() == 'less' or triple_element.strip().lower() in self.jjr_stop_words):
                        self.list_of_NIN_pairs.remove(raw_triple)
                    else:
                        for stop_word in self.condition_filter_stop_words:
                            if triple_element.strip().lower() == stop_word.strip().lower():
                                # print('condition within a triple', raw_triple, element_index)
                                JJS_word = self.condition_object['JJS'].strip()
                                triple_object = {'text': triple_element.replace(JJS_word, '').strip(),
                                                 'condition_object': self.condition_object}

                                _triple = self.list_of_NIN_pairs[triple_index]['triple']
                                _triple_list = list(_triple)
                                _triple_list[element_index] = triple_object
                                self.list_of_NIN_pairs[triple_index]['triple'] = tuple(_triple_list)
                                # TODO：now delay this process
                                # print('triple_object', triple_object)

    def join_cascading_predicates(self):
        for predicate_word in self.list_of_predicates_candidate:
            predicate_word_index = self.find_index_of_word(predicate_word)
            # iterate through triples and find those having predicates, check for cascading predicates
            for list_index, subject_element_list in enumerate(self.list_of_subjects_candidate):
                if type(subject_element_list) == type([]):
                    for element_index, subject_element in enumerate(subject_element_list):
                        if type(subject_element) != type('x'):
                            if subject_element['predicate']:
                                predicate_in_subject_index = self.find_index_of_word(subject_element['predicate'])
                                if abs(predicate_word_index - predicate_in_subject_index) == 1:
                                    # print('p1', predicate_word, 'p2', subject_element['predicate'])
                                    if predicate_word_index > predicate_in_subject_index:
                                        new_predicate = subject_element['predicate'] + ' ' + predicate_word
                                        print('new predicate', new_predicate)
                                    else:
                                        new_predicate = predicate_word + ' ' + subject_element['predicate']
                                        print('new predicate', new_predicate)

                                    # remove word from predicate, update the one in subjects
                                    subject_element['predicate'] = new_predicate
                                    self.list_of_predicates_candidate.remove(predicate_word)

        for predicate_word_1 in self.list_of_predicates_candidate:
            for predicate_word_2 in self.list_of_predicates_candidate:
                index_1 = self.find_index_of_word(predicate_word_1)
                index_2 = self.find_index_of_word(predicate_word_2)
                if abs(index_1 - index_2) == 1:
                    if index_1 > index_2:
                        _new_phrase = [predicate_word_2, predicate_word_1]
                    else:
                        _new_phrase = [predicate_word_1, predicate_word_2]

                    self.list_of_predicates_candidate.remove(predicate_word_1)
                    self.list_of_predicates_candidate.remove(predicate_word_2)
                    self.list_of_predicates_candidate.append(_new_phrase)
                # remove both predicate_words

    def find_index_of_word(self, word):
        word = word.strip().lower()
        _sentence = self.original_sentence
        # print('original sentence ', _sentence)
        index = -9
        _sentence_array = _sentence.split(' ')
        for i, _word in enumerate(_sentence_array):
            if word == _word:
                return i

        return index

    def remove_general_question_indicator_from_sentence(self, tokens):

        _temp = []
        for token in tokens:
            if token.strip().lower() not in self.general_question_key_words:
                _temp.append(token)

        return _temp

    def identify_parallel_subjects(self):
        # for triple and for sub list
        print()  # both in subject list
        # one in subject list, one in triple
        # both in triple list
        # 1. Both in sujects

        sub_strings_list = []
        for sub_idx, sub in enumerate(self.list_of_subjects_candidate):
            if type(sub) == type([]):
                # sub_string = sub[0]
                there_is_object = False
                for s in sub:
                    if type(s) == type({}):
                        sub_strings_list.append(
                            {'type': 'sub_list', 'string': s['text'], 'index': sub_idx, 'has_predicate': True})
                        there_is_object = True
                if not there_is_object:
                    sub_string = sub[0]
                    sub_strings_list.append(
                        {'type': 'sub_single', 'string': sub_string, 'index': sub_idx, 'has_predicate': False})

            else:
                sub_string = sub
                sub_strings_list.append(
                    {'type': 'sub_single', 'string': sub_string, 'index': sub_idx, 'has_predicate': False})

        for idx, triple in enumerate(self.list_of_NIN_pairs):
            triple_pair = triple['triple']
            # print('triple_pair --', triple_pair)
            for element_idx, single_element in enumerate(triple_pair):
                if type(single_element) == type('x'):
                    sub_strings_list.append(
                        {'type': 'triple_string', 'string': single_element, 'index': [idx, element_idx]})
                elif type(single_element) == type([]):  # a list
                    sub_strings_list.append(
                        {'type': 'triple_list', 'string': single_element[0], 'index': [idx, element_idx]})
                elif type(single_element) == type({'a': 'b'}):  # a obj
                    sub_strings_list.append(
                        {'type': 'triple_obj', 'string': single_element['text'], 'index': [idx, element_idx]})

        # print()
        # print('sub_strings_list', sub_strings_list)
        # print()

        # print()
        self.find_index_of_phrases_and_return_what_is_in_between_and_join_them(sub_strings_list)

    def find_index_of_phrases_and_return_what_is_in_between_and_join_them(self, sub_string_list):
        there_is_triple = False
        one_contain_predicate = False

        chosen_triple = ''
        chosen_sub = ''

        chosen_element = ''

        # self.collection_of_cascading_subjects = set()
        if len(sub_string_list) >= 2:
            length = len(sub_string_list)
            for idx, sub_string in enumerate(sub_string_list):
                string = sub_string['string']
                if idx < (length - 1):
                    the_next_sub = sub_string_list[idx + 1]
                    self.find_what_is_in_between(string, the_next_sub['string'],
                                                 sub_string, the_next_sub)
        # print()
        # print('-----------------------------------------------------------------------')
        # print('three_is_triple', three_is_triple)
        # print('collection_of_cascading_subjects', collection_of_cascading_subjects)
        # print('index_map', index_map)
        # print('-----------------------------------------------------------------------')
        # print()
        time.sleep(1)
        # print('important', self.collection_of_cascading_subjects)
        for an_element in self.collection_of_cascading_subjects:
            # print()
            obj = self.index_map[an_element]
            # print('according obj', obj)
            # print('string ', obj['string'])
            # print('type ', obj['type'])
            _xtype = obj['type']
            # print('--- element type ---', _xtype)
            if _xtype.startswith('triple'):
                _index = obj['index']
                # print('index: ', _index)
                idx_1 = _index[0]
                idx_2 = _index[1]
                chosen_triple = self.list_of_NIN_pairs[idx_1]['triple'][idx_2]
                # print('the chosen triple', chosen_triple)
                there_is_triple = True
            elif _xtype.startswith('sub'):
                _sub_index = obj['index']
                one_contain_predicate = obj['has_predicate']
                chosen_sub = self.list_of_subjects_candidate[_sub_index]
            # print('-------------------------------------')

        if self.collection_of_cascading_subjects:
            print('it exist')
            if there_is_triple:
                # remove chosen one from the set
                chosen_element = chosen_triple
            else:
                if one_contain_predicate:
                    for sub in chosen_sub:
                        if type(sub) == type({}):
                            chosen_element = sub['text']
                else:
                    chosen_element = self.collection_of_cascading_subjects.pop()
                    self.collection_of_cascading_subjects.add(chosen_element)

            # print('chosen element', chosen_element)
            # print('new collection ', self.collection_of_cascading_subjects)

            # print()
            # print('\t\t==============================================')
            # print('\t\t The type we are facing: ', self.index_map[chosen_element])

            # update the sub in chosen_element first
            _type_of_chosen_element = self.index_map[chosen_element]['type']
            if _type_of_chosen_element.startswith('triple'):
                # this is a triple  string list obj
                if 'string' in _type_of_chosen_element:
                    # print('x-triple-string-')
                    _idxs = self.index_map[chosen_element]['index']
                    _idx_1 = _idxs[0]
                    _idx_2 = _idxs[1]
                    _tuple = self.list_of_NIN_pairs[_idx_1]['triple']
                    _list_tuple = list(_tuple)
                    _list_tuple[_idx_2] = list(self.collection_of_cascading_subjects)
                    self.list_of_NIN_pairs[_idx_1]['triple'] = tuple(_list_tuple)

                elif 'list' in _type_of_chosen_element:
                    # print('x-triple--list--')
                    print()
                elif 'obj' in _type_of_chosen_element:
                    # print('x-triple--obj---')
                    print()
            else:
                # the chosen one is  list, single
                if 'list' in _type_of_chosen_element:
                    # print('x-sub-list')
                    _idx = self.index_map[chosen_element]['index']
                    _list = self.list_of_subjects_candidate[_idx]
                    self.collection_of_cascading_subjects.remove(chosen_element)
                    _the_ones_that_is_not_chosen = list(self.collection_of_cascading_subjects)
                    self.collection_of_cascading_subjects.add(chosen_element)

                    for _i, _s in enumerate(_list):
                        if type(_s) == type({}):
                            _s['text'] = list(self.collection_of_cascading_subjects)

                        else:
                            # print('Look at me', _s)
                            new_list = [_s] + _the_ones_that_is_not_chosen
                            _list[_i] = new_list

                elif 'single' in _type_of_chosen_element:
                    _single_index = self.index_map[chosen_element]['index']
                    self.list_of_subjects_candidate[_single_index] = list(self.collection_of_cascading_subjects)

            self.collection_of_cascading_subjects.remove(chosen_element)
            for key in self.collection_of_cascading_subjects:
                # in sub_list
                # in triples
                to_be_removed_obj = self.index_map[key]
                to_be_removed_obj_type = to_be_removed_obj['type']
                if to_be_removed_obj_type.startswith('triple'):
                    # print('to_be_removed_obj', to_be_removed_obj)
                    print()
                elif to_be_removed_obj_type.startswith('sub'):
                    # print('to_be_removed_obj', to_be_removed_obj)
                    self.list_of_subjects_candidate.remove(to_be_removed_obj['string'])

    def find_what_is_in_between(self, phrase, next_phrase, obj, next_obj):
        _phrase = phrase.lower().strip()
        _next_phrase = next_phrase.lower().strip()

        sample_obj = {'type': 'triple_triple_list', 'index': [1, 3]}
        if (_phrase in self.original_sentence) and (_next_phrase in self.original_sentence):
            len_1 = len(_phrase)
            idx_1 = self.original_sentence.index(_phrase)
            len_2 = len(_next_phrase)
            idx_2 = self.original_sentence.index(_next_phrase)
            length_map = {idx_1: len_1, idx_2: len_2}
            end_idx = max(idx_1, idx_2)
            start_idx = min(idx_1, idx_2)
            offset = length_map[start_idx]

            word_in_between = self.original_sentence[start_idx + offset: end_idx].strip().lower()

            if word_in_between in self.connect_words:
                self.collection_of_cascading_subjects.add(phrase)
                self.index_map[phrase] = obj
                self.collection_of_cascading_subjects.add(next_phrase)
                self.index_map[next_phrase] = next_obj
        return 'good'
        # print(self.original_sentence.index(sub_string))

    def assign_predicates_to_subjects(self):
        for predicate in self.list_of_predicates_candidate:
            # iterate through the list of subjects:
            # it must be a list:
            for element_index, element_in_sub_list in enumerate(self.list_of_subjects_candidate):
                if type(element_in_sub_list) == type([]):
                    if len(element_in_sub_list) == 2:
                        if predicate in element_in_sub_list[0]:
                            _text = element_in_sub_list[1]
                            obj = {'text': _text, 'predicate': predicate}
                            element_in_sub_list[1] = obj
                            # remove it from the predicate
                            if predicate in self.list_of_predicates_candidate:
                                self.list_of_predicates_candidate.remove(predicate)

        for _predicate in self.list_of_predicates_candidate:
            # iterate through the triples
            for triple in self.list_of_NIN_pairs:
                triple = triple['triple']
                for element in triple:
                    if type(element) == type([]):
                        if len(element) == 2:
                            if _predicate in element[0]:
                                _text = element[1]
                                obj = {'text': _text, 'predicate': _predicate}
                                element[1] = obj
                                if predicate in self.list_of_predicates_candidate:
                                    self.list_of_predicates_candidate.remove(_predicate)
                                    # print('removed predicate')

    def test_one_sentence(self, sentence):
        sentence = sentence.replace('?', '')
        sentence = sentence.replace('.', '')
        # Replace all number words to digits
        self.original_sentence = sentence.lower().strip()

        sentence = self.replace_word_with_digits(sentence)
        # filter out the useless in the world phrase
        sentence = sentence.replace('in the world', '').strip()

        train_sents = conll2000.chunked_sents('train.txt', chunk_types=['NP'])
        bigram_chunker = NLTK_Chunker.UnigramChunker(train_sents)

        tokens = nltk.word_tokenize(sentence)

        self.map_of_tokens = self.make_map_of_tokens(tokens)
        for t in tokens:
            if t.lower().strip() == 'both':
                tokens.remove(t)

        print('tokens ', tokens)
        tagged = nltk.pos_tag(tokens)
        _temp = []
        # print('tagged sentence', tagged)
        for tag in tagged:
            if tag[0].lower().strip() == 'which' and tag[1] == 'JJ':
                new_tag = ('which', 'WDT')
                _temp.append(new_tag)

            elif tag[0].lower().strip() == 'rivers' and tag[1] == 'VBZ':
                new_tag = ('rivers', 'NNP')
                _temp.append(new_tag)

            elif tag[0].lower().strip() == 'which' and tag[1] == 'NNP':
                new_tag = ('which', 'WDT')
                _temp.append(new_tag)

            elif tag[0].lower().strip() == 'princess' and tag[1] == 'VB':
                new_tag = ('princess', 'NNS')
                _temp.append(new_tag)

            elif tag[0].lower().strip() == 'die' and tag[1] == 'NN':
                new_tag = ('die', 'VB')
                _temp.append(new_tag)

            elif tag[0].lower().strip() == 'languages' and tag[1] == 'VBZ':
                new_tag = ('languages', 'NNP')
                _temp.append(new_tag)

            elif tag[0].lower().strip() == 'states' and tag[1] == 'VBZ':
                new_tag = ('states', 'NNP')
                _temp.append(new_tag)

            else:
                _temp.append(tag)

        tagged = _temp
        self.condition_filter(tagged, sentence)

        s = self.function_indicator_filter(tagged,
                                           sentence)  # return a new sentence that function indicators are filtered out
        if s:
            tokens = nltk.word_tokenize(s)
            tagged = nltk.pos_tag(tokens)

        new_tokens = []
        if tokens[0].lower().strip() in self.general_question_key_words:
            tokens = tokens[1:]

        new_tagged = nltk.pos_tag(tokens)

        # TODO: filter out general question verb

        ne = nltk.ne_chunk(new_tagged)
        # ne.draw()

        recongized_ne = []
        collected_ne = []
        for i, tree in enumerate(ne):
            if type(tree) == nltk.Tree:
                if (tree.label() == 'PERSON' or 'GPE' or 'LOCATION') and (tree.leaves() not in collected_ne):
                    # new starter ne spotted, create new tree in order to hold it
                    new_tree_leaves = tree.leaves()
                    index = i
                    while index < len(ne) - 1:
                        index = index + 1
                        if type(ne[index]) == nltk.Tree:
                            if ne[index].label() == 'PERSON' or 'GPE' or 'LOCATION':
                                # print('This is a cascading NE', ne[index].leaves())
                                new_tree_leaves += ne[index].leaves()
                                collected_ne.append(ne[index].leaves())
                            else:
                                break
                        else:
                            break
                    # DONE : Join cascading Entity
                if Tree('NE', new_tree_leaves) not in recongized_ne:
                    recongized_ne.append(Tree('NE', new_tree_leaves))

        result = bigram_chunker.parse(tagged)
        # result.draw()
        for j, tree in enumerate(result):
            if type(tree) == nltk.Tree:
                add_to_subject_candidates = True
                # case 1: 'sB or A
                # if tree.leaves()[0][1] == 'POS':  # 'sB
                #     add_to_subject_candidates = False
                #
                # if j < len(result) - 1:
                #     if type(result[j + 1]) == nltk.Tree:
                #         if (result[j + 1][1] == 'NP') and result[j + 1].leaves()[0][1] == 'POS':  # A
                #             add_to_subject_candidates = False
                #
                #     if type(result[j + 1]) != nltk.Tree:
                #         if result[j + 1][1] == 'IN':  # next NP is after of , this is A of B - A
                #             add_to_subject_candidates = False
                #
                # if j - 1 > 0:
                #     if type(result[j - 1]) == nltk.Tree and (result[j - 1][1] == 'NP') and result[j - 1].leaves()[0][
                #         1] == 'POS':  # B
                #         if result[j - 1][1] == 'IN':  # next NP is after of , this is A of B - B
                #             add_to_subject_candidates = False

                # if add_to_subject_candidates:

                if recongized_ne:
                    filtered = self.filter_named_entity(recongized_ne, tree)['filtered']
                    if not filtered:
                        self.list_of_subjects_candidate.append(tree.leaves())

                else:
                    self.list_of_subjects_candidate.append(tree.leaves())

                # recoginze_JJS(tree)

                # Done: recognize NP POS NP triple
                if tree.label() == 'NP' and result[j - 1]:
                    if type(result[j - 1]) == nltk.Tree and tree.leaves()[0][1] == 'POS':
                        # print('A NPN triple is recognized: ', result[i - 1], '-', result[i])
                        npn_triple = (
                            self.filter_named_entity(recongized_ne, result[j - 1])['joined_tree'],
                            self.filter_named_entity(recongized_ne, result[j])['joined_tree'])
                        # TODO: now , deal with parallel triple
                        pos_word = tree.leaves()[0][0].strip().strip()
                        self.predefined_stop_words.append(pos_word)
                        self.list_of_NIN_pairs.append({'type': 'POS', 'triple': npn_triple})
            else:
                # Done: recognize NN IN NN triple
                if 1 <= j < len(result) - 1:
                    if tree[1] == 'IN' and result[j + 1] and result[j - 1]:
                        # print('here')
                        _pos_word = tree[0].strip().lower()
                        self.predefined_stop_words.append(_pos_word)
                        if type(result[j + 1]) == nltk.Tree and type(result[j - 1]) == nltk.Tree and j >= 1:
                            # print('A NIN triple is recognized: ', result[i - 1], '-', result[i], '-', result[i + 1])

                            if self.filter_named_entity(recongized_ne, result[j + 1]):
                                npn_triple = (self.filter_named_entity(recongized_ne, result[j + 1])['joined_tree'],
                                              self.filter_named_entity(recongized_ne, result[j - 1])['joined_tree'])
                                self.list_of_NIN_pairs.append({'type': 'NIN', 'triple': npn_triple})

                # if j + 1 < len(result):
                # print('The next possible predicate', result[j + 1])

                # TODO now: Predicate should be able to add cascading feature
                self.list_of_predicates_candidate.append(tree)

            # TODO: recognize 'In which films did Julia Roberts as well as Richard Gere play?'

            # TODO: recognize 'How many people live in Poland', translate How many people to population???

    def get_one_question(self, _questions):
        selected_question = random.choice(_questions)
        _questions.remove(selected_question)
        # print(random.choice(questions))
        return selected_question['question'][0]['string']

    def test(self):
        s1 = 'Did Park_Chin_Chong ever win Grand Prix at Cannes'
        s2 = '''Is President Lincoln's wife called Mary'''
        s2_5 = '''Was the wife of President Lincoln called Mary'''
        s3 = '''Which indian company has most employees'''
        s4 = '''Who died in 1944 and have dated JFK'''
        s5 = '''Who wrote the Game of Thrones theme'''
        s6 = '''Show me all books in Asimov's Foundation series'''
        s7 = '''Give me all federal chancellors of Germany'''
        s8 = '''Who is the author of The Hitchhiker's Guide to the Galaxy'''
        s9 = '''Answer to the Ultimate Question of Life, the Universe, and Everything'''
        s10 = '''Show me all museums in London'''
        s11 = '''What form of government does Russia have?'''
        s12 = '''What was the original occupation of the inventor of Lego'''
        s13 = '''Whom did Lance Bass marry?'''
        s14 = '''What movies does Jesse Eisenberg play in'''
        s15 = '''Which states border Illinois?'''
        s16 = '''Does Neymar play for Real Madrid?'''
        s17 = '''Whom did Lance Bass marry?'''
        s18 = '''What is the longest river in China'''
        s19 = '''How did Michael Jackson die?'''
        s20 = '''Show me all the South American Countries'''
        s21 = '''Where is Nanyang Technological University Singapore'''
        s22 = '''Where is Syngman Rhee buried?'''
        s23 = '''What are the ingredients of chocolate chip cookies'''
        s24 = '''Show all the South American Countries'''
        s25 = '''List all South American Countries'''
        s26 = '''Give me all South American Countries'''
        s27 = '''Show me a list of South American Countries'''
        s28 = '''Give me a list of all critically endangered birds.'''
        s29 = '''Which presidents were born in 1945'''
        s30 = '''Which country has the most official languages?'''
        s31 = '''What is the largest country in the world?'''
        s32 = '''How short is the shortest NBA player?'''
        s33 = '''Show me all basketball players that are higher than 2 meters.'''
        s34 = '''Give me a list of all Canadians that reside in the U.S.'''
        s35 = '''Which countries have more than ten volcanoes?'''
        s36 = '''Which Temperature sensor has a value more than 30 degrees'''
        s37 = '''What country has a population more than 5 billion'''
        s38 = '''Show me Hemingway's autobiography'''
        S39 = '''What is Batman's real name?'''
        s40 = '''What is the name of the wife of the first president of the United States '''
        s41 = '''What is China's president's term limit'''
        s42 = '''What is president of China's first name'''
        s43 = '''Is Russia the largest country in the world?'''
        s45 = '''Did Elvis Presley have children'''
        s47 = '''Did Elvis Presley directed any movie'''
        s48 = '''What is the highest mountain in China'''
        s50 = '''Which continent have less than 10 countries?'''
        s51 = '''Who produced the most films'''
        s52 = '''What is the biggest stadium in Spain'''
        s53 = '''How many rivers are there in Europe?'''
        s54 = '''How many Albums does Queen have'''
        s55 = '''How many mountains in California have elevation higher than 1000 meters '''
        s56 = '''How long is the longest river in Africa'''

        s57 = '''In which films did Julia Roberts as well as Richard Gere play?'''
        s58 = '''Who is the son of Sonny and Cher?'''
        s59 = '''What is the longest river in both Africa and Asia'''
        s60 = '''What is the longest river in Africa and Asia and Europe'''
        s61 = '''Who is the son of Michael and Jone'''
        s62 = '''Which movies starring both Michael and Jone?'''
        s63 = '''When did the Boston Tea Party take place?'''
        s64 = '''How many Islands are there in Singapore'''
        s65 = '''How many skyscrapers in United States that have a height higher than 200 meters'''
        s66 = '''Show me all skyscrapers in United States that have a height higher than 200 meters'''

        show_me_all_training_set = [s28, s20, s10, s6, s24, s25, s26, s27]
        condition_training_set = [s37, s40, s48, s29, s30, s31, s18, s33, s35, s36, s41, s42]
        sub_sentence = [s33, s35, s36, s37]
        triple_training_set = [s41, s42, s12, s6, s38, S39, s40]
        general_question_set = [s2, s47, s16, s43]
        how_question_training_set = [s41, s55, s48, s56, s18, s32, s53, s54, s19, s64]
        parallel_training_set = [s62, s57, s58, s59, s60, s61]

        how_many_question_training_set = [s66, s65, s55, s64, s54, s53]

        # test_one_sentence(get_one_question())
        # test_one_sentence(s5)
        st = StanfordNERTagger('english.muc.7class.distsim.crf.ser.gz')
        with open('./Training.json') as file:
            obj = json.loads(file.read())
            questions = obj['questions']

        for text in how_many_question_training_set:
            # for i in range(0, 1):
            # print('')
            print('================================================================')

            self.list_of_subjects_candidate = []
            self.list_of_predicates_candidate = []
            self.list_of_NIN_pairs = []
            self.list_of_function_indicators = []
            self.list_of_subjects_from_NIN_pairs = []
            self.list_of_suspicious_verb = []
            self.stop_words = ['place']
            self.condition_filter_stop_words = []
            self.map_of_tokens = {}
            self.hmq_object = {}
            self.condition_object = {'condition_target': '', 'JJS': '', 'RBS': '', 'Time': '', 'Types': [],
                                     'value': -85564251}

            self.collection_of_cascading_subjects = set()
            self.index_map = {}

            self.target_class = ''
            self.expected_class = ''
            self.general_condition = ''
            self.general_question = False
            self.SMA_Object = {'SMA': False, 'Target': ''}

            # text = self.get_one_question(questions)
            # text = s63
            # text = input('Ask me a question')
            # text = 'How many mountains in Asia that are higher than 4000 meters'
            print('########## -- ', text, ' -- ##########')
            tokenized_text = word_tokenize(text)
            classified_text = st.tag(tokenized_text)
            # print(classified_text)
            self.test_one_sentence(text)

            print('\n\n\t\t| ======================================================')
            # print('\t\t|stopwords\n\t\t', self.stop_words)

            self.list_of_subjects_candidate = self.clean_a_list(self.list_of_subjects_candidate)
            self.remove_redundant_from_sub()
            self.clean_triple_list()
            # print('\t\t|triples\n\t\t|', self.list_of_NIN_pairs)

            # TODO: remove redundant elements in sub
            # TODO: implement a soft core filter to predicate
            self.remove_redundant_in_sub_from_triple()
            self.remove_redundant_from_predicates()
            self.assign_predicates_to_subjects()
            self.remove_redundant_in_triple_from_condition_filter()

            self.join_cascading_predicates()

            self.identify_parallel_subjects()

            # print('\t\t|stop_words\n\t\t|', self.stop_words)

            # print('\t\t|condition_filter_stop_words\n\t\t|', self.condition_filter_stop_words)
            print('\t\t|SMA\n\t\t|', self.SMA_Object)
            print('\t\t|HMQ\n\t\t|', self.hmq_object)
            print('\t\t|is_general_question\n\t\t|', self.general_question)
            print('\t\t|general_condition\n\t\t|', self.general_condition)
            print('\t\t|expected_class\n\t\t|', self.expected_class)
            print('\t\t|target_class\n\t\t|', self.target_class)
            print('\t\t|subjects\n\t\t|', self.list_of_subjects_candidate)
            print('\t\t|predicates\n\t\t|', self.list_of_predicates_candidate)
            print('\t\t|triples\n\t\t|', self.list_of_NIN_pairs)
            print('\t\t|subjects from NIN\n\t\t|', self.list_of_subjects_from_NIN_pairs)
            print('\t\t|condition_object\n\t\t|', self.condition_object)
            print('\t\t|================================================================')
            print('')

            nlq = JPS_NLQ_test.NLQ(self.hmq_object, self.list_of_NIN_pairs, self.target_class, self.condition_object, self.SMA_Object)
            nlq.execute()
            # time.sleep(10)


# TODO: make a prototype that can answer the target class question.
if __name__ == '__main__':
    chunker = Chunker()
    chunker.test()
