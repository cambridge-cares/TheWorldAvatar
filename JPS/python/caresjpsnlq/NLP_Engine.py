from nltk.corpus import conll2000
from nltk.corpus import stopwords
import nltk
from nltk import Tree

from NLP_Toolbox import toolbox

import NLTK_Chunker


class NLP_Engine:
    def __init__(self):
        pass

    def bigram_chunk_sentence(self, tags):
        train_sents = conll2000.chunked_sents('train.txt', chunk_types=['NP'])
        bigram_chunker = NLTK_Chunker.UnigramChunker(train_sents)
        return bigram_chunker.parse(tags)

    def top_pattern_recognizer(self, bigram):
        """return a tree structure that contains all the potential subjects and the according relationships"""
        bigram = self.internal_bigram_rectifier(bigram)
        bigram = self.external_bigram_rectifier(bigram)
        top_grammar = '''
                         NINVSNIN: {<NP>+<IN>*<NP>*<VS><NP>+<IN>*<NP>*<IN>*<NP>*}
                         NINCCNIN: {<NP>+<IN>*<NP>*<CC><NP>+<IN>*<NP>*}
                         NINDIVNIN: {<NP>+<IN>*<NP>*<DIV><NP>+<IN>*<NP>*}
                         NINCHAINCHAIN: {<NP>+<IN><NP>+<IN><NP>+<IN><NP>+}
                         NINCHAIN: {<NP>+<IN><NP>+<IN><NP>+}
                         NIN:      {<NP><IN><NP>} 
                         NPCHAIN:  {<NP><NP>+}
                         SINGLENP: {<NP>}
                         REV_PRE:  {<VBN><IN>}
                         PRE:      {<VB|VBN|VBP|VBD|VBZ>}'''

        higher_level_grammar = '''
                        SPO: {<SINGLENP|NIN|NINCHAIN|NINCHAINCHAIN><PRE|REV_PRE><SINGLENP|NIN|NINCHAIN|NINCHAINCHAIN>}
                        PO: {<PRE|REV_PRE><SINGLENP|NIN|NINCHAIN|NINCHAINCHAIN>}
                                   '''

        result = toolbox.parse(top_grammar, bigram)
        result = self.question_filter(result)
        result = self.general_filter(result)
        result = self.higher_level_parser(higher_level_grammar, result)
        return result

        # Who is the number of power plants versus the gdp total of all countries
        # What is the longest river in China
        # What chemical product is produced by Biodiesel Plant 3

    # in order to deal with the number of power plants (CI NP pattern, outside the pattern)
    def internal_bigram_rectifier(self, bigram):
        np_internal_grammar = '''FUNC_JJS: {<DT><JJS>}
                                 FUNC_TARGET: {<WP|WDT><NP|NN>+}
                                 '''
        for np in [np for np in bigram if toolbox.is_tree(np)]:
            result = toolbox.parse(np_internal_grammar, np)
            if result != np:  # this indicates FUNC_TARGET is spotted
                labels = [sub_tree.label() for sub_tree in result if toolbox.is_tree(sub_tree)]
                print('labels', labels)
                if 'FUNC_TARGET' in labels:  # only FUNC_TARGET need to seperate noun from FUNC
                    if [tag_name[1] for tag_name in result.leaves() if tag_name[1].startswith('NN')]:
                        # if there is noun within the tree, seperate it and make it NP
                        result = toolbox.separate_noun_in_tree(result)
                        result.set_label('NP')
                bigram = toolbox.update_element_within_tree(np, result, bigram)
        return bigram

    # in order to deal with the longest river in China (NP(DT JJS NN, inside the pattern))
    def external_bigram_rectifier(self, bigram):
        np_external_grammar = '''NP: {<FUNC_CI|FUNC_ALL|FUNC_SUM>+<NP>}
                                 FUNC_JJS: {<DT><JJS>}
                                 '''
        result = toolbox.parse(np_external_grammar, bigram)
        return result

    def question_filter(self, main_tree):
        for tree in main_tree:
            if toolbox.is_tree(tree):
                if tree.label() == 'SINGLENP':
                    sub = toolbox.get_the_only_sub_tree(tree)
                    question_result = toolbox.question_identifier(sub)
                    is_question = question_result[0]
                    expectation = question_result[1]
                    if is_question:
                        main_tree.remove(tree)
                        main_tree.insert(0, Tree('QI', (expectation, 'EXP')))
            else:
                question_result = toolbox.question_identifier(tree)
                is_question = question_result[0]
                expectation = question_result[1]
                if is_question:
                    main_tree.remove(tree)
                    main_tree.insert(0, Tree('QI', (expectation, 'EXP')))

                else:
                    main_tree.remove(tree)

        return main_tree

    def general_filter(self, main_tree):
        """Filters out Pre or NP that are in stop word list"""
        temp = [sub_tree for sub_tree in main_tree if toolbox.is_tree(sub_tree)]
        pres = [element for element in temp if element.label() == 'PRE']
        stop = set(stopwords.words('english'))
        for pre in pres:
            word = pre[0][0]
            if word in stop:
                main_tree.remove(pre)
        return main_tree

    def higher_level_parser(self, grammar, tree):
        """It gathers components on a higher level"""
        return toolbox.parse(grammar, tree)

    @staticmethod
    def nin_pattern_recognizer(tree):
        nin_grammar = '''NIN:{<NP><IN><NP>} '''
        return toolbox.parse(nin_grammar, tree)

