import NLQ_Preprocessor as preProcessor
import NLP_Engine as nlpEngine
import NLQ_Interpreter as interpreter
import nltk
import time



class NLQ_Chunker:
    def __init__(self):

        self.preprocessor = preProcessor.PreProcessor()
        self.nlp_engine = nlpEngine.NLP_Engine()
        self.interpreter = interpreter.Interpreter()


    def chunk_a_sentence(self, sentence):

        sentence = self.preprocessor.replace_special_words(sentence)['sentence']
        # this method returns an object {'sentence': xxxx, 'origional_sentence': xxxx}
        tokens = self.preprocessor.filter_tokens_result(nltk.word_tokenize(sentence))
        tags = self.preprocessor.recify_tagging_result(nltk.pos_tag(tokens))

        # get the bigram of the sentence, which tells subjects/objects from other elements
        bigram = self.nlp_engine.bigram_chunk_sentence(tags)
        final_gram = self.nlp_engine.top_pattern_recognizer(bigram) # the fully processed tree that contains all the info needed.
        # final_gram.draw()
        return self.interpreter.main_tree_navigator(final_gram)


#
#
#
#
#
# chunker = NLQ_Chunker()
# sentence = input('Ask: ')
# start = time.time()
# chunker.chunk_a_sentence(sentence)
# print('took ' , time.time() - start, 'seconds')