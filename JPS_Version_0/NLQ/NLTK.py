import nltk
import NLTK_Chink
sentence1 = '''How many people lived in Poland'''
sentence2 = '''What are the zodiac signs'''
sentence3 = '''Which Indian company has the most employees'''


sentence = sentence2
NLTK_Chink.Test(-1,sentence)



tokens = nltk.word_tokenize(sentence)
tagged = nltk.pos_tag(tokens)
entities = nltk.chunk.ne_chunk(tagged)
 
entities.draw()

