import nltk
import NLTK_Chink
import NLTK_CompareString
import re
sentence1 = '''How many people lived in Poland'''
sentence2 = '''What are the zodiac signs'''
sentence3 = '''Which Indian company has the most employees'''

phrase1 = '''writer'''
phrase2 = '''author'''

score = NLTK_CompareString.phraseSimilarity(phrase1, phrase2)
print('Score', score)

sentence = sentence2

#NLTK_Chink.Test(-1, sentence)
phrase1 = ' '.join(re.sub('(?!^)([A-Z][a-z]+)', r' \1', phrase1).split())
tokens = nltk.word_tokenize(phrase1)
print(tokens)
#tagged = nltk.pos_tag(tokens)
#entities = nltk.chunk.ne_chunk(tagged)

#entities.draw()
