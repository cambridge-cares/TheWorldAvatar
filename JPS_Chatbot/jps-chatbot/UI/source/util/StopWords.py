
def removeStopWords(_question):
    stopwords = ['the', 'an', 'a', 'is', 'what', 'are', 'of', 'describe', 'find', 'find me']
    _question = _question.strip()
    _question = _question.lower()
    words = _question.split(' ')
    words = [w for w in words if w not in stopwords]
    return ' '.join(words)