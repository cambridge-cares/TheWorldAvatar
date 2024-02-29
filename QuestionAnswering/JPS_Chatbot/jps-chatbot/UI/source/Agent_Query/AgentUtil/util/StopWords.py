
def removeStopWords(_question):
    stopwords = ['an', 'a', 'is', 'what', 'are', 'describe', 'find', 'find me', 'show', 'show me', 'the', 'all', 'me']
    _question = _question.strip()
    _question = _question.lower()
    _question = _question.replace("'s", ' singlequotesign')
    words = _question.split(' ')
    words = [w for w in words if w not in stopwords]
    return ' '.join(words)