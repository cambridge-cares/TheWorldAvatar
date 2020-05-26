from search_engine.search_interface import SearchInterface

interface = SearchInterface()
word = 'chemical formula'
while word is not 'q':
    word = input('search the uri of ')
    result = interface.uri_look_up(None, None, word)
    print(result)