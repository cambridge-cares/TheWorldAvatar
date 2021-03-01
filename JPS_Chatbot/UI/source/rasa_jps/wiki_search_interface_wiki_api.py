import json
import urllib.parse
import urllib.request
from pprint import pprint


class SearchInterface:

    def __init__(self):
        self.api = 'https://www.wikidata.org/w/api.php?action=wbsearchentities&search=Benzene&format=json&errorformat' \
                   '=plaintext&language=en&uselang=en&type=item'

    def fire_query(self, word):
        url = self.api
        values = {'search': word}
        data = urllib.parse.urlencode(values).encode('utf-8')
        req = urllib.request.Request(url, data)
        response = urllib.request.urlopen(req).read()
        return response

si = SearchInterface()
x = si.fire_query('chemicals')
pprint(json.loads(x))