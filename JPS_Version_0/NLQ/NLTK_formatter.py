import NLTK_CompareString


class ReportGenerator:
    def __init__(self, return_dict, json_response, term, original_term):
        self.return_dict = return_dict
        self.json_response = json_response
        self.term = term
        self.original_term = original_term

    def generate_html_report(self):
        result_string = ''
        term = self.term
        original_term = self.original_term
        json_response = self.json_response
        return_dict = self.return_dict

        result_string = result_string + '<p><b>' + term + '</b>\t<br/>'
        if json_response['results']:
            for result in json_response['results']:
                result_string += '<u><b>' + result['uri'] + '</u></b>\t\t|---|\t\t'
                result_string += result['label'] + '\t\t|\t\t'
                result_string += str(result['refCount']) + '\t\t|\t\t'
                result_string += '</p>'

                print('originalTerm', original_term)
                print('label', result['label'])
                if result['label']:
                    result_string = result_string + 'score: ' + str(
                        NLTK_CompareString.phraseSimilarity(result['label'], original_term)) + '<br/>'
                result_string = result_string + '<br/>'
        return_dict['result'] = return_dict['result'] + '<br/>' + result_string
