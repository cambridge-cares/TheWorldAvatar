class ReportGenerator:
    def __init__(self, return_dict):
        self.return_dict = return_dict	
	
	def generateHTMLreport(self):
		resultString = resultString + '<p><b>' + term + '</b>\t<br/>'
		if(jsonResponse['results']):
			for result in jsonResponse['results']:
				resultString = resultString + '<u><b>' +result['uri']  + '</u></b>\t\t|---|\t\t'
				resultString = resultString + 	      result['label']	  + '\t\t|\t\t'
				resultString = resultString +     str(result['refCount']) + '\t\t|\t\t'
				resultString = resultString + '</p>'
			
				print('originalTerm',originalTerm)
				print('label',result['label'])
				if(result['label']):
					cb = wordnet.synset(result['label'].replace(' ','_') + '.n.01')
					ib = wordnet.synset(originalTerm[1:] + '.n.01')
					score = cb.wup_similarity(ib)
					resultString = resultString + 'score: ' + score +'<br/>'
			resultString = resultString + '<br/>'
		return_dict['result'] = return_dict['result'] + '<br/>' +(resultString)