import nltk
import urllib.request
from urllib.parse import quote
import json
import multiprocessing
from nltk.corpus import wordnet


def joinChunk(words):
	wordList = []
	wordList2 = []
	resultList = []
	originString = ''
	for word in words:
		wordList.append(word[0])
		originString = originString + '_' + word[0]
		resultList.append('_'.join(wordList))

	for word in words[::-1][:-1]:
		wordList2.append(word[0])
		resultList.append('_'.join(wordList2[::-1]))
	return {'list':resultList,'origin': originString}
		
def Lookup(termObj):

	termList = termObj['list']
	originalTerm = termObj['origin']
	resultArray = []
	jobs = []
	manager = multiprocessing.Manager()
	return_dict = manager.dict()
	return_dict['result'] = ''
	for term in termList:
		p = multiprocessing.Process(target=singleRequest,args=(term,originalTerm,return_dict))
		jobs.append(p)
		p.start()
	for j in jobs:
		j.join()
	
	return return_dict.values()[0]

def singleRequest(term,originalTerm,return_dict):
	resultString = ''
	url = 'http://lookup.dbpedia.org/api/search/PrefixSearch?QueryClass=&MaxHits=10&QueryString=' + quote(term)
	headers = {}
	headers['Accept'] = 'application/json'		
	request = urllib.request.Request(url, headers = headers)
	response = urllib.request.urlopen(request)
	print('1 request send')
	jsonResponse = json.loads(response.read().decode('utf-8','replace'))
	
	reportGenerator = ReportGenerator(return_dict)
	



s1 		= '''who served in Apollo 11 mission'''
s2 		= '''who was the doctoral supervisor of Albert Einstein'''
s3		= '''how many pages are there in war and peace'''
s4		= '''when was PRC established'''
s5 		= '''what is in a chocolate chip cookie'''
s6		= '''Which electronics companies were founded in Beijing'''
s7		= '''How many movies did Park Chan-wook direct'''


def Test(index,sentence):

	tokens = nltk.word_tokenize(sentence)
	tagged = nltk.pos_tag(tokens)
	grammar = '''
		Sub_Obj:
			{<.*>+}
			}<VBD|IN|WP|VBP|RB|WRB|VBZ|VBN|WP|WDT|VB|RBS|JJS>+{
		Predicate:
			{<IN|VBN|VBD|VB>}	
		Question:
			{<WP|WRB>}
		JJS:	
			{<JJS>}
		RBS:
			{<RBS>}
		JJNN:
			{<JJ><NN|NNP|NNS*>}
		'''

	cp = nltk.RegexpParser(grammar)
	result = cp.parse(tagged)
	ne = nltk.ne_chunk(tagged)
	
	
		

	with open('test.html','a') as file:
		file.write('<h2>SENTENCE: ' + str(index) + ':' + sentence + '</h2><br/>')

		for tree in ne:
				if(type(tree) == nltk.Tree):
					file.write('<p>' + str(tree.label()) + str(tree.leaves()) + '/<p><hr/>')


		for tree in result:
			if(type(tree) == nltk.Tree):
				file.write('<p>' + str(tree.label()) + str(tree.leaves()) + '/<p><hr/>')

				if(tree.label() == 'Sub_Obj'):		
					file.write(Lookup(joinChunk(tree.leaves())))


