import json
import NLTK_Chink
import time

startTime = time.time()
with open('./Training.json') as file:
	obj = json.loads(file.read())
	questions = obj['questions']

	
	for i,question in enumerate(questions):
		sentenece = question['question'][0]['string'].replace('?','')
		print('Question',i,sentenece)
		NLTK_Chink.Test(i,sentenece)	

endTime = time.time()
print('totally used',format((endTime - startTime),'.2f'))
