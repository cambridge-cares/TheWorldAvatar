import shutil
from multiprocessing import Process
import time
import rdflib





def prepareDataToUpdate():

	taskList = []
	with open('outputBusOPF.txt') as fileNow:
		linesNow = fileNow.readlines()	
		for line in linesNow:
			splittedLine = line.split('\t')
			index = str('{0:03}'.format(int(splittedLine[0])))
			busOutputMap = ['index',('V_PuVout_EBus-%s'%index),('V_thetaout_EBus-%s'%index),('V_Pout_EBus-%s'%index),('V_Qout_EBus-%s'%index)]
			filename = '../' + ('EBus-%s'%index) + '.owl'
			print('filename',filename)
			attrPairForALine = []
			for i in range(1,len(busOutputMap)):
				IRI = filename + '#' + busOutputMap[i]
				value = splittedLine[i] + ' (updated)'
				attrPair = {'IRI': IRI, 'value': value}
				attrPairForALine.append(attrPair)
			taskList.append({'fileName': filename,'aLine': attrPairForALine})
			
	# with open('outputBranchOPF.txt') as fileNow:
		# linesNow = fileNow.readlines()	
		# for line in linesNow:
			# splittedLine = line.split('\t')
			# index = str('{0:03}'.format(int(splittedLine[0])))
			# print('index -- ',index );
			#stop = input('yo')
			# branchOutputMap = ['index',('V_Ploss_MGDL-%s'%index),('V_Qloss_MGDL-%s'%index)]
			# filename = ('MGDL-%s'%index) + '.owl'
			# print('filename',filename)
			# attrPairForALine = []
			# for i in range(1,len(branchOutputMap)):
				# IRI = filename + '#' + branchOutputMap[i]
				# value = splittedLine[i] + ' (updated)'
				# attrPair = {'IRI': IRI, 'value': value}
				# attrPairForALine.append(attrPair)
			# taskList.append({'fileName': filename,'aLine': attrPairForALine})
			
	
	return taskList
	

def updateToFile(filename,attrPairForALine):
	print('filename',filename)
	print('attrPairForALine',attrPairForALine)
	updates = []	
		
	for attrPair in attrPairForALine:
		
		targetIRI = attrPair['IRI']
		value 	  = attrPair['value']
		
		deleteString = """PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#> DELETE WHERE  { <http://www.theworldavatar.com/""" + targetIRI + """> system:numericalValue ?o .}"""
		insertString = """PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#> INSERT DATA	 { <http://www.theworldavatar.com/""" + targetIRI + """> system:numericalValue '""" + value + """' .}"""

		updates.append(deleteString)
		updates.append(insertString)

	

	localfilename =  filename
	g = rdflib.Graph()
	g.load(localfilename)
	for update in updates:
		g.update(update)
		
	g.serialize(destination='%s' %localfilename,format='pretty-xml')
	print(filename)




if __name__ == "__main__":  # confirms that the code is under main function
	
	
	taskArray = prepareDataToUpdate()
	
	
	procs = []
	proc = Process(target=updateToFile)  # instantiating without any argument
	procs.append(proc)
	proc.start()

	for task in taskArray:
		proc = Process(target=updateToFile, args=(task['fileName'],task['aLine'],))
		procs.append(proc)
		proc.start()

	for proc in procs:
		proc.join()


