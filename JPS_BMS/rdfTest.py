from rdflib import Namespace, URIRef, Graph, Literal, XSD
from rdflib.namespace import RDF, FOAF
import shutil

import datetime
import pprint
import os

import sys

import json

#inputs	:	destination owlfile namespace
#			IRI to be modified  
#			value to be appended {'value': 10.01, 'type': float, 'unit': {'isSI': true, 'unit': CM}}
#			


#function :	momick the structure of exsiting instance
#			append timeStamp to add new instance storing datatype

with open('config/config.json', 'r') as f:
	config = json.load(f)

templateDir = config['TEMPLATE_DIR']
outputDir = config['TEST']['OUTPUT_DIR']

def replaceFile(filename):
	# shutil.copyfile('../DES/' + filename, './' + filename)
	shutil.copyfile(templateDir + filename, outputDir + filename)

def appendOwlFile(filename,targetIRI,value,isBinary):

	
	
	URL = ("http://www.theworldavatar.com/BMS/%s" %filename )
	
	print('-----URL-----', URL)
	print('-----IRI-----', targetIRI)
	
	
	g = Graph()
	try:
		g.parse(URL)		# read owl file from URL 
	except:
		replaceFile(filename)
	
	# 2018.11.19 Implementing error handling mechanism right here..
	
	
	
	timeStamp = datetime.datetime.now().strftime('%D-%H:%M:%S').replace('/','-') # generate the timeStamp in such format : 09-05-17-12:19:40 
	system = Namespace('http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#')
	derivedUnit = 'http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/SI_unit/derived_SI_units.owl#'
	SIUnit		= 'http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/SI_unit/SI_unit.owl#'
	# iterate through the owl file, find all target IRI related instances, duplicated them and append timeStamp
	for stmt in g:
		if('#' in stmt[0]):
			if(stmt[0].split('#')[1].strip() == targetIRI.strip()):
				g.add((URIRef(stmt[0] + '@' + timeStamp), stmt[1], stmt[2]))
				subject = stmt[0] + '@' + timeStamp
				print('subject --- ', subject)
		if('#' in stmt[2]):
			if(stmt[2].split('#')[1] == targetIRI):
				g.add((stmt[0], stmt[1], (URIRef(stmt[2] + '@' + timeStamp))))

		
	datatypes = {'int': XSD.int, 'float': XSD.float, 'string': XSD.string}
	if(value['value']):
		# try:
			g.add((URIRef(subject),
				   system.numericalValue,
				   Literal(value['value'],
				   datatype= datatypes[value['type']]))) # adding data to the newly
		# except:
			# print("ERROR")
	# g.serialize(destination='%s' %filename, format='pretty-xml')					   # serialize and save the file
	g.serialize(destination=outputDir + filename, format='pretty-xml')					   # serialize and save the file

#def appendOwlFile(filename,targetIRI,value):
	
# value = {'value': 10.01, 'type': 'float', 'unit': {'isSI': True, 'unit': 'CM'}}
# filename = 'WFH-07_SashOp_sensor1'
# targetIRI = 'V_SashOpeningOfWFH-07'
# appendOwlFile(filename,targetIRI,value)
