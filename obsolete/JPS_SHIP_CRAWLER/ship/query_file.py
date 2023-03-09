import os
import sys
import re
import rdflib
import datetime

def query_a_file(iri,coordinates_map):
	# use regex to find the filename... Ship-563063740.owl
	# 
	filename = re.search('Ship-[0-9]*.owl', iri)
	filename = filename.group(0)
	print(filename)
	g = rdflib.Graph()
	g.parse('owlfiles/' + filename)
	
	qres = g.query(
		"""SELECT DISTINCT ?nvx ?nvy
		   WHERE {
			  ?ship <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x>  ?x .
			  ?ship <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y>  ?y .
			  ?x <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> ?vx .
			  ?y <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> ?vy .
			  ?vx <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> ?nvx .
			  ?vy <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> ?nvy .
			  }""")

	x = 0
	y = 0
	for row in qres:
		x = row[0]
		y = row[1]
	print(x,y) 
	



#iri_list = sys.argv[1] # the list of iri is designated to be the first parameter of the request
with open('test_iri_list.txt') as f:
	coordinates_map = {}
	a = datetime.datetime.now()
	counter = 0
	iri_list = f.readlines()
	for i in range(0,1):
		for iri in iri_list:
			query_a_file(iri,coordinates_map)
			counter = counter + 1
	b = datetime.datetime.now() # Benchmark: On Xiaochi's computer, querying 70 files takes less than 2.5 seconds. 
	print(counter)
	print(b-a)
