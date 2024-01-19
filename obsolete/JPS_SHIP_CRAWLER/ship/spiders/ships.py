import scrapy
import math
import json
import time
import os

from scrapy import signals
from scrapy.xlib.pydispatch import dispatcher

class get_all_ships(scrapy.Spider):
	name = "ships"
	def start_requests(self):
		self.content = 'instanceiri,latlong,classid\n'
		#os.mkdir('./owlfiles')
		with open('./ship-coordinates-new.csv', 'w') as _csv: 
			_csv.write(self.content)
			
		with open('./mmsi', 'w') as mmsi: 
			mmsi.write('')
		self.MMSI = []
		self.data_collection = []
		#with open('./AIS_DATABASE_SINGAPORE'+ str(time.time()) +'.txt','w') as f0:
		with open('./station_list.txt') as f:
			index_list = f.readlines()
		urls = ['http://www.aishub.net/stations/' + i.strip() for i in index_list]
		for url in urls:
			yield scrapy.Request(url=url, callback=self.get_page_number, meta={'station_index': url.replace('http://www.aishub.net/stations/','')})
		

		
	# def spider_closed(self, spider):
		# with open('./MMSI', 'w') as mmsi:
			# mmsi.write('\n'.join(self.MMSI))
		# with open('./ship-coordinates.csv', 'w') as _csv: 
			# _csv.write(self.content)
		
		
	def get_page_number(self, response):
		# first of all, get the number of pages first... 
		try:
			page_number = math.ceil(int(response.css('div.summary').css('b::text').extract()[1].replace(',','')) / 8)
		except:
			page_number = 150
		print('----------------- page number --------------------')
		print(page_number)
		station_index =  response.meta.get('station_index')
		MMSI_list_this_page = []
		for i in range(1, page_number):		
			url = 'http://www.aishub.net/stations/'+ station_index +'?page=' + str(i)
			yield scrapy.Request(url=url, callback=self.parse_page)
		


	def parse_page(self, response):	
		MMSI_list = response.css('td[data-col-seq="0"]::text').extract()
		for MMSI in MMSI_list:
			url = 'https://www.vesselfinder.com/api/pub/click/' + str(MMSI)
			yield scrapy.Request(url=url, callback=self.format_ship_in_json, meta={'MMSI': str(MMSI)})
	
	def format_ship_in_json(self, response):
		'''with open('./AIS_DATABASE_SG', 'a') as f:		
			with open('./log', 'a') as l:
				l.write(time.ctime(time.time()))
				l.write(': ' + str(len(str(response.body_as_unicode()))))
				l.write('\n')
			f.write(response.body_as_unicode())
			f.write('\n')'''
		obj = json.loads(str(response.body_as_unicode()))
		name = obj['name'].strip().replace(' ','-')	
		imo = obj['imo']
		MMSI = response.meta.get('MMSI')
		path = name + '-IMO-' + str(imo) + '-MMSI-' + str(MMSI)
		full_path = 'https://www.vesselfinder.com/vessels/' + path
		
		
		yield scrapy.Request(url=full_path, callback=self.analyze_data, meta={'MMSI': MMSI})


	def analyze_data(self, response):
		
		# Check whether the ship exist the in directory ... 
		
		
		
		data = {};
		MMSI = response.meta.get('MMSI')
		rows = response.css('table.tparams').css('tbody').css('tr')
		for row in rows:
			key = row.css('td.n3::text').extract_first()
			value = row.css('td.v3::text').extract_first()
			if (key is not None) and (value is not None):
				data[key] = value;
		
		type = data['AIS Type']
		eta = data['ETA']
		imo = data['IMO / MMSI'].split(' / ')[0]
		mmsi = data['IMO / MMSI'].split(' / ')[1]
		callsign = data['Callsign']
		length = data['Length / Beam'].split(' / ')[0]
		draught = data['Current draught'].replace(' m','')
		y = data['Coordinates'].split(' N/')[0]
		x = data['Coordinates'].split(' N/')[1].replace(' E','').replace(' W','')
		self.data_collection.append(data)
		iri = 'http://www.theworldavatar.com/kb/ships/Ship-' + MMSI + '.owl#Ship-' + MMSI + ','
		coordi = '"(' + data['Coordinates'].replace(' N/', ', ').replace(' E','').replace(' W','') + ')",1'		
		if mmsi not in self.MMSI:
			self.MMSI.append(mmsi)		
			with open('./mmsi', 'a') as mmsi_file: 
				mmsi_file.write(mmsi + '\n')
			with open('./ship-coordinates-new.csv', 'a') as _csv:
				_csv.write(iri)
				_csv.write(coordi)
				_csv.write('\n')
				
			#self.content = self.content + iri + coordi + '\n'
			

		file_content = '''<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF
   xmlns:j.1="http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#"
   xmlns:j.3="http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#"
   xmlns:j.4="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#"
   xmlns:owl="http://www.w3.org/2002/07/owl#"
   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl##MMSI#">
    <j.4:hasMMSI rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#MMSIOf#MMSI#"/>
    <j.4:hasPositioningDeviceType rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#PositioningDeviceTypeOf#MMSI#"/>
    <j.4:hasCallSign>#CALLSIGN#</j.4:hasCallSign>
    <j.4:hasShipName>#NAME#</j.4:hasShipName>
    <j.4:hasStarboardLength rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#StarboardLengthOf#MMSI#"/>
    <j.4:hasSternLength rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#SternLengthOf#MMSI#"/>
    <j.3:hasGISCoordinateSystem rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#CoordinateSystemOf#MMSI#"/>
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#Ship"/>
    <j.4:hasCOG rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#COGOf#MMSI#"/>
    <j.4:hasETA rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#ETAOf#MMSI#"/>
    <j.4:hasDraught rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#DraughtLengthOf#MMSI#"/>
    <j.4:hasIMONumber rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#IMONumberOf#MMSI#"/>
    <j.4:hasBowLength rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#BowLengthOf#MMSI#"/>
    <j.3:hasTimestamp rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#TemporalCoordinateSystemOf#MMSI#"/>
    <j.4:hasPortLength rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#PortLengthOf#MMSI#"/>
    <j.1:hasSubsystem rdf:resource="http://www.theworldavatar.com/kb/ships/Engine-001.owl#Engine-001"/>
    <j.4:hasSOG rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#SOGOf#MMSI#"/>
    <j.4:hasPAC>0</j.4:hasPAC>
    <j.4:hasDestination rdf:resource="http://dbpedia.org/resource/Singapore"/>
    <j.4:hasShipType rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#ShipTypeOf#MMSI#"/>
    <j.4:hasNavigationalStatus rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#NavigationalStatusOf#MMSI#"/>
    <j.4:hasRateOfTurn rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#RateOfTurnOf#MMSI#"/>
    <j.1:hasSubsystem rdf:resource="http://www.theworldavatar.com/kb/ships/Chimney-1.owl#Chimney-1"/>
    <j.4:hasHeading rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#HeadingOf#MMSI#"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#RateOfTurnOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#RateOfTurn"/>
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_RateOfTurnOf#MMSI#"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#PortLengthOf#MMSI#">
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_PortLengthOf#MMSI#"/>
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#DimensionOfPort"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_COGhOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue"/>
    <j.1:numericalValue>0</j.1:numericalValue>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#PositioningDeviceTypeOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#PositioningDeviceType"/>
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_PositioningDeviceTypeOf#MMSI#"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#NavigationalStatusOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#NavigationalStatus"/>
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_NavigationalStatusOf#MMSI#"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#MMSIOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#MMSI"/>
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_MMSIOf#MMSI#"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#BowLengthOf#MMSI#">
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_BowLengthOf#MMSI#"/>
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#DimensionOfBow"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_SOGOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue"/>
    <j.1:numericalValue>0</j.1:numericalValue>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#yCoordinateOf#MMSI#">
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_yCoordinateOf#MMSI#"/>
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#AngularCoordinate"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_BowLengthOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue"/>
    <j.1:hasUnitOfMeasure rdf:resource="http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m"/>
    <j.1:numericalValue>0</j.1:numericalValue>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_SternLengthOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue"/>
    <j.1:numericalValue>0</j.1:numericalValue>
    <j.1:hasUnitOfMeasure rdf:resource="http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_yCoordinateOf#MMSI#">
    <j.1:numericalValue rdf:datatype="http://www.w3.org/2001/XMLSchema#decimal">#Y#</j.1:numericalValue>
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue"/>
    <j.1:hasUnitOfMeasure rdf:resource="http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_DraughtLengthOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue"/>
    <j.1:hasUnitOfMeasure rdf:resource="http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m"/>
    <j.1:numericalValue>#DRAUGHT#</j.1:numericalValue>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#ETAOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#EstimatedTimeOfArrival"/>
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_ETAOf#MMSI#"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#StarboardLengthOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#DimensionOfStarboard"/>
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_StarboardLengthOf#MMSI#"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#CoordinateSystemOf#MMSI#">
    <j.3:hasProjectedCoordinate_y rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#yCoordinateOf#MMSI#"/>
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#ProjectedCoordinateSystem"/>
    <j.3:hasProjectedCoordinate_x rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#xCoordinateOf#MMSI#"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_ETAOf#MMSI#">
    <j.1:numericalValue>#ETA#</j.1:numericalValue>
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_MMSIOf#MMSI#">
    <j.1:numericalValue>0</j.1:numericalValue>
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_RateOfTurnOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue"/>
    <j.1:numericalValue>0</j.1:numericalValue>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#COGOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#CourseOverGround"/>
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_COGhOf#MMSI#"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#HeadingOf#MMSI#">
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_HeadingOf#MMSI#"/>
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#Heading"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_IMONumberOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue"/>
    <j.1:numericalValue>#IMO#</j.1:numericalValue>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/Chimney-1.owl#Chimney-1">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Pipe"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#IMONumberOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#IMOIdentificationNumber"/>
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_IMONumberOf#MMSI#"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#xCoordinateOf#MMSI#">
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_xCoordinateOf#MMSI#"/>
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#AngularCoordinate"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.semanticweb.org/kevin/ontologies/2018/11/untitled-ontology-2073">
    <owl:imports rdf:resource="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl"/>
    <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Ontology"/>
    <owl:imports rdf:resource="http://www.theworldavatar.com/ontology/ontocape/OntoCAPE.owl"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_StarboardLengthOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue"/>
    <j.1:numericalValue>0</j.1:numericalValue>
    <j.1:hasUnitOfMeasure rdf:resource="http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_PositioningDeviceTypeOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue"/>
    <j.1:numericalValue>0</j.1:numericalValue>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#timestampOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#TemporalCoordinate"/>
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_timestampOf#MMSI#"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#SternLengthOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#DimensionOfStern"/>
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_SternLengthOf#MMSI#"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_HeadingOf#MMSI#">
    <j.1:hasUnitOfMeasure rdf:resource="http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree"/>
    <j.1:numericalValue>0</j.1:numericalValue>
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#TemporalCoordinateSystemOf#MMSI#">
    <j.3:hasTemporalCoordinate rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#timestampOf#MMSI#"/>
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#TemporalCoordinateSystem"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_xCoordinateOf#MMSI#">
    <j.1:hasUnitOfMeasure rdf:resource="http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree"/>
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue"/>
    <j.1:numericalValue rdf:datatype="http://www.w3.org/2001/XMLSchema#decimal">#X#</j.1:numericalValue>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_ShipTypeOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue"/>
    <j.1:numericalValue>#TYPE#</j.1:numericalValue>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_timestampOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue"/>
    <j.1:numericalValue>0</j.1:numericalValue>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#ShipTypeOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#ShipType"/>
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_ShipTypeOf#MMSI#"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_PortLengthOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue"/>
    <j.1:hasUnitOfMeasure rdf:resource="http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m"/>
    <j.1:numericalValue>#LENGTH#</j.1:numericalValue>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#DraughtLengthOf#MMSI#">
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_DraughtLengthOf#MMSI#"/>
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#Draught"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#SOGOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#SpeedOverGround"/>
    <j.1:hasValue rdf:resource="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_SOGOf#MMSI#"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://www.theworldavatar.com/kb/ships/#MMSI#.owl#V_NavigationalStatusOf#MMSI#">
    <rdf:type rdf:resource="http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue"/>
    <j.1:numericalValue>0</j.1:numericalValue>
  </rdf:Description>
</rdf:RDF>'''	
			
			
		file_content = file_content.replace("#MMSI#", 'Ship-' + str(MMSI)).replace('#TYPE#', type).replace('#ETA#', eta).replace('#CALLSIGN#', callsign).replace('#LENGTH#', length).replace('#DRAUGHT#',draught).replace('#NAME#',MMSI).replace("#X#",x).replace('#Y#',y).replace('#IMO#',imo)
		with open('C:\TOMCAT\webapps\ROOT\kb\ships\Ship-' + MMSI + '.owl','w') as owlfile:
			owlfile.write(file_content)
			
