import re
import rdfTest

# This yet another great python script maps BMS 
# raw data (in xml format) into separated owl files

# Magical regex <VAV_+[a-zA-Z]+[0-9]_[a-zA-Z0-9]*_WFH-.*>	
# WFH...Door
# WFH...status


# this function uses regular expressions to separate values from tags
# and separate values and units
def separateContent(content, idRegex, filenameTemplate, IRITemplate, isSI, nameOfRow):
	valueRegex = '>.*<'
	tagRegex = '<[a-zA-Z0-9_ -]*>'
	# all the values are wrapped following such pattern : '<tagName>........</tagName>'
	# therefore, such regex identify the value wrapped within tags 
	numericalValueRegex = '-*[0-9]+.*[0-9]*.*' 
	# such regex will identify strings like '10.11','-0','100','10.11 cm','-0 %', which are numerical values
	
	value = re.search(valueRegex, content) # search out the value part
	print('content', content)
	if (value):
		tag = (re.search(tagRegex, content).group(0))
		if(abbrPair):						# whether the case needs to get the abbriation from the dictionary

			IRI = abbrPairs[currentRoomName]
			if (idRegex == 'roomItem'):
				id 	= IRI.replace('_','-')
			else:
				id	= (re.search(idRegex,tag)).group(0).replace('_','-')
			filename = (filenameTemplate %id)
			targetIRI = (IRITemplate %IRI)			
		else:
			if(type == '1'):
				if(re.match('.*AV_[a-zA-Z][0-9]_[0-9]*.*',tag)):
					id	= (re.search(idRegex,tag)).group(0).replace('_','-')
					filename = (filenameTemplate %id.replace('_','-'))
					targetIRI = (IRITemplate %(id[:5] + '/' + id[5:]).replace('_','-'))	

				else:
					id = heaterAirflowIdTag[:5] + '/' + heaterAirflowIdTag[5:]
					filename = (filenameTemplate %heaterAirflowIdTag.replace('_','-'))
					targetIRI = (IRITemplate %id.replace('_','-'))	
			
			elif(type == '2'):
				value = value.group(0).strip()[1:-1].strip()
				
				splittedArray = value.split(' ')			
				value = splittedArray[0]
				if(value == '1'):
					id = (re.search(idRegex,tag)).group(0)
					id = id.replace('0','').replace('_','-')[1:]
					filename = (filenameTemplate%id)
					targetIRI = (IRITemplate %id)
					value = {'value':tag.split(' ')[1][:-1] , 'type': 'string', 'unit': {'isSI': isSI, 'unit': ''}}
					rdfTest.appendOwlFile(filename,targetIRI,value,True)		
					return 0
					
				else:
					return 0
			
			elif(type == '3'):
				if('MAU' in EquipmentName):
					print('---------------------- type 3')
					id = re.search(idRegex,EquipmentName).group(0)
					filename = (filenameTemplate%id)
					if('%s' in IRITemplate):
						if('C7-1' in id and (not ('Fan' in filename) and (not ('VA' in filename)) and (not ('FIL' in filename)))):
							print('Here we are ',line)
							print('id ----',id)
							if('Pre_Cool_Coil_CHW_Out_Temp' in tag):
								filename = 'MAU-C7-1_P5T_sensor1.owl'
								targetIRI = 'V_PreCoolTemperatureCHWOutP5OfMAU-C7-1'
							else:
								return 0
						
						else:	

							targetIRI = (IRITemplate %id)
					else:
						targetIRI = IRITemplate
					#print('filename--- ',filename)
					#print('targetIRI---	',targetIRI)
					#rdfTest.appendOwlFile(filename,targetIRI,value,True)
				else:
					return 0
					
			elif(type == '4'):
				#print('nameOfRow',nameOfRow)
				if('MAU' in EquipmentName):
					#print('----------------------')
					print('EquipmentName',EquipmentName)
					id = re.search(idRegex,EquipmentName).group(0)[2:]
					print('id',id)
					filename = (filenameTemplate%id)
					if('%s' in IRITemplate):
						targetIRI = (IRITemplate %id)
					else:
						targetIRI = IRITemplate
					#print('filename--- ',filename)
					#print('targetIRI---	',targetIRI)
					#rdfTest.appendOwlFile(filename,targetIRI,value,True)
				else:
					return 0					
					
			elif(type == '5'):

				filename = (filenameTemplate %idRegex)
				targetIRI = (IRITemplate)
				print('filename',filename)
				print('targetIRI',targetIRI)
				
				
			else:
				id	= (re.search(idRegex,tag)).group(0)
				filename = (filenameTemplate %id)
				targetIRI = (IRITemplate %id)
				
				
				
		
		# print('tag--- ',tag)
		# print('id--- ',id)
		# print('filename--- ',filename)
		# print('targetIRI---	',targetIRI)
		

		value = value.group(0).strip()[1:-1].strip() # cut off > and < leaving the value content only
		if(re.match(numericalValueRegex,value)):      # whether the value is a numerical value
			splittedArray = value.split(' ')			
			if(len(splittedArray)> 1):
				unit = splittedArray[1]
				value = splittedArray[0]
			else:
				unit = ''
		else:
			unit = ''
	else:
		value = ''
		unit = ''
	
	print('targetIRI -------' , targetIRI)
	return {
		'content': {'value': value, 'type': 'float', 'unit': {'isSI': isSI, 'unit': unit}},
		'info'	 : {'filename': filename, 'targetIRI': targetIRI }
	
	}
	
	
# ===========Sample Mapping=============

# in xml : FH...Door re = 'WFH-[0-9]+_Door' (sufficient to identify)
# re = .*WFH-[0-9]+_Door.* to identify the whole line

# ===========================================================

# WFH-06_SashOp_sensor1 --- name of the owl file
# VAV_E7_14_WFH-06_Door --- name of the tag in xml raw data
# V_SashOpeningOfWFH-06 --- name of the IRI in the owl file 

abbrPairs = {}
heaterParams = ['HeaterControl','HeaterAirflow']


with open('workingdir/abbrPair.csv') as file:
	lines = file.readlines()
	for line in lines[1:]:
		cols = line.split(',')
		roomName = cols[0].strip()
		abbrName = cols[1].strip()
		abbrPairs[roomName] = abbrName
		
#print(abbrPairs)
#print('=======================================')

#input = input('stop')

with open('workingdir/map.csv') as file:
	lines = file.readlines()
	for line in lines[1:]: 
		cols = line.split(',')
		if(len(cols) > 3):
			nameOfRow 			= cols[0].strip()
			nameRegex 			= cols[1].strip()
			idRegex	  			= cols[2].strip()
			filenameTemplate 	= cols[3].strip()
			IRITemplate			= cols[4].strip()
			isSIUnit			= (int(cols[5].strip()) > 0) 
			abbrPair			= (int(cols[6].strip()) > 0)
			type 				= cols[8].strip()
			print('type',type)
			
			currentRoomName = ''
			heaterAirflowIdTag = ''
			EquipmentName = ''
			
			valueRegex = '>.*<'
	# Read local xml file
			with open('workingdir/bmsResult.html') as file:
				lines = file.readlines()
				for line in lines:
					if('<Area>' in line):
						
						currentRoomName = re.search(valueRegex,line).group(0)[1:-1].strip()
						
					if('<Equipment>' in line):
						EquipmentName = re.search(valueRegex,line).group(0)[1:-1].strip()
					
					heaterAirflowIdTagRegex = '<VAV_S[0-9]_[0-9]*.*'
				
					if(re.match(heaterAirflowIdTagRegex,line)):
						heaterAirflowIdTag = re.search('VAV_S[0-9]_[0-9]*',line).group(0).strip()
					
					
					if(re.match(nameRegex,line)):
						# inconsistent occurance # 001 
						line = line.replace('FH0','FH-0')
						#idRegex = 'W*FH-*[0-9]*'
						#idRegex2= '[a-zA-Z]*_[a-zA-Z][0-9]_[0-9]*'
						#filenameTemplate = ('%s.owl')
						#filenameTemplate2 = ('%s_SashOp_sensor1.owl')
						#IRITemplate = 'V_SashOpeningOf%s'
						value = separateContent(line,idRegex,filenameTemplate,IRITemplate,True,nameOfRow)
		

						if(value != 0):
							filename = value['info']['filename']
							targetIRI= value['info']['targetIRI']
							rdfTest.appendOwlFile(filename,targetIRI,value['content'],False)
							#owlfileMatch = ('WFH-%s_SashOp_sensor1' %id)
							#print('========================================')
							#print()
					
					# value = {'value': 10.01, 'type': 'float', 'unit': {'isSI': True, 'unit': 'CM'}}
					# filename = 'WFH-07_SashOp_sensor1'
					# targetIRI = 'V_SashOpeningOfWFH-07'
					# appendOwlFile(filename,targetIRI,value)



