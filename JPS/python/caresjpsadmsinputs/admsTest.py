import sys
import json
import config
from collections import namedtuple
from admsAplWriter import admsAplWriter
from admsInputDataRetrieverNew import admsInputDataRetriever

bldString = '''
{"BldIRI": ["http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_35ED6FD1-5876-4F03-996D-8C885FD11057", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_ABE4C2D7-C6E0-4B95-9A3C-E4457B796F3C", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_C1C1997A-43EC-4F7C-B62B-5F115CAD5117", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_240BBC7B-CE2C-420D-89E5-CFD6A2C0C044", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_4611BF50-156F-458A-92EC-A11EEF98471F", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings0.owl#BuildingGUID_94405F3C-FB53-4EC8-93A1-5F95FEC74CBD", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings3.owl#BuildingGUID_E09C7CD0-2088-4F9F-AA4E-C0ACB582D77F", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings2.owl#BuildingGUID_81AB074F-EEB6-4006-87B6-6BB5E80E692B", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_7CA23259-87DB-4FF2-913B-DA4D82A5D3AE", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings2.owl#BuildingGUID_9D576B3B-9D3C-4E50-812D-FDCA5F55198A", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_73624930-7901-4F47-84FF-C1D0FC2E8991", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings4.owl#BuildingGUID_B42C1246-F744-4AC3-80EE-1768B1729924", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings4.owl#BuildingGUID_BD5DD2CF-FFE1-4F39-9704-70D673C4744B", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_0FC3F2FF-A572-4AAE-B7C2-EA3BE1FA1C12", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_E4705286-9AFA-433E-9105-4F953FA50FE1", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_952A0A82-9CEB-4F21-836D-3845D068012A", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_6FA9E00C-A79E-408C-9AD4-2880E3A60972", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings2.owl#BuildingGUID_43E9C3A8-46CF-46A1-9CF1-5DC457A9EF50", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_DDA5FD88-34C5-4C44-A485-CA001E91C7FD", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings2.owl#BuildingGUID_2DDCBBF1-041C-40EE-AE1C-96C90F63DAF2", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_00564872-9958-4788-997C-1E86F79AF802", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings3.owl#BuildingGUID_3C20D25F-1A9B-4C9B-B55F-7097636D6CAF", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings2.owl#BuildingGUID_5638ACFF-363C-409E-996B-2F423D46A9E7", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings3.owl#BuildingGUID_97612518-F080-48FE-B48C-D3600B96BB1E", "http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings4.owl#BuildingGUID_3FA056EB-3F9A-48B0-95A2-7C8CC2319B7E"], "BldName": ["3-996D-8C885FD11057", "5-9A3C-E4457B796F3C", "C-B62B-5F115CAD5117", "D-89E5-CFD6A2C0C044", "A-92EC-A11EEF98471F", "8-93A1-5F95FEC74CBD", "F-AA4E-C0ACB582D77F", "6-87B6-6BB5E80E692B", "2-913B-DA4D82A5D3AE", "0-812D-FDCA5F55198A", "7-84FF-C1D0FC2E8991", "3-80EE-1768B1729924", "9-9704-70D673C4744B", "E-B7C2-EA3BE1FA1C12", "E-9105-4F953FA50FE1", "1-836D-3845D068012A", "C-9AD4-2880E3A60972", "1-9CF1-5DC457A9EF50", "4-A485-CA001E91C7FD", "E-AE1C-96C90F63DAF2", "8-997C-1E86F79AF802", "B-B55F-7097636D6CAF", "E-996B-2F423D46A9E7", "E-B48C-D3600B96BB1E", "0-95A2-7C8CC2319B7E"], "BldType": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0], "BldX": [79973.71875, 79787.640625, 79925.125, 79954.203125, 79948.859375, 79974.6171875, 79952.265625, 79909.65625, 79949.6875, 79785.65625, 79966.4453125, 79921.453125, 79759.09375, 79711.0390625, 79891.390625, 79914.0703125, 79794.1484375, 79937.171875, 79735.75, 79931.8359375, 79914.75, 79965.609375, 79923.5546875, 79904.6640625, 79826.1484375], "BldY": [454782.125, 454766.46875, 454780.5625, 454785.0, 454803.5625, 454818.59375, 454688.28125, 454806.6875, 454828.59375, 454908.8125, 454775.03125, 454748.5625, 454874.8125, 454754.625, 454821.53125, 454775.5625, 454786.21875, 454811.875, 454692.1875, 454820.0625, 454796.25, 454823.625, 454830.8125, 454824.03125, 454925.78125], "BldHeight": [10.737, 2.9479999999999995, 9.981, 7.461, 10.178, 10.44, 15.303, 10.207999999999998, 10.344999999999999, 24.349, 10.067, 8.495000000000001, 16.275000000000002, 3.7880000000000003, 10.163, 9.948, 37.544, 10.181000000000001, 3.8180000000000005, 10.209, 10.181, 10.44, 10.332, 10.174, 22.083000000000002], "BldLength": [12.194571766421577, 3.5010548773057315, 12.18068970379577, 20.012886473470036, 27.02887001805893, 30.517742062059675, 45.50402680069232, 11.54724764706535, 22.899354404194593, 88.9055006696234, 6.876083011288113, 36.02745619798401, 50.932170565715694, 45.69805327094259, 12.42453784351756, 11.992398715566665, 21.994791406845472, 14.370313859138786, 10.097839849636468, 10.99169886085205, 11.687001263590085, 11.842227082992297, 21.55373604732266, 23.943691319508492, 77.84151104831635], "BldWidth": [43.933992586863255, 2.1863527662027455, 14.484646410741185, 23.616948568285327, 12.070734017488169, 8.0351608360033, 35.599823062466385, 16.543349307636653, 21.671364403851815, 29.960374440069312, 19.94850881146744, 10.763834586891047, 12.297131127957927, 12.127251223923013, 16.6691075064258, 15.847145562234376, 0.0, 13.256393494502742, 36.6426969005909, 5.097374782828319, 16.71367695325049, 24.58331644715868, 5.6796918563345296, 10.196489685096378, 17.13276583276476], "BldAngle": [62.69621896421969, 64.06391913991176, 62.66250519556777, 63.27490516231601, 63.123883789801624, 12.613126977268506, 63.355762445880245, 153.6083092001291, 11.911813136216717, 102.71120642156859, 63.84253387496758, 62.40695834723154, 102.65273150454249, 157.60015169277182, 63.8860873697093, 62.533516186061, 0.0, 153.62997382056363, 70.85011770550763, 63.307463192052545, 153.6062361437698, 102.65180382686528, 153.63927720769354, 153.7192098064336, 140.28130867746722]}'''

regionString = '''{"ymax": 455190, "xmax": 80000, "ymin": 454670, "xmin": 79480}'''
with open('./logs.txt','w') as f:
	f.write(str(sys.argv[1]))
buildingdata = json.loads(sys.argv[1].replace("'",'"'))
#buildingdata = json.loads(bldString)

BDN = namedtuple('BDN', ['BldNumBuildings','BldName','BldType','BldX','BldY','BldHeight', 'BldLength', 'BldWidth', 'BldAngle'])
BDN.BldName = buildingdata['BldName']
BDN.BldNumBuildings = len(BDN.BldName)
BDN.BldType = buildingdata['BldType']
BDN.BldX = buildingdata['BldX']
BDN.BldY = buildingdata['BldY']
BDN.BldHeight = buildingdata['BldHeight']
BDN.BldLength = buildingdata['BldLength']
BDN.BldWidth = buildingdata['BldWidth']
BDN.BldAngle = buildingdata['BldAngle']



coordinates = str(sys.argv[2]).replace("'",'"');
coordinates = json.loads(coordinates)

xmax = coordinates['uppercorner']['upperx']
ymax = coordinates['uppercorner']['uppery']

xmin = coordinates['lowercorner']['lowerx']
ymin = coordinates['lowercorner']['lowery']

coordinates['xmin'] = xmin
coordinates['ymin'] = ymin
coordinates['xmax'] = xmax
coordinates['ymax'] = ymax

#coordinates = json.loads(regionString)

plant = str(sys.argv[3])
workingDir = str(sys.argv[4]).replace('/','//')

#plant = "http://www.theworldavatar.com/Plant-001.owl#Plant-001";
#workingDir = 'C://Users//nasac//Documents//GIT//JPS//workingdir//ADMS'

test = admsInputDataRetriever(plant,config.bldTopnode, coordinates,  ["CO2"   ,"CO" ,  "NO2" ,  "HC" ,  "NOx"], 2, config.bdnLimit,False, BDN)
result = test.get()

result['Bdn'] = BDN
writer = admsAplWriter(result, workingDir + '//test.apl')
writer.write()