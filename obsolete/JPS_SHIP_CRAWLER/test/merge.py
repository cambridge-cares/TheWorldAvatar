import re


def test():
	old_ships = []
	full_old_ships = []
	with open('./ship-coordinates-new.csv') as old_csv:
		full_old_ships = old_csv.readlines()
		old_ships = full_old_ships[1:]

	new_ships = []	
	with open('./ship-coordinates-leasure.csv') as new_csv:
		new_ships = new_csv.readlines()
		
	mmsi_list = []
	with open('./mmsi') as mmsi_file:
		mmsi_list = mmsi_file.read().splitlines()

	print(mmsi_list)	
		
	# compare leasure with mmsi_file
	counter = 0
	for ship in new_ships:
		new_ship_mmsi = re.search('[-][0-9]*', ship).group(0).replace('-','')
		if new_ship_mmsi in mmsi_list:
			counter = counter + 1

	for old_ship in old_ships:
		old_ship_mmsi = re.search('[-][0-9]*', old_ship).group(0).replace('-','')
		#print(old_ship_mmsi)
		if str(old_ship_mmsi) not in mmsi_list:
			print('bad line for old ship')
			
	print('bad line ', counter)
	
def merge():

	old_ships = []
	full_old_ships = []
	
	mmsi_list = []
	with open('./mmsi') as mmsi_file:
		mmsi_list = mmsi_file.read().splitlines()
	
	with open('./ship-coordinates-new.csv') as old_csv:
		full_old_ships = old_csv.readlines()
		old_ships = full_old_ships[1:]
	new_ships = []	
	with open('./ship-coordinates-leasure.csv') as new_csv:
		new_ships = new_csv.readlines()
			
	for ship in new_ships:
		new_ship_mmsi = re.search('[-][0-9]*', ship).group(0).replace('-','')
		if new_ship_mmsi not in mmsi_list:
			full_old_ships.append(ship)
	content = ''.join(full_old_ships)
	
	with open('./ship-coordinates.csv','w') as f:
		f.write(content)
			
merge()
 