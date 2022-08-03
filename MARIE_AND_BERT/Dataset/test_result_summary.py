import os 
import csv


def read_csv(file_name):
	max_num = 0
	max_file_name = ''
	with open(file_name) as csv_file:
		csv_reader = csv.DictReader(csv_file, delimiter=',')
		for row in csv_reader:
			print(row['Hit-1 Ratio'])


root_dir = './'

dirs = [x[0] for x in os.walk(root_dir) if '_' in x[0] and 'results' in x[0]]


for _dir in dirs:
	files = [f for f in os.listdir(os.path.join(root_dir, _dir)) if f.endswith('csv') and 'Testing' in f]
	print(_dir)
	# print(files)
	
	# read csv files, get hit1, hit 1 filtered, hit 3,5,10 and the filtered version
	for file in files:
		print(file)
		read_csv(os.path.join(root_dir, _dir, file))
		
	print('================')
	