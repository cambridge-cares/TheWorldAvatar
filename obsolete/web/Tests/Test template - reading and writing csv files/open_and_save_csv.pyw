import os, csv

#dictionary function designed to read .csv file from a provided address and given an array to store the values			
def RCSV(address):
	csv_reader = csv.DictReader(open(address, 'r'), delimiter=',', quotechar='"')
	headers = csv_reader.fieldnames
	input=[]
	for line in csv_reader:
		for i in range(len(csv_reader.fieldnames)):
			input.append(line[csv_reader.fieldnames[i]])
	return input, headers

#dictionary function which writes a .csv file given its address, an array with values to be saved and an array with headers under which it is supposed to write the values (to be improved)
def WCSV(address, output, headers):
		writer = csv.writer(open(address, 'w'), delimiter=',', quotechar='|', quoting=csv.QUOTE_MINIMAL, lineterminator = '\n')
		writer.writerow(headers)
		writer.writerows(output)	
		
output, headers = RCSV(r"Input file.csv")
WCSV(r"Output file.csv", output, headers)