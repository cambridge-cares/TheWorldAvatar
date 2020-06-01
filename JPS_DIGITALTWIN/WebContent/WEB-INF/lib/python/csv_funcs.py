import os, csv

def RCSV(address):
    #dictionary function designed to read .csv file from a provided address and given an array to store the values
    csv_reader = csv.DictReader(open(address, 'r'), delimiter=',', quotechar='"')
    headers = csv_reader.fieldnames
    input=[]
    for line in csv_reader:
        for i in range(len(csv_reader.fieldnames)):
            input.append(line[csv_reader.fieldnames[i]])
    return input, headers
    
	
	

def WCSV(address, output, headers):
	#dictionary function which writes a .csv file given its address, an array with values to be saved and an array with headers under which it is supposed to write the values (to be improved
	with open(address, 'w') as csvfile:
		writer = csv.DictWriter(csvfile, fieldnames=headers, lineterminator = '\n')
		writer.writeheader()
		for i in range(int(len(output)/len(headers))):
			writer.writerow({headers[x]: output[int(len(headers)*i):int(len(headers)*(i+1))][x] for x in range(len(headers))})
		
		
def ACSV(address, output, headers):
    #dictionary function which appends a .csv file given its address, an array with values to be saved and an array with headers under which it is supposed to write the values (to be improved)
    does_exist = os.path.isfile(address)
    with open(address, 'a') as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=headers, lineterminator = '\n')
            if (does_exist==False): writer.writeheader()
            writer.writerow({headers[x]: output[x] for x in range(len(headers))})
			
			
			
def find_index(headers,k,recursions=0):
    #function for finding indices of streams and operations within a HYSYS simulation (used when calling specific items in a HYSYS simulation)
    # all members of 'headers' need to be contained within streams only or operations only
    #k=0 will search through streams, while k=1 will look through operations
    if recursions > 1: raise NameError('The name does not appear in the model or not all names belong to the same type.')
    type=['Streams','Operations']
    index=[]
    j=0
    i=0
    try:
        while j!=len(headers):
            if(eval('hysys.Flowsheet.'+type[k]+'.Names[i]')==headers[j]):
                    index.append(i)
                    i=0
                    j=j+1
                    continue
            else:
                i=i+1
                continue
    except IndexError:
        if k == 0: k = 1
        else: k = 0
        recursions += 1
        index = find_index(headers,k,recursions)
        print(type[k])
    
    return index

    