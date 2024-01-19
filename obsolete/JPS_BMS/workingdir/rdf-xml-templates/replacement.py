import os
for filename in os.listdir("./"):
	if filename.endswith(".owl"): 
		# print(os.path.join(directory, filename))
		with open(filename, 'r') as file :
			filedata = file.read()
			# Replace the target string
			filedata = filedata.replace('http://www.theworldavatar.com/OntoEIP/Eco-industrialPark.owl#', 'http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#')

			# Write the file out again
			with open(filename, 'w') as file:
				file.write(filedata)
			
		