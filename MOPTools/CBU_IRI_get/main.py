from IRI_wrapper.iri_operations import cbuIRIOperations

doc = """This utility program queries the ontospecies knowledge grahp. 
The input file is a csv document containing entries of species and Inchis.
After the query the program writes back the IRI number of the queried inchi in an updated document.
That is, the updated document has the entries of the input file and in addition it has the IRI entries.  
"""
def start():
    cbuCSVFilePath = input("Path to the csv file:")
    cbuIRIOperations(cbuCSVFilePath)

if __name__ == '__main__':
    start()