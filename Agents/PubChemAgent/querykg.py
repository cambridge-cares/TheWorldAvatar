from pubchem.kgoperations.getkgdata import *


osIRI='http://www.theworldavatar.com/kb/ontospecies/Species_2e8f9829-37ca-46ad-91bc-010f37835b4f'

data1, data2 = get_ontospecies_data(osIRI)

print(data1, data2)