import json 

 

with open('owls_in_directory') as f:
	owls_in_directory = json.loads(f.read())
    
    
all_owl_files = ['http://www.theworldavatar.com/kb/ontocompchem/' + x for x in list(set(owls_in_directory))]

with open('all_owl', 'w') as f:
    for file in all_owl_files:
        f.write(file + '\n')
        
    f.close()


