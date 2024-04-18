import rdflib
from rdflib.plugin import register, Parser
from rdflib import Graph, plugin
import json
import sys
import os


def convert_owl_to_jsonld(owl_file_path):
    # Load the OWL file into an RDFLib Graph
    g = Graph()
    g.parse(owl_file_path, format='xml')

    # Serialize the graph to JSON-LD
    jsonld_data = g.serialize(format='json-ld', indent=4)
    
    # Convert bytes to a JSON object
    json_object = json.loads(jsonld_data)
    
    return json_object


# Register the plugins for the parsers
register('json-ld', Parser, 'rdflib_jsonld.parser', 'JsonLDParser')

# check if the source and target filename was provided
if len(sys.argv) < 4:
    print("Usage:")
    print("Case-1: python owl2jsonld.py -f path/to/source.owl path/to/target.jsonld")
    print("Case-2: python owl2jsonld.py -all path/to/source_directory path/to/target_directory")
    sys.exit(1)

if  sys.argv[1] == '-f':  
    # The first command-line argument is the script name, so the second one is the source
    source_owl_file = sys.argv[2]

    # the third argument is the target
    target_jsonld_file = sys.argv[3]

    print('Conversion started. Required time is proportional to the file size. \nTherefore, please keep patience until you get a message.')
    # Convert OWL to JSON-LD
    jsonld_output = convert_owl_to_jsonld(source_owl_file)

    with open(target_jsonld_file, 'w') as file:
        json.dump(jsonld_output, file, indent=4)
elif sys.argv[1] == '-all':
    source_directory = sys.argv[2]
    target_directory = sys.argv[3]

    # Ensure the directories end with '/'
    if not source_directory.endswith('/'):
        source_directory += '/'
    if not target_directory.endswith('/'):
        target_directory += '/'

    print('Conversion started. Required time is proportional to the number of files and file sizes. \nTherefore, please keep patience until you get a message.')
    # Loop through each file in the source directory
    for file_name in os.listdir(source_directory):
        if file_name.endswith('.owl'):  # Check if it's an OWL file
            source_owl_file = os.path.join(source_directory, file_name)
            target_jsonld_file = os.path.join(target_directory, file_name.replace('.owl', '.jsonld'))

            # Convert OWL to JSON-LD
            jsonld_output = convert_owl_to_jsonld(source_owl_file)

            # Write JSON-LD output to the target file
            with open(target_jsonld_file, 'w') as file:
                json.dump(jsonld_output, file, indent=4)
else:
    print("Usage:")
    print("Case-1: python owl2jsonld.py -f path/to/source.owl path/to/target.jsonld")
    print("Case-2: python owl2jsonld.py -all path/to/source_directory path/to/target_directory")
    sys.exit(1)
print('Conversion completed.')