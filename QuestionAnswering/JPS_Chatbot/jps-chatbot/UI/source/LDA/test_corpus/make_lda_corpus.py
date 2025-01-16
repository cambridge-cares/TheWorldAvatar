import json, re


ontologies =  ['ontocompchem', 'ontokin', 'wiki']
def process_puncutation(string):
    # Load the regular expression library

    # Remove punctuation
    string_temp = re.sub('[-\n,.!?()\[\]0-9]', '', string)
    # Convert the titles to lowercase
    string_temp = string_temp.lower()
    # Print out the first rows of papers
    return string_temp

arrays = []
for o in ontologies:
    f_name = '%s_corpus' % o
    with open(f_name) as f:
        content = json.loads(f.read())
        content = [process_puncutation(x) for x in content]
        arrays.append(content)

with open('corpus', 'w') as f:
    f.write(json.dumps(arrays))
