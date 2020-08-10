import json, re


def process_puncutation(string):
    # Load the regular expression library

    # Remove punctuation
    string_temp = re.sub('[-\n,.!?()\[\]0-9]', '', string)
    # Convert the titles to lowercase
    string_temp = string_temp.lower()
    # Print out the first rows of papers
    return string_temp

arrays = []
for i in range(4):
    f_name = 'topic_%s' % str(i + 1)
    with open(f_name) as f:
        content = f.read()
        content = process_puncutation(content)

        array = content.split(' ')
        print(array)
        print('--------------')
        arrays.append(array)

with open('corpus', 'w') as f:
    f.write(json.dumps(arrays))
