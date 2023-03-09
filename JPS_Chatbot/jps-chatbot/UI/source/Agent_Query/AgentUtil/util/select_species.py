import json
import random

s_list = json.loads(open('super_list').read())

selected_species = random.sample(s_list, 100)

with open('selected', 'w') as f:
    for ss in selected_species:
        try:
            f.write(ss)
            f.write('\n')
        except:
            pass
    f.close()