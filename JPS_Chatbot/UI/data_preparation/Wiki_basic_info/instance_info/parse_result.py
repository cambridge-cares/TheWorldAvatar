from os import listdir
from os.path import isfile, join
import json

#################
#
# parse each file separately
# retrieve label and alt labels and formula 
# 
#################
#
# label 
# altLabel_list
# formula 
#
#################
def parse_file(filename):
    

    
    main_label = '' 
    formula = ''
    alts = []
    with open (filename) as f:
        data = json.loads(f.read())
        data = data['results']['bindings']
        for item in data: # extract info from the bindings
            if 'label' in item:
                label = item['label']['value']
                main_label = label 
            if 'class' in item:
                _class = item['class']['value']
                if _class not in classes:
                    if 'classLabel' in item:
                        class_label = item['classLabel']['value']
                        classes.append(_class)
                        distinct_classes.append({'class': _class, 'label': class_label})
                
            if 'formula' in item:
                formula = item['formula']['value']
                subscript = str.maketrans("₀₁₂₃₄₅₆₇₈₉","0123456789")
                formula = formula.translate(subscript)
            if 'altLabel_list' in item:
                altLabel_list = item['altLabel_list']['value'].split('$')
                alts = altLabel_list
     
    #print('the main label is ', main_label)
    #print('the formula is ', formula)
    #print('the alternative labels are', alts)
    #print('=================================')
    
classes = []
distinct_classes = []
mypath = '.'



onlyfiles = [f for f in listdir(mypath) if isfile(join(mypath, f)) and f.startswith('Q')] 
print('There are in total', len(onlyfiles), 'instances')
x = input('Press any key to continue')

# iterate through all the files, their names begin with Q




for f in onlyfiles:
   parse_file(f)
   
print('the classes are ', classes)
