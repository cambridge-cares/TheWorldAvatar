# the instances of chemical materials come from two classes
# class of chemical substances  and structural groups and chemical compounds
# this script merge the 3 sources and create a list of class for further query and get the alt_labels of the classes 
import json

names = ['001_clean', '002_clean', '003_clean']

all_classes_list = []
number_of_all_classes = 0
number_of_all_distinct_classes = 0


for name in names:
    with open('results/' + name) as f:
        list = json.loads(f.read())
        number_of_all_classes = number_of_all_classes + len(list)
        for item in list:
            if item not in all_classes_list and (len(item['itemLabel']) < 20): # remove repeated items 
                all_classes_list.append(item)
                number_of_all_distinct_classes = number_of_all_distinct_classes + 1
        f.close()
                
print('number_of_all_classes', number_of_all_classes)
print('number_of_all_distinct_classes', number_of_all_distinct_classes)


with open('all_distinct_classes', 'w') as f:
    f.write(json.dumps(all_classes_list))
    f.close()

        