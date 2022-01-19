import json
import collections
from pprint import pprint

path = '../../SPARQL_warehouse'
class_table = 'all_distinct_classes'
property_table = 'distinct_properties'
instances_table = 'selected_instance'


# you need two things
# 1. three arrays of all the labels, in order to have a rank of the uris
# 2. three dictionaries to find the uri of the labels

def load_corpus():
    class_path = path + '/' + class_table
    class_object = load_file(class_path)

    property_path = path + '/' + property_table
    property_object = load_file(property_path)

    instances_path = path + '/' + instances_table
    instances_object = load_file(instances_path)

    return {'class': class_object, 'property': property_object, 'instance': instances_object}


def load_file(file_path):
    with open(file_path) as f:
        content = json.loads(f.read())
    return content


all_corpus = load_corpus()


def add_to_dictionary(label, uri, dictionary):
    if type(label) is str:
        if label in dictionary:
            if uri not in dictionary[label]:
                dictionary[label].append(uri)
        else:
            dictionary[label] = [uri]
        return dictionary
    elif type(label) is list:
        for l in label:
            if l in dictionary:
                if uri not in dictionary[l]:
                    dictionary[l].append(uri)
            else:
                dictionary[l] = [uri]
        return dictionary


def process_instances():
    all_labels_instances = []
    instance_dictionary = {}
    transl = str.maketrans(dict(zip('₀₁₂₃₄₅₆₇₈₉', '0123456789')))
    # replace the subscripts of the formula
    instances = all_corpus['instance']
    for instance in instances:
        # print(instance)
        uri = instance["item"]["value"]
        # ---------------- label -----------------
        label = instance["itemLabel"]["value"].lower()
        instance_dictionary = add_to_dictionary(label, uri, instance_dictionary)
        # ---------------- alt label -------------
        alt_label_list = [x.strip().lower() for x in instance["altLabel_list"]["value"].split('$ ')]
        instance_dictionary = add_to_dictionary(alt_label_list, uri, instance_dictionary)
        # ---------------- formula ----------------
        formula = instance["x"]["value"].translate(transl).lower()
        instance_dictionary = add_to_dictionary(formula, uri, instance_dictionary)
        # =============== make label collection ==========
        all_labels_instances.append(formula)
        all_labels_instances = all_labels_instances + alt_label_list
        all_labels_instances.append(label)
    # check whether there is any repetition.
    # print([item for item, count in collections.Counter(all_labels_instances).items() if count > 1])
    # pprint(instance_dictionary)
    print('------- instances --------')
    print(instance_dictionary['c6h12o6'])
    print(instance_dictionary['glucose'])
    return {'dict': instance_dictionary, 'list':list(set(all_labels_instances))}


#     return {'class': class_object, 'property': property_object, 'instance': instances_object}
def process_classes():
    all_labels_classes = []
    class_dictionary = {}
    # replace the subscripts of the formula
    classes = all_corpus['class']
    for _class in classes:
        # print(instance)
        uri = _class["item"]
        # ---------------- label -----------------
        label = _class["itemLabel"].lower()
        class_dictionary = add_to_dictionary(label, uri, class_dictionary)
        all_labels_classes.append(label)
    # check whether there is any repetition.
    # print([item for item, count in collections.Counter(all_labels_instances).items() if count > 1])
    # pprint(class_dictionary)
    print('-------- classes --------------')
    print(class_dictionary['acid'])
    print(class_dictionary['fatty acid'])

    return {'dict': class_dictionary, 'list': list(set(all_labels_classes))}


def process_properties():
    all_labels_properties = []
    property_dictionary = {}
    properties = all_corpus['property']
    for p in properties:
        # print(instance)
        uri = p["item"]["value"]
        # ---------------- label -----------------
        label = p["itemLabel"]["value"].lower()
        property_dictionary = add_to_dictionary(label, uri, property_dictionary)
        # ---------------- alt label -------------
        if 'itemAltLabel' in p:
            alt_label_list = [x.strip().lower() for x in p["itemAltLabel"]["value"].split(', ')]
            property_dictionary = add_to_dictionary(alt_label_list, uri, property_dictionary)
            all_labels_properties = all_labels_properties + alt_label_list
            property_dictionary = add_to_dictionary(alt_label_list, uri, property_dictionary)

        # =============== make label collection ==========
        all_labels_properties.append(label)
    # check whether there is any repetition.
    # print([item for item, count in collections.Counter(all_labels_instances).items() if count > 1])
    # pprint(process_properties())
    print(property_dictionary['heat capacity'])
    print(property_dictionary['boiling point'])

    return {'dict': property_dictionary, 'list': list(set(all_labels_properties))}


final_dictionary = {'class': process_classes(), 'entity': process_instances(), 'attribute': process_properties()}

with open('wiki_dictionary', 'w') as f:
    f.write(json.dumps(final_dictionary))

# instances have
#                    item.value   :  the uri
#              altLabel_list.value:  the alt_labels
#                          x.value: the formula
