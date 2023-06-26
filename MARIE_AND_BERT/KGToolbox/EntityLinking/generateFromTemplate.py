'''
From entity file and question template generating train/val/test files |jsonl


'''

#json: [mention, text]
import csv
import json
import random,re

TEMPLATE = '../data/template_training.csv'
#read templates
templates = []
with open(TEMPLATE, newline='') as csvfile:
    reader = csv.reader(csvfile, delimiter = '@')
    for row in reader:
        templates.append(row[0].strip())

questions = []


templateNum = len(templates)


def get_entity_num(format_str):
    return format_str.count("{}")
def get_entity_types_mix(format_str):
    instance_starts = [m.start() for m in re.finditer('{INSTANCE}', format_str)]
    class_starts = [m.start() for m in re.finditer('{CLASS}', format_str)]
    shape_starts = [m.start() for m in re.finditer('{SHAPE}', format_str)]
    sorted_idxs = sorted(class_starts + instance_starts + shape_starts)
    e_types = []
    for idx in sorted_idxs:
        if idx in instance_starts:
            e_types.append('instance')
        elif idx in class_starts:
            e_types.append('class')
        else:
            e_types.append('shape')
    return e_types




def toolong(name):
    return True if len(name) >= 100  else False

def generate_questions(seed):
    #Train 1, test 10, valid 100
    random.seed(seed)

    with open('./ontokin.jsonl', 'rt') as f:
        for line in list(f)[0:]:
            item = json.loads(line.strip())
            name = item['entity']
            id = item['idx']
            text = item['text']
            othernames = []
            if toolong(name):
                continue
            othernames.append(name)
            #if id in aliases:
            #    othernames.extend(aliases[id].copy())
            for chosen in othernames:
                template = templates[random.randrange(0, 14)]
                gq = template.format(chosen)
                questions.append({'mention':chosen, 'text':gq, 'id':id, 'entity':name, 'des':text})
    return questions


#TODO: Note, exclude any entities longer than 30!
def generate_questions_multientity(filepath,num_to_gen, seed, single_entity=True):
    #Train 1, test 10, valid 100
    random.seed(seed)
    alle = []
    with open(filepath, 'rt') as f:
        for line in list(f)[0:]:
            item = json.loads(line.strip())
            name = item['entity']
            id = item['idx']
            text = item['text']
            if len(name)<=30:
                alle.append((id, name, text))
    NUM_ENTITY = len(alle)
    for i in range(num_to_gen):
        template = templates[random.randrange(0, len(templates))]
        print(template)
        num_entity = get_entity_num(template) if not single_entity else 1
        repeat = get_entity_num(template) if single_entity else 1
        qes= []
        for i in range(num_entity):
            e = alle[random.randrange(0, NUM_ENTITY)]
            for j in range(repeat): #repeat same entity of single entity template
                qes.append(e)
        ids = [e[0] for e in qes]
        names = [e[1] for e in qes]
        des = [e[2] for e in qes]
        types = [4 for e in qes]
        print(names)
        gq = template.format(*names)
        questions.append({'mention':names, 'text':gq, 'id':ids, 'entity':names, 'des':des,'types':types})
    return questions
#print(questions)
#print(questions)

def generate_questions_SMILES(num_to_gen, seed, single_entity=True):
    #Train 1, test 10, valid 100
    random.seed(seed)
    alle = []
    with open('../data/pubchem5000withSMILE_less.jsonl', 'rt') as f:
        for line in list(f)[0:]:
            item = json.loads(line.strip())
            name = item['iupac']
            smile = item['smile']
            id = item['idx']
            text = item['text']
            if len(name) <= 30 and len(smile)<=30:
                alle.append((id, smile, text))
    NUM_ENTITY = len(alle)
    for i in range(num_to_gen):
        template = templates[random.randrange(0, len(templates))]
        print(template)
        num_entity = get_entity_num(template) if not single_entity else 1
        repeat = get_entity_num(template) if single_entity else 1
        qes= []
        for i in range(num_entity):
            e = alle[random.randrange(0, NUM_ENTITY)]
            for j in range(repeat): #repeat same entity of single entity template
                qes.append(e)
        ids = [e[0] for e in qes]
        names = [e[1] for e in qes]
        des = [e[2] for e in qes]
        types = [1 for e in qes]
        print(names)
        gq = template.format(*names)
        questions.append({'mention':names, 'text':gq, 'id':ids, 'entity':names, 'des':des,'types':types})
    return questions
#print(questions)

shapefile = '../mops_shape.jsonl'

def generate_questions_mix_type(classfile, instancefile, seed):
    #Train 1, test 10, valid 100
    random.seed(seed)
    instances = []
    classes = []
    shapes = []
    with open(instancefile, 'rt') as f:
        for line in list(f)[0:]:
            item = json.loads(line.strip())
            name = item['title']
            id = item['idx']
            text = item['text']
            instances.append((id, name, text))
    NUM_INSTANCE = len(instances)
    with open(classfile, 'rt') as f:
        for line in list(f)[0:]:
            item = json.loads(line.strip())
            name = item['title']
            id = item['idx']
            text = item['text']
            for repeat in range(20):
                classes.append((id, name, text))
    NUM_CLASS = len(classes)
    with open(shapefile, 'rt') as f:
        for line in list(f)[0:]:
            item = json.loads(line.strip())
            name = item['title']
            id = item['idx']
            text = item['text']
            shapes.append((id, name, text))
    NUM_SHAPE = len(shapes)
    for i in range(NUM_CLASS):
        template = templates[random.randrange(0, len(templates))]
        type_entities = get_entity_types_mix(template)

        qes= []
        types = []
        is_first_e = True
        first_e = classes[i]
        for t in type_entities:#Loop through all entities in a template
            if t == 'instance':
                e = instances[random.randrange(0, NUM_INSTANCE)]
                types.append(5)
            elif t == 'shape':
                e = shapes[random.randrange(0, NUM_SHAPE)]
                types.append(6)
            elif is_first_e is True:#First entity in the template, use the indexed one
                e = first_e
                types.append(3)
                is_first_e = False
            else:#For second or third entity in the question, use a randomly selected entity
                e = classes[random.randrange(0, NUM_CLASS)]
                types.append(3)
            qes.append(e)
        ids = [e[0] for e in qes]
        names = [e[1] for e in qes]
        des = [e[2] for e in qes]

        print(names)
        template = template.replace('CLASS','').replace('INSTANCE','').replace('SHAPE','')
        gq = template.format(*names)
        questions.append({'mention':names, 'text':gq, 'id':ids, 'entity':names, 'des':des,'types':types})
    return questions



questions = generate_questions(1)
#questions = generate_questions_multientity('../generate_training_data/ontokin.jsonl', 1000, 10, False  )
#questions = generate_questions_SMILES(2000, 1)
with open('valid_ontokin.jsonl', 'w') as wf:
    for entry in questions:
        json.dump(entry, wf)
        wf.write('\n')
#generate questions, save to json