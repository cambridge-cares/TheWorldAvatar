import json
import math
import random
import re

import nltk


def get_pos_tags(p_label):
    stop_tags = ['DT']
    text = nltk.word_tokenize(p_label)
    # text = [ ps.stem(w.lower()) for w in text]
    word_tag_pairs = nltk.pos_tag(text)
    tags = [pair[1] for pair in word_tag_pairs if pair[1] not in stop_tags]
    tag_chain = ' '.join(tags)
    # rearrange the elements in the

    return tag_chain


# There are in total 5 question types
# for each question type, create 500 * 10 questions
# produces nlu.md

def process_equations(equation):
    equation = equation.replace('[=]', '=]')
    equation = equation.replace(' + ', ' add_sign ')
    # split the equation, reform it
    reactants = equation.split('=]')[0]
    products = equation.split('=]')[1]
    reactants = reactants.replace('add_sign', '$').replace('=]', '$').split('$')
    products = products.replace('add_sign', '$').replace('=]', '$').split('$')

    random_species = '[' + random.choice(reactants + products).strip() + '](species)'

    tmp = []
    reactants = [s.strip() for s in reactants]
    for r in reactants:
        if r in FORMULA_NAME_DICT:
            r = random.choice([r, random.choice(FORMULA_NAME_DICT[r])])
        tmp.append('[%s](species)' % r.strip())
    reactants_string = ' add_sign '.join(tmp)

    tmp = []
    products = [s.strip() for s in products]
    for r in products:
        if r in FORMULA_NAME_DICT:
            r = random.choice([r, random.choice(FORMULA_NAME_DICT[r])])
        tmp.append('[%s](species)' % r.strip())
    products_string = ' add_sign '.join(tmp)

    equation = reactants_string + ' separator(separator) ' + products_string
    half_equation = random.choice([reactants_string, products_string]).replace(' add_sign ',
                                                                               random.choice([' add_sign ', ' and ']))

    return equation, random_species, half_equation


# 'what is the reaction rate of xx + xx '
# 'is the reaction reversible  '
def query_reaction_property():
    name = 'query_reaction_property'
    questions = []
    for equation in random.sample(ontokin_equations, round(200 * factor)):
        processed_equation = process_equations(equation)
        equation = processed_equation[0]
        half_equation = processed_equation[2]
        equation = random.choice([equation, half_equation])

        # can A + B = C + D be reserved
        # is A + B = C + D reversible
        # is A + B = C + D a reversible reaction
        # is the reaction A + B = C + D reversible?
        # what is the reaction rate of A + B = C + D
        q = random.choice(heads) + ' ' + random.choice(['[reaction rate](attribute) of ', '[rate of reaction]('
                                                                                          'attribute) of ']) + equation
        q2 = 'is ' + random.choice([' the reaction ', '']) + equation + ' [reversible](attribute)'
        q3 = 'can ' + random.choice([' the reaction ', '']) + equation + ' be [reversed](attribute)'
        q4 = 'is ' + equation + ' a [reversible](attribute) reaction'
        questions = questions + [q, q2, q3, q4]
    block = '\n ## intent:%s\n' % name + '\n - ' + '\n - '.join(questions)
    return block


# 'what mechanism contains xxx xxx'
def select_mechanism_by_reaction():
    name = 'select_mechanism_by_reaction'
    questions = []
    for equation in random.sample(ontokin_equations, round(200 * factor)):
        processed_equation = process_equations(equation)
        equation = processed_equation[0]
        half_equation = processed_equation[2]
        equation = random.choice([equation, half_equation])
        # 'what mechanism contains xxx xxx'

        q = random.choice(heads) + random.choice([' mechanism ', ' mechanisms ']) + \
            random.choice(connects) + \
            random.choice([' the reaction ', ' reaction ']) + equation

        questions.append(q)
    block = '\n ## intent:%s\n' % name + '\n - ' + '\n - '.join(questions)
    return block


# 'what reaction produces xxx'
def select_reaction_by_species():
    name = 'select_reaction_by_species'
    questions = []
    for equation in random.sample(ontokin_equations,  round(200 * factor)):
        processed_equation = process_equations(equation)
        equation = processed_equation[0]
        random_species = processed_equation[1]
        half_equation = processed_equation[2]
        equation = random.choice([equation, half_equation])
        # 'what reaction have xxx and xxx as reactants and xxx and xxx as products'
        # reaction that produces xxx
        # reaction produces xxx
        # reaction that uses xxx as reactants
        use_and_produce = [' [that produces](produce) ',
                           ' [producing](produce) ',
                           ' [give](produce) ',
                           ' [gives](produce) ',
                           ' [yield](produce) ',
                           ' [yields](produce) ',
                           ' [produce](produce) ',
                           ' [uses](use) ',
                           ' [take](use) ',
                           ' [takes](use) ',
                           ' [that uses](use) ']

        q = random.choice(heads) + ' reaction ' + random.choice(use_and_produce) + random.choice(
            [random_species, half_equation])

        q2 = random.choice(heads) + ' reaction ' + random.choice(use_and_produce) + random.choice(
            [random_species, half_equation]) + ' as reactants(use) and ' + random.choice(
            [random_species, half_equation]) + ' as products(produce) '

        questions.append(' - ' + q)
        questions.append(' - ' + q2)
    block = '\n ## intent:%s\n' % name + '\n ' + '\n'.join(questions)
    return block


# 'what is the a of b '
def create_simple_questions(name, species_list, properties):
    stop_tags = ['VB', 'IN', 'RBR']
    questions = []
    print('EXTRA_PROPERTIES', EXTRA_PROPERTIES)
    print('name', name)
    if name in EXTRA_PROPERTIES:
        for i in range(5):
            extra_properties = EXTRA_PROPERTIES[name]
            properties = properties + extra_properties
    for _property in properties:
        tags = get_pos_tags(_property)
        # for species in random.sample(species_list, 300):  # + common_species:
        for species in species_list:  # + common_species:
            species = species.replace('[', '').replace(']', '')
            if species in FORMULA_NAME_DICT:
                names = FORMULA_NAME_DICT[species]
                species_options = [species] + names
                species = random.choice(species_options)
                species = species.encode('utf-8', 'ignore').decode('utf-8', 'ignore')

            q = random.choice(heads) + ' [' + _property + '](attribute) ' + random.choice(
                ['of', ' ']) + ' [' + species + '](species)'
            q2 = random.choice(heads) + ' [' + species + '](species) [' + _property + '](attribute)'
            questions.append(' - ' + random.choice([q, q2]))

    questions = random.sample(questions, round(len(questions) / 5 * factor))
    block = '\n ## intent:%s\n' % name + '\n ' + '\n'.join(questions)
    return block


# ontokin questions, what is the property of species
def query_thermodynamic():
    name = "query_thermodynamic"
    return create_simple_questions(name, ontokin_species, ontokin_properties)


# ontocompchem question, what is the property of species
def query_quantum_chemistry():
    name = "query_quantum_chemistry"
    return create_simple_questions(name, ontocompchem_species, ontocompchem_properties)

factor = 1

with open('EXTRA_PROPERTIES') as f:
    EXTRA_PROPERTIES = json.loads(f.read())
    f.close()

with open('common_species') as f:
    common_species = json.loads(f.read())
    f.close()

with open('ontocompchem_species') as f:
    ontocompchem_species = json.loads(f.read())['species']
    ontocompchem_species = json.loads(ontocompchem_species)
    f.close()

with open('ontokin_species_and_equations') as f:
    ontokin_species_and_equations = json.loads(f.read())
    f.close()

with open('ontokin_properties') as f:
    ontokin_properties = json.loads(f.read())
    f.close()

with open('ontocompchem_properties') as f:
    ontocompchem_properties = json.loads(f.read())
    f.close()

with open('../Wiki_basic_info/FORMULA_NAME_DICT') as f:
    FORMULA_NAME_DICT = json.loads(f.read())
    f.close()

heads = ['what is ', 'show ', 'show me ', 'give ', 'provide ', 'find ', 'find me ', 'get ', '']
connects = [' containing ', ' that contains ', ' with ', ' having ', ' that has ']
ontokin_species = json.loads(ontokin_species_and_equations['species'])
ontokin_equations = json.loads(ontokin_species_and_equations['equations'])

full_block = query_thermodynamic() + '\n' + query_quantum_chemistry() + '\n' + query_reaction_property() + '\n' \
             + select_mechanism_by_reaction() + '\n' + select_reaction_by_species()
full_block = full_block.lower()

_RE_COMBINE_WHITESPACE = re.compile(r"[ ]+")
full_block = _RE_COMBINE_WHITESPACE.sub(" ", full_block).strip()

with open('test/data/nlu.md', 'w', encoding="utf-8") as f:
    f.write(full_block)
    f.close()
