import json
import random
import re
import os
from location import TRAINING_FILES_DIR
import nltk


def get_file_folder_dir(filename):
    return os.path.join(TRAINING_FILES_DIR, filename)


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

    add_sign = random.choice([' add_sign ', ' and '])
    reactants_string = add_sign.join(tmp)

    tmp = []
    products = [s.strip() for s in products]
    for r in products:
        if r in FORMULA_NAME_DICT:
            r = random.choice([r, random.choice(FORMULA_NAME_DICT[r])])
        tmp.append('[%s](species)' % r.strip())

    add_sign = random.choice([' add_sign ', ' and '])
    products_string = add_sign.join(tmp)

    equation = reactants_string + ' [equation-splitter](separator) ' + products_string
    half_equation = random.choice([reactants_string, products_string]).replace(' add_sign ',
                                                                               random.choice([' add_sign ', ' and ']))

    return equation, random_species, half_equation, reactants_string, products_string


def query_pce():
    name = 'query_pce_agent'
    questions = []
    with open(os.path.join(TRAINING_FILES_DIR, 'PCE_SMILES_LIST')) as f:
        PCE_SMILES_LIST = json.loads(f.read())
        f.close()
    properties = ['pce', 'power conversion efficiency']
    template_1 = '[%s](attribute) OPV with SMILES [%s](species)'
    template_2 = '[%s](attribute) of [%s](species)'
    templates = [template_1, template_2]
    for pce_smiles in PCE_SMILES_LIST:
        template = random.choice(templates)
        attribute = random.choice(properties)
        question = template % (attribute, pce_smiles)
        questions.append(question)

    block = '\n ## intent:%s\n' % name + '\n - ' + '\n - '.join(questions)
    return block



def query_thermocalculation():
    name = 'query_thermo_agent'
    questions = []



# 'what is the reaction rate of xx + xx '
# 'is the reaction reversible  '
def query_reaction_property():
    name = 'query_reaction_property'
    questions = []
    for equation in random.sample(ontokin_equations, min(round(200 * factor), len(ontokin_equations))):
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
    for equation in random.sample(ontokin_equations, min(round(200 * factor), len(ontokin_equations))):
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
    for equation in random.sample(ontokin_equations, min(round(200 * factor), len(ontokin_equations))):
        processed_equation = process_equations(equation)
        equation = processed_equation[0]
        random_species = processed_equation[1]
        half_equation = processed_equation[2]
        reactant_string = processed_equation[3]
        product_string = processed_equation[4]

        equation = random.choice([equation, half_equation])
        # 'what reaction have xxx and xxx as reactants and xxx and xxx as products'
        # reaction that produces xxx
        # reaction produces xxx
        # reaction that uses xxx as reactants
        connecting_words = ['containing', 'contains', 'that contains', 'which contain',
                            'which contains', 'that contain', 'has', 'have', 'having', 'with']

        produce_words = [
            '[uses](reaction use)',
            '[take](reaction use)',
            '[takes](reaction use)',
            '[that uses](reaction use)']

        use_words = [
            '[that produce](reaction produce)',
            '[that produces](reaction produce)',
            '[producing](reaction produce)',
            '[give](reaction produce)',
            '[gives](reaction produce)',
            '[yield](reaction produce)',
            '[yields](reaction produce)',
            '[produce](reaction produce)',
            '[produces](reaction produce)']

        # reaction and reactions
        reaction_word = random.choice(['reaction', 'reactions', 'chemical reaction', 'chemical reactions'])
        head = random.choice(heads)
        # reaction_component = random.choice([random_species, half_equation])
        connecting_word = random.choice(connecting_words)

        reactant_string = random.choice([random_species, reactant_string])
        product_string = random.choice([random_species, product_string])

        reactant_word = random.choice(['reactants', 'a reactant', 'reactant', 'material'])
        product_word = random.choice(['product', 'a product', 'products'])
        # 1. reactions that produces/uses xx (species/ reactant string/ product string )
        # 2. reactions that produces/uses xx and xx (reactant string/ product string)
        # 3. reactions that uses xx/(xx and xx) and produces xx/ (xx and xx)

        # 4. reactions with xxx/(xx and xx) as reactant(s)
        # 5. reactions containing xx/(xx and xx) as product(s)
        # 6. reactions with xxx/(xx and xx) as reactant and xxx/(xx and xx) as products

        use_word = random.choice(use_words)
        produce_word = random.choice(produce_words)
        q_1 = '%s %s %s' % (reaction_word, use_word, reactant_string)  # 1,2
        q_2 = '%s %s %s' % (reaction_word, produce_word, product_string)  # 1,2
        q_3 = '%s %s %s and %s %s' % (reaction_word, use_word, reactant_string, produce_word, product_string)  # 3

        # reactions with xx/ (xx and xx) as reactants
        q_4 = '%s %s %s [as %s](reaction use ahead)' % (reaction_word, connecting_word, reactant_string, reactant_word)
        # reactions with xx/ (xx and xx) as products
        q_5 = '%s %s %s [as %s](reaction produce ahead)' % (
        reaction_word, connecting_word, product_string, product_word)
        q_6 = '%s %s %s [as %s](reaction use ahead) and %s [as %s](reaction produce ahead)' \
              % (reaction_word, connecting_word, reactant_string, reactant_word, product_string, product_word)
        #
        #
        # q = '%s %s %s %s' % (head, reaction_word, use_and_produce_word, reaction_component)
        # # q = random.choice(heads) + ' reaction ' + random.choice(use_and_produce) + random.choice(
        # #     [random_species, half_equation])
        #
        # # q2 = '%s %s %s %s as [%s](reaction use)' %(head, reaction_word, use_and_produce_word, reaction_component,  )
        # q2 = head + reaction_word + use_and_produce_word + reaction_component + ' [as %s](reaction use) and ' % (
        #     random.choice(['reactants', 'reactant'])) + random.choice(
        #     [random_species, half_equation]) + ' [as %s](reaction produce) ' % (random.choice(['product', 'products']))
        # reaction that produces xx and xx (half component)
        # reaction that uses xx
        candidates = [q_1, q_2, q_3, q_4, q_5, q_6]
        for q in candidates:
            questions.append(' - ' + q)
    block = '\n ## intent:%s\n' % name + '\n ' + '\n'.join(questions)
    return block


# 'what is the a of b '
def create_simple_questions(name, species_list, properties):
    stop_tags = ['VB', 'IN', 'RBR']
    questions = []
    print('EXTRA_PROPERTIES', EXTRA_PROPERTIES)
    print('name', name)
    if name in EXTRA_PROPERTIES:
        for i in range(40):
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
            q2 = random.choice(heads) + ' [' + species + '](species)' + random.choice(
                [' ', ' quote ']) + '[' + _property + '](attribute)'
            flag = False
            for stop_p in stop_properties:
                if stop_p in q + q2:
                    flag = True
            if not flag:
                questions.append(' - ' + random.choice([q, q2]))

    questions = random.sample(questions, min(round(len(questions) / 5 * factor), len(questions)))
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

with open(get_file_folder_dir('EXTRA_PROPERTIES')) as f:
    EXTRA_PROPERTIES = json.loads(f.read())
    f.close()

with open(get_file_folder_dir('common_species')) as f:
    common_species = json.loads(f.read())
    f.close()

with open(get_file_folder_dir('ontocompchem_species')) as f:
    ontocompchem_species = json.loads(f.read())['species']
    ontocompchem_species = json.loads(ontocompchem_species)
    f.close()

with open(get_file_folder_dir('ontokin_species_and_equations')) as f:
    ontokin_species_and_equations = json.loads(f.read())
    f.close()

with open(get_file_folder_dir('ontokin_properties')) as f:
    ontokin_properties = json.loads(f.read())
    f.close()

with open(get_file_folder_dir('ontocompchem_properties')) as f:
    ontocompchem_properties = json.loads(f.read())
    f.close()


with open(get_file_folder_dir('FORMULA_NAME_DICT')) as f:
    FORMULA_NAME_DICT = json.loads(f.read())
    f.close()

stop_properties = ['reactant', 'molecular', 'name', 'pressure', 'geometry', 'element']

# heads = ['what is ', 'show ', 'show me ', 'give ', 'provide ', 'find ', 'find me ', 'get ', '']
heads = ['']
connects = [' containing ', ' that contains ', ' with ', ' having ', ' that has ', 'contains']
ontokin_species = json.loads(ontokin_species_and_equations['species'])
ontokin_equations = json.loads(ontokin_species_and_equations['equations'])

full_block = query_thermodynamic() + '\n' + query_quantum_chemistry() + '\n' + query_reaction_property() + '\n' \
             + select_mechanism_by_reaction() + '\n' + select_reaction_by_species() + '\n' + query_pce()
full_block = full_block.lower()

_RE_COMBINE_WHITESPACE = re.compile(r"[ ]+")
full_block = _RE_COMBINE_WHITESPACE.sub(" ", full_block).strip()

ontokin_part = query_thermodynamic() + query_reaction_property() + \
               select_mechanism_by_reaction() + select_reaction_by_species()  # reactions and thermo

ontocompchem_part = query_quantum_chemistry()

with open(get_file_folder_dir('ontokin_corpus'), 'w', encoding='utf-8') as f:
    f.write(ontokin_part)
    f.close()

with open(get_file_folder_dir('ontocompchem_corpus'), 'w', encoding='utf-8') as f:
    f.write(ontocompchem_part)
    f.close()

with open('test/data/nlu.md', 'w', encoding="utf-8") as f:
    f.write(full_block)
    f.close()
