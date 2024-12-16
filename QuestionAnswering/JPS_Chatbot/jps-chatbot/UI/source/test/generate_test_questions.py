import json
import random

question_pool = ['find the', 'what is the', 'show me the', '', 'show the']
# waht is the chemical structure of Acetylene c2h2
ontocompchem_templates = {
    'ELECTRONIC_ENERGY': '%s electronic energy of %s',
    'FORMAL_CHARGE': '%s formal charge of %s',
    'GAUSSIAN_FILE': '%s gaussian files of %s',
    'GEOMETRY_TYPE': '%s geometry type of %s',
    'SPIN_MULTIPLICITY': '%s spin multiplicity of %s',
    'SYMMETRY_NUMBER': '%s symmetry number of %s',
    'VIBRATION_FREQUENCY': '%s vibration frequency of %s'

}

the_super_question_set = []


for ontocompchem_question_key in ontocompchem_templates:
    with open('ontocompchem_dict_%s' % ontocompchem_question_key) as f:
        content = f.read()
        one_ontokin_list = json.loads(content.replace(' ',''))
        template = ontocompchem_templates[ontocompchem_question_key]
        for i in range(0, 20):
            wh = random.choice(question_pool)
            question = (template % (wh, random.choice(one_ontokin_list))).strip()
            print(question)
            the_super_question_set.append(question)


ontokin_templates = {'DIPOLE_MOMENT': '%s dipole moment of %s',
                     'LENNARD_JONES_WELL_DEPTH': '%s lennard jones well depth of %s',
                     'POLARIZABILITY': '%s polarizability of %s',
                     'RELAXATION_COLLISION': '%s rotational relaxation collision number %s'}

for ontokin_key in ontokin_templates:
    with open('ontokin_dict_%s' % ontokin_key) as f:
        content = f.read()
        one_ontokin_list = json.loads(content.replace(' ',''))
        template = ontokin_templates[ontokin_key]
        for i in range(0, 20):
            wh = random.choice(question_pool)
            question = (template % (wh, random.choice(one_ontokin_list))).strip()
            print(question)
            the_super_question_set.append(question)

reactions_templates = ['what is the reaction rate of %s', 'what reaction produces %s', 'is the reaction %s reversible',
                          'what reaction has %s reactant', 'what mechanism contains %s']

def process_reactions(equation):
    reactants = [x.strip() for x in equation.split('=]')[0].strip().split('+')]
    products = [x.strip() for x in equation.split('=]')[1].strip().split('+')]
    print('reactants', reactants)
    print('products', products)
    return reactants, products


with open('reactions') as f:
    reactions = json.loads(f.read())
    for reaction in random.choices(reactions, k=20):
        equation = reaction['Equation']
        r = process_reactions(equation)
        reactants = r[0]
        products = r[1]
        equation = equation.replace('=]', '->')
        # reactions_templates = ['what is the reaction rate of %s', 'what reaction produces %s',
        #                        'is the reaction %s reversible',
        #                        'what reaction has %s reactant', 'what mechanism contains %s']

        q1 = 'what is the reaction rate of %s' % equation
        q2 = 'what reaction produces %s' % random.choice(products)
        q3 = 'is the reaction %s reversible' % equation
        q4 = 'what reaction has %s reactant' % random.choice(reactants)
        q5 = 'what mechanism contains %s' % equation

        tmp = [q1, q2, q3, q4, q5]
        the_super_question_set = the_super_question_set + tmp

print(the_super_question_set)
print('===================')
for x in the_super_question_set:
    print(x)


wiki_questions = ['']
