import json, re, random
import os
from fuzzywuzzy import fuzz

import chemparse
from .locations import RASA_JPS_DIR

word_map = {'hydrogen': 'H2', 'water': 'H2O', 'oxygen': 'O2', 'benzene': 'C6H6', 'methane': 'CH4',
            'hydrogen peroxide': 'H2O2'}

class SpeciesValidator:

    def __init__(self):
        with open(os.path.join(RASA_JPS_DIR, 'ontocompchem_dict')) as f:
            self.ontocompchem_species_dict = json.loads(f.read())
            f.close()

        with open(os.path.join(RASA_JPS_DIR, 'ontokin_dict')) as f:
            self.ontokin_species_dict = json.loads(f.read())

        # with open('ontokin_dict') as f:
        #     self.ontokin_species_dict = json.loads(f.read())
        #     f.close()

    def normalize_formula(self, s):
        s = s.replace(' ', '').strip().upper()  # remove space, make them capital.
        pattern = r'[a-zA-Z]+[0-9]*'
        if s == '':
            return s
        else:
            component_list = re.findall(pattern, s)
            temp = []
            for c in component_list:
                c_with_end = c
                tail_pattern = r'[a-zA-Z]+[1]'
                # to make sure the species into is the same as H2O1 and H2O, the 1 after O must be removed
                # find any c that fully matches the tail_pattern, replace 1, then remove the dollar sign
                match = re.fullmatch(tail_pattern, c_with_end)

                if match is not None:
                    c_transformed = c_with_end.replace('1', '')
                else:
                    c_transformed = c_with_end
                temp.append(c_transformed)

            # re-arrange the components alphabetically.
            # print(component_list)
            # remove all XX1 component ...
            return ''.join(sorted(temp))

    def validate(self, attribute, ontology, intent, species):
        intents = ['polarizability',
                   'dipole_moment',
                   'rotational_relaxation_collision',
                   'lennard_jones_well'] + ['symmetry_number',
                                            'rotational_constants',
                                            'vibration_frequency',
                                            'guassian_file',
                                            'spin_multiplicity',
                                            'formal_charge',
                                            'electronic_energy',
                                            'geometry_type']

        best_intent = \
            sorted([(word, fuzz.ratio(attribute, word)) for word in intents], key=lambda x: x[1], reverse=True)[0][0]

        if ontology == 'ontocompchem':
            species_dict = self.ontocompchem_species_dict[best_intent]
            if species in species_dict:
                return species
        elif ontology == 'ontokin':
            species_dict = self.ontokin_species_dict[best_intent]
            if species in species_dict:
                return species
        # else:
        #     return None

        if species.lower() in word_map:
            species = word_map[species.lower()]

        species = self.normalize_formula(species)
        # use regular expression to separate the components
        # rearrange them alphabetically
        # make the comparison
        # replace the species with what is in the dict ...
        if ontology == 'ontocompchem':
            species_dict = self.ontocompchem_species_dict[best_intent]
        elif ontology == 'ontokin':
            species_dict = self.ontokin_species_dict[best_intent]
        else:
            return None
        # chem parse is terrible, use regular expression instead
        for species_in_dict in species_dict:
            original = species_in_dict  # this is important, this goes to the query ...
            transformed_species_in_dict = self.normalize_formula(species_in_dict)
            if species == transformed_species_in_dict:
                # there is a match, the species is available in the KG. return the original
                return original
            else:
                pass

        return None

        # focus on one

        # TODO: use regular expression to find whether the question can be answered (the dict contains the species)

    def select_species(self, intent):
        species_list = self.ontocompchem_species_dict[intent]
        # selected_species = random.choices(species_list, k=40)
        selected_species = species_list
        return selected_species

# speices_validator = SpeciesValidator()
# intent = 'vibration_frequency'
#
# selected = speices_validator.select_species(intent)
# pool = ['CL1O4', 'CL3O3TI1', 'C4H4O2', 'H4O2C4', 'H2', 'C7H12O2', 'MAMA', 'H2', 'H2O', '', 'C 13 H 12 O 1', 'C13H12O', 'h2o1',
#         'h2', 'CO2', 'h2O2', 'Cl4Ti1', 'CL4Ti']
# # pool = ['C4H4O2', 'H4O2C4', 'C11H24']
# for species in pool:
#     r = speices_validator.validate(intent, species)
#     print('the result for validation is', r)
#     print('------------')

# species = 'C8H17O-1-2'
# speices_validator = SpeciesValidator()
# intent = 'rotational_relaxation_collision'
# r = speices_validator.validate('ontokin',intent, species)
# print('the r is', r)
