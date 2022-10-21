import os, sys
import random

sys.path.append("../..")
import pandas as pd
import pickle
from Marie.Util.location import DATA_DIR

# TODO: create question set, containing the question, the h and t

# Competency questions:
# Show me the vibration frequency of H2O2
# What is the symmetry number of C8H14
# What is the spin multiplicity of C8H14
# Electronic energy of C2H2O2
# Show the formal charge of C3H6
# What is the geometry type of C2H2O2
# find unique relations
# ==================================
# 'hasGeometryType': 1, 'oc:hasFrequencies': 2, 'oc:hasRotationalConstants': 3, 'oc:hasRotationalSymmetryNumber'


rel2idx_pkl = open(os.path.join(DATA_DIR, "ontocompchem_calculation/relation2idx.pkl"), 'rb')
relations = pickle.load(rel2idx_pkl)
ent2idx_pkl = open(os.path.join(DATA_DIR, "ontocompchem_calculation/entity2idx.pkl"), 'rb')
entities_dict = pickle.load(ent2idx_pkl)

property_labels = [["geometry type"],
                   ["vibration frequency"],
                   ["rotational constant"],
                   ["rotational symmetry number"]]

property_names = ["oc:hasGeometryType", "oc:hasFrequencies", "oc:hasRotationalConstants",
                  "oc:hasRotationalSymmetryNumber"]

property_names_label_dict = {}
for p_l, p_n in zip(property_labels, property_names):
    property_names_label_dict[p_n] = p_l

"""
oc:hasGeometryType
oc:hasFrequencies
oc:hasRotationalConstants
oc:hasRotationalSymmetryNumber
"""
# TODO: map h and t with question
ontocompchem_triples = pd.read_csv(
    os.path.join(DATA_DIR, "ontocompchem_calculation/ontocompchem_calculation-train.txt"),
    sep='\t', header=None)

"""
1. Find the species used in calculation 
2. Find the property leading to the numerical node 
3. Choose a property. 
4. Create the question 
"""


def find_species_paris(df):
    """
    :param df: all the triples from ontocompchem
    :return: a triple of (species, property, value node)
    """

    result = []
    species_calculation_pairs = df.loc[df.iloc[:, 1] == "oc:hasUniqueSpecies"]  # calculation - cd:has - species
    # make a set of species
    species = species_calculation_pairs.iloc[:, 2].values.tolist()
    calculations = species_calculation_pairs.iloc[:, 0].values.tolist()
    # species = list(set(species))
    calculations_node_pairs = df.loc[df.iloc[:, 1] == "gc:isCalculationOn"]  # calculation - isCalculatedOn - node

    value_nodes_random = df.loc[df.iloc[:, 2].str.contains("Value")].iloc[:, 2].values.tolist()
    value_nodes_random = list(set(value_nodes_random))
    for s, c in zip(species, calculations):
        # find all the nodes related to this calculation via isCalculationOn
        nodes = calculations_node_pairs.loc[df.iloc[:, 0] == c].iloc[:, 2].values.tolist()
        # find all the node - property - value node triples
        for n in nodes:
            for p in property_names:
                # n - p - vn
                value_nodes = df.loc[(df.iloc[:, 1] == p) & (df.iloc[:, 0] == n)].iloc[:,
                              2].values.tolist()  # with p, with n
                if len(value_nodes) == 1:
                    value_nodes_random_copy = value_nodes_random
                    value_nodes_random_copy.remove(value_nodes[0])
                    triple = (s, p, value_nodes[0], 1)
                    result.append(triple)

                    fake_tail_node = random.choice(value_nodes_random_copy)
                    fake_triple = (s, p, fake_tail_node, 0)
                    result.append(fake_triple)



    return result
    # node - property - value_node

    # find all row with gc:isCalculationOn and
    # then find the


# def search_properties(df):
#     property_dict  = {}
#     for p in property_names:
#         property_result =
#


s_p_vn_triples = find_species_paris(ontocompchem_triples)  # (species, property, value node)


def translate_to_idx(entity):
    return entities_dict[entity]


def make_questions(triples):
    q_s_vn_triples = []

    for triple in triples:
        (species, property_iri, value_node, score) = triple
        # make the questions using templates
        labels = property_names_label_dict[property_iri]
        template = "what is the %s of"
        for l in labels:
            question = template % l
            triple = (question, translate_to_idx(species), translate_to_idx(value_node), score)
            q_s_vn_triples.append(triple)

        # print(q_s_vn_triples)
    return pd.DataFrame(q_s_vn_triples)


q_s_vn_triples = make_questions(s_p_vn_triples)
# print(q_s_vn_triples)
q_s_vn_triples.columns = ["question", "head", "tail", "score"]
q_s_vn_triples.to_csv(os.path.join(DATA_DIR, "ontocompchem_calculation/score_model_training.tsv"), sep='\t')
