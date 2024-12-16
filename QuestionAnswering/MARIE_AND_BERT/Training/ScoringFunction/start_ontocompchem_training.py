import os, sys
import random

sys.path.append("../..")
import pandas as pd
import pickle
from Marie.Util.location import DATA_DIR
from Marie.Util.NHopExtractor import HopExtractor

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
hop_extractor = HopExtractor(dataset_dir=os.path.join(DATA_DIR, "ontocompchem_latent_40"),
                             dataset_name="ontocompchem_calculation")

rel2idx_pkl = open(os.path.join(DATA_DIR, "ontocompchem_latent_40/relation2idx.pkl"), 'rb')
relations = pickle.load(rel2idx_pkl)
ent2idx_pkl = open(os.path.join(DATA_DIR, "ontocompchem_latent_40/entity2idx.pkl"), 'rb')
entities_dict = pickle.load(ent2idx_pkl)

property_labels = [["geometry type"],
                   ["vibration frequency"],
                   ["rotational constant"],
                   ["symmetry number"]]

property_names = ["hasGeometryType", "oc:hasFrequencies", "oc:hasRotationalConstants",
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
    os.path.join(DATA_DIR, "ontocompchem_latent_40/ontocompchem_calculation-train.txt"),
    sep='\t', header=None)

"""
1. Find the species used in calculation 
2. Find the property leading to the numerical node 
3. Choose a property. 
4. Create the question 
"""

p_dict = {'oc:hasFrequencies1': 'latent_relation_2',
          'oc:hasRotationalConstants1': 'latent_relation_3',
          'oc:hasRotationalSymmetryNumber1': 'latent_relation_4',
          'hasGeometryType1': 'latent_relation_5',
          'oc:hasFrequencies2': 'latent_relation_6',
          'oc:hasRotationalConstants2': 'latent_relation_7',
          'oc:hasRotationalSymmetryNumber2': 'latent_relation_8',
          'hasGeometryType2': 'latent_relation_9'
          }
def find_species_paris(df):
    """
    :param df: all the triples from ontocompchem
    :return: a triple of (species, property, value node)
    """

    result = []

    derived_triples = []

    species_calculation_pairs = df.loc[df.iloc[:, 1] == "oc:hasUniqueSpecies"]  # calculation - cd:has - species
    # make a set of species
    species = species_calculation_pairs.iloc[:, 2].values.tolist()
    repeated_species = [x for n, x in enumerate(species) if x in species[:n]]
    print('repeated_species',repeated_species)

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
                    value_node = value_nodes[0]
                    # value_node_idx = entities_dict[value_node]
                    value_nodes_random_copy = hop_extractor.extract_neighbour_from_label(s)
                    # print('tail:', value_node)
                    if value_node in value_nodes_random_copy:
                        value_nodes_random_copy.remove(value_node)
                    # print('neighbours', value_nodes_random_copy)

                    rel_label = p_dict[p + '1']
                    rel_idx = relations[rel_label]
                    # rel_idx = rel_label
                    triple = (s, p, value_node, rel_idx)
                    derived_triples.append((s, p + '1', value_node))
                    # derived_triples.append((s, 'latent_relation_1', n))
                    # derived_triples.append((c, p + '2', value_node))
                    if s not in repeated_species:
                        result.append(triple)
                        try:
                            for fake_tail_node in [random.choice(value_nodes_random_copy)]:
                                # check whether the triple exist or not ...
                                # s_idx = entities_dict[s]

                                fake_triple = (s, p, fake_tail_node, 0)
                                # result.append(fake_triple)
                        except:
                            # print(value_nodes_random)
                            # print(value_nodes[0])
                            pass

    derived_triples = pd.DataFrame(list(set(derived_triples)))
    derived_triples.to_csv(os.path.join(DATA_DIR, 'ontocompchem_latent_40', 'derived_triples.tsv'), sep='\t',
                           index=False, header=False)

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
    # return entity
    return entities_dict[entity]


def make_questions(triples):
    q_s_vn_triples = []
    cross_graph = []

    for triple in triples:
        (species, property_iri, value_node, rel_idx) = triple
        # make the questions using templates
        labels = property_names_label_dict[property_iri]
        template = "%s"
        for l in labels:
            question = template % l
            triple = (question, translate_to_idx(species), translate_to_idx(value_node), rel_idx)
            q_s_vn_triples.append(triple)

            cross_triple = (question, species, 1, value_node)
            cross_graph.append(cross_triple)

        # print(q_s_vn_triples)
    return pd.DataFrame(q_s_vn_triples), pd.DataFrame(cross_graph)


q_s_vn_triples, cross_graph_triples = make_questions(s_p_vn_triples)
# print(q_s_vn_triples)
q_s_vn_triples.columns = ["question", "head", "tail", "rel"]
q_s_vn_triples.to_csv(os.path.join(DATA_DIR, "ontocompchem_latent_40/score_model_training.tsv"), sep='\t')


cross_graph_triples.columns = ["question", "head", "domain", "answer"]
cross_graph_triples.to_csv(os.path.join(DATA_DIR, "CrossGraph/ontochemistry_cross_score.tsv"), sep='\t')
