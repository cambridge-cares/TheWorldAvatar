import os, sys

sys.path.append("../..")
import pandas as pd
import pickle
from Marie.Util.location import DATA_DIR
from Marie.Util.NHopExtractor import HopExtractor

dataset_dir = "ontocompchem"
dataset_name = "ontocompchem"
full_dir = os.path.join(DATA_DIR, "CrossGraph", dataset_dir)
hop_extractor = HopExtractor(dataset_dir=full_dir,
                             dataset_name=dataset_name)

rel2idx_pkl = open(os.path.join(full_dir, "relation2idx.pkl"), 'rb')
relations = pickle.load(rel2idx_pkl)
ent2idx_pkl = open(os.path.join(full_dir, "entity2idx.pkl"), 'rb')
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
# map h and t with question
ontocompchem_triples = pd.read_csv(
    os.path.join(DATA_DIR, "CrossGraph", "ontocompchem/ontocompchem-train.txt"),
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
    print('repeated_species', repeated_species)

    calculations = species_calculation_pairs.iloc[:, 0].values.tolist()
    calculations_node_pairs = df.loc[df.iloc[:, 1] == "gc:isCalculationOn"]  # calculation - isCalculatedOn - node
    for s, c in zip(species, calculations):
        # find all the nodes related to this calculation via isCalculationOn
        nodes = calculations_node_pairs.loc[df.iloc[:, 0] == c].iloc[:, 2].values.tolist()
        # find all the node - property - value node triples
        for n in nodes:
            for p in property_names:
                # n - p - vn, where n is the head entity, p is the relation, vn to be the value node
                value_nodes = df.loc[(df.iloc[:, 1] == p) & (df.iloc[:, 0] == n)].iloc[:,
                              2].values.tolist()  # with p, with n
                if len(value_nodes) == 1:
                    value_node = value_nodes[0]
                    rel_label = p_dict[p + '1']
                    if rel_label not in relations:
                        relations[rel_label] = len(relations)
                    rel_idx = relations[rel_label]

                    triple = (s, p, value_node, rel_idx)
                    derived_triples.append((s, p + '1', value_node))
                    if s not in repeated_species:
                        result.append(triple)

    # By deriving implicit relations, new relations and new triples will be created.
    # As a result, the relation dictionaries and the triples are updated and will overwrite the existing
    # triples and dictionaries.
    file = open(os.path.join(full_dir, 'relation2idx.pkl'), 'wb')
    pickle.dump(relations, file)
    file = open(os.path.join(full_dir, 'idx2relation.pkl'), 'wb')
    relations_reversed = sorted(list(set(relations.keys())))
    pickle.dump(relations_reversed, file)
    derived_triples = pd.DataFrame(list(set(derived_triples)))
    derived_triples.to_csv(os.path.join(full_dir, 'ontocompchem-train.tsv'), sep='\t',
                           index=False, header=False)

    return result


s_p_vn_triples = find_species_paris(ontocompchem_triples)  # (species, property, value node)


def translate_to_idx(entity):
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
    return pd.DataFrame(q_s_vn_triples), pd.DataFrame(cross_graph)


q_s_vn_triples, cross_graph_triples = make_questions(s_p_vn_triples)
q_s_vn_triples.columns = ["question", "head", "tail", "rel"]
q_s_vn_triples.to_csv(os.path.join(DATA_DIR, "CrossGraph", dataset_dir, "score_model_training.tsv"), sep='\t')
