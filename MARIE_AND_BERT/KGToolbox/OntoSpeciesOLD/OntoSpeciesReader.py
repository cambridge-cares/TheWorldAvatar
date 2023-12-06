import json
import os
import time
from pprint import pprint

import pandas
import pandas as pd
from SPARQLWrapper import SPARQLWrapper, JSON
from Marie.Util.Web.SPARQLWarehouse import ONTOSPECIES_ALL_SPECIES, ONTOSPECIES_ALL_PROPERTIES_TEMPLATE
from Marie.Util.location import ARCHIVE_DIR, DATA_DIR
from ast import literal_eval


class OntoSpeciesReader:

    def __init__(self):
        self.all_species_iri = []
        self.dataset_path = os.path.join(DATA_DIR, 'CrossGraph', 'ontospecies')
        self.p_iri_list = []
        self.p_iri_name = []
        self.p_label_list = []
        self.species_iri_list = []
        self.p_iri_name_mapping = {}
        self.species_mapping = {}
        self.property_names = {"type": "type",
                               "hasGeometryType_latent": "geometry type",
                               "oc:hasFrequencies_latent": "vibration frequency",
                               "oc:hasRotationalConstants_latent": "rotational constants",
                               "oc:hasRotationalSymmetryNumber_latent": "rotational symmetry number ",
                               "casRegistryID": "cas registry id",
                               "inChI": "inchi",
                               "SMILES": "smiles",
                               "hasCharge": "charge",
                               "hasMolecularWeight": "molecular weight",
                               "hasMolecularFormula": "molecular formula",
                               "hasGeometry": "geometry",
                               "spinMultiplicity": "spin multiplicity",
                               "hasAtom": "atoms",
                               "hasAtomicBond": "atomic bond",
                               "hasStandardEnthalpyOfFormation": "standard enthalpy of formation",
                               "pubChemCID": "CID",
                               }

    def query_blazegraph(self, query, namespace="ontospecies"):
        sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/" + namespace + "/sparql")
        sparql.setQuery(query)
        sparql.setReturnFormat(JSON)
        results = sparql.query().convert()
        return results

    def process_query_result(self, result_json):
        results = result_json['results']['bindings']
        for r in results:
            geomType = r['geomType']['value'].split('/')[-1]
            species = r['species']['value'].split('/')[-1]
            print(geomType, species)

    def find_all_properties_of_all_species(self):
        triples = []

        property_list = pd.read_csv(os.path.join(self.dataset_path, 'species_properties.csv'), sep='\t')
        self.p_iri_list = property_list['iri'].values.tolist()
        self.p_iri_list = [p.replace('<', '').replace('>', '') for p in self.p_iri_list]
        property_list.labels = property_list.labels.apply(literal_eval)
        self.p_label_list = property_list['labels'].values.tolist()
        failed_species = []
        for p_iri in self.p_iri_list:
            if '#' in p_iri:
                h = p_iri.split('#')[-1]
            elif ':' in p_iri and '/' not in p_iri:
                h = p_iri.split(':')[-1]
            else:
                h = p_iri.split('/')[-1]
            self.p_iri_name.append(h)

        for p_iri, p_iri_name in zip(self.p_iri_list, self.p_iri_name):
            self.p_iri_name_mapping[p_iri_name] = p_iri
        with open(os.path.join(self.dataset_path, 'property_mapping.json'), 'w') as f:
            f.write(json.dumps(self.p_iri_name_mapping))
            f.close()

        counter = 0
        for species in self.all_species_iri:
            counter += 1
            try:
                SPARQL_ALL_PROPERTIES = self.find_all_properties_of_one_species(species)
                result = self.query_blazegraph(SPARQL_ALL_PROPERTIES, 'ontospecies')
                self.species_mapping[species] = result
                time.sleep(1)
                if counter % 1000 == 0 or counter == len(self.all_species_iri):
                    print('writing the results down')
                    with open(os.path.join(self.dataset_path, 'ontospecies.json'), 'w') as f:
                        f.write(json.dumps(self.species_mapping))
            except:
                failed_species.append(species)
                with open(os.path.join(self.dataset_path, 'failed_species'), 'w') as f:
                    f.write(json.dumps(failed_species))
                    f.close()
            print(f"{counter} of out {len(self.all_species_iri)}")

    def find_all_properties_of_one_species(self, species_iri=None):
        heads = 'SELECT DISTINCT' + ' '.join(['?' + p_name for p_name in self.p_iri_name]) + '\n'
        content = []
        for p_iri, p_iri_name in zip(self.p_iri_list, self.p_iri_name):
            line = f"<{species_iri}> <{p_iri}> {'?' + p_iri_name} ."
            line = "OPTIONAL {" + line + "}"
            content.append(line)
        content = '\n'.join(content)
        sparql_query = heads + '\n' + 'WHERE {' + content + '}'
        return sparql_query

    def find_all_species(self):
        rst = self.query_blazegraph(ONTOSPECIES_ALL_SPECIES)['results']['bindings']
        for r in rst:
            species = r['species']['value']
            self.all_species_iri.append(species)
        df_all_species = pd.DataFrame(self.all_species_iri)
        df_all_species.columns = ['species']
        df_all_species = df_all_species.reset_index(drop=True)
        df_all_species.to_csv(os.path.join(self.dataset_path, 'all_species.tsv'), sep='\t')
        self.all_species_iri = list(set(self.all_species_iri))

    def numerical_value_query(self, node_name):
        QUERY = """
            SELECT ?value ?unit 
            WHERE {
                <%s> <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#value> ?value .
            OPTIONAL {
                <%s> <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#units> ?unit .
                }
            } 
        """ % (node_name, node_name)
        rst = self.query_blazegraph(query=QUERY)
        return rst['results']['bindings']

    def create_triples(self):
        triples = []
        value_dict = {}
        all_species = json.loads(open(os.path.join(self.dataset_path, 'ontospecies.json')).read())
        counter = 0
        for species in all_species:
            counter += 1
            print(f"{counter} out of {len(all_species.keys())}")
            if counter % 1000 == 0:
                with open(os.path.join(self.dataset_path, 'ontospecies_value_dict.json'), 'w') as f:
                    f.write(json.dumps(value_dict))
                    f.close()
            data = all_species[species]
            if '#' in species:
                short_species = species.split('#')[-1]
            elif ':' in species and '/' not in species:
                short_species = species.split(':')[-1]
            else:
                short_species = species.split('/')[-1]
            vars = data["head"]["vars"]
            for row in data["results"]["bindings"]:
                for var in vars:
                    if var in row and var != "type":
                        d = row[var]
                        value = d['value']
                        data_type = d['type']
                        if data_type == "uri":
                            new_node = value.split('/')[-1]
                            bindings = self.numerical_value_query(value)
                            for b in bindings:
                                if 'value' in b and 'unit' in b:
                                    value = b['value']['value']
                                    unit = b['unit']['value']
                                    value_dict[new_node] = f"{value} {unit}"

                        elif data_type == "literal":
                            new_node = f"{short_species}_{var}"
                            value_dict[new_node] = value
                        else:
                            new_node = "EMPTY"

                        if "MolecularFormula_" not in new_node:
                            triples.append((short_species, var, new_node))


        with open(os.path.join(self.dataset_path, 'ontospecies_value_dict.json'), 'w') as f:
            f.write(json.dumps(value_dict))
            f.close()
        df = pd.DataFrame(triples)
        df = df.drop_duplicates()
        df = df.reset_index(drop=True)
        df.to_csv(os.path.join(self.dataset_path, 'ontospecies-train.txt'), sep='\t', header=False, index=False)
        df_test = df.sample(frac=0.2)
        df_test.to_csv(os.path.join(self.dataset_path, 'ontospecies-test.txt'), sep='\t', header=False, index=False)




if __name__ == '__main__':
    osr = OntoSpeciesReader()
    # osr.find_all_species()
    # osr.find_all_properties_of_all_species()
    osr.create_triples()

# sample training unit: what is the geometry type of CH4, head CH4, tail: geometry, RotationalSymetry ...
#  TODO: construct the path between species and RotationalSymmetry
