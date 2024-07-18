from wikidata.entity import EntityId
from wikidata.client import Client
import json
import sys
sys.path.append("")
from Marie.Util.location import DATA_DIR, ROOT_DIR

if __name__=="__main__":
    filename = DATA_DIR + "/CrossGraph/wikidata_numerical/wikidata_numerical-train.txt"
    client = Client()
    formula_relation = client.get(EntityId("P274"))
    SUB = str.maketrans("₀₁₂₃₄₅₆₇₈₉", "0123456789")

    species_iri = []
    with open(filename, "r") as infile:
        for line in infile:
            value = line.strip().split("\t")[0]
            if value not in species_iri:
                species_iri.append(value)
    print(len(species_iri))
    species_map = {}
    counter = 0
    for species in species_iri:
        counter += 1
        print(f"{counter} out of {len(species_iri)}")
        entity = client.get(EntityId(species))
        formula = entity[formula_relation] if formula_relation in entity else None
        if formula == None:
            if hasattr(entity, 'label'):
                label = entity.label
            else:
                label = None
            if label is None:
                species_map[species] = species
            else:
                species_map[species] = str(label)
        else:
            species_map[species] = str(formula).translate(SUB)

    with open(ROOT_DIR+"/KGToolbox/Wikidata/wikidata_species_map.json", "w") as outfile:
        json.dump(species_map, outfile)

    print(species_map)



