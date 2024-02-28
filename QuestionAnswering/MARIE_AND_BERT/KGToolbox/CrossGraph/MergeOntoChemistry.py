import os
from Marie.Util.location import DATA_DIR

ontology_list = ["ontocompchem", "ontospecies"]

# merge the training and test triples from all there ontologies
content_train = ""
content_test = ""
for ontology in ontology_list:
    train_path = os.path.join(DATA_DIR, f'CrossGraph/{ontology}/{ontology}-train.txt')
    test_path = os.path.join(DATA_DIR, f'CrossGraph/{ontology}/{ontology}-test.txt')
    content_train += open(train_path).read()
    content_test += open(test_path).read()

ontology = "ontochemistry"
with open(os.path.join(DATA_DIR, f'CrossGraph/{ontology}/{ontology}-train.txt'), 'w') as f:
    f.write(content_train)
    f.close()

with open(os.path.join(DATA_DIR, f'CrossGraph/{ontology}/{ontology}-test.txt'), 'w') as f:
    f.write(content_test)
    f.close()
