class URILookup:
    def __init__(self, ontology):
        self.path = '../../SPARQL_warehouse/wiki_chemical_substance_instances'
        self.class_table = 'all_distinct_classes'
        self.property_table = 'distinct_properties'
        self.instances_table = 'selected_instance'


def load_corpus():
    pass