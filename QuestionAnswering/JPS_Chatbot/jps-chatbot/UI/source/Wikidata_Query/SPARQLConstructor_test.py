from SPARQLConstructor import  SPARQLConstructor

sc = SPARQLConstructor()

intent_and_entities = {'intent': 'batch_attribute_query', 'entities': [{'attribute': ['P117']}, {'species': ['Q27117205']}]}



rst =sc.fill_sparql_query(intent_and_entities)
print(rst)