from fuzzywuzzy import fuzz
from attribute_mapping import AttributeMapper


attribute_mapper = AttributeMapper()
print(attribute_mapper.THERMO_ATTRIBUTES)
print(attribute_mapper.find_closest_attribute('query_thermodynamic', 'polarizability'))
print(attribute_mapper.find_closest_attribute('query_thermodynamic', 'dipole moment'))
# larger_score = max([fuzz.ratio(attribute, word) for word in larger_than])
# 1. att name, 1.5  2. att iri name 3. att name 4. att iri name


print(attribute_mapper.find_closest_attribute('query_quantum_chemistry', 'gaussian file'))
print(attribute_mapper.find_closest_attribute('query_quantum_chemistry', 'scf energy'))
print(attribute_mapper.find_closest_attribute('query_quantum_chemistry', 'geometry type'))
print(attribute_mapper.find_closest_attribute('query_quantum_chemistry', 'log file'))

attribute_iri = attribute_mapper.find_closest_attribute('query_quantum_chemistry', 'scf energy')
intent = attribute_mapper.map_to_quantum_queries(attribute_iri)
print(intent)