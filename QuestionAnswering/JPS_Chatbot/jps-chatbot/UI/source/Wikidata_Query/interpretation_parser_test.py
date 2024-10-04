from pprint import pprint

from Interpretation_parser import InterpretationParser

ip = InterpretationParser(None)
entities = [{'confidence_entity': 0.9979640530540691,
  'end': 18,
  'entity': 'attribute',
  'extractor': 'CRFEntityExtractor',
  'start': 2,
  'value': 'chemical formula'},
 {'confidence_entity': 0.9999196831244241,
  'end': 29,
  'entity': 'class',
  'extractor': 'CRFEntityExtractor',
  'start': 22,
  'value': 'alkanol'},
 {'confidence_entity': 0.9999886772252127,
  'end': 48,
  'entity': 'attribute',
  'extractor': 'CRFEntityExtractor',
  'start': 35,
  'value': 'heat capacity'},
 {'confidence_entity': 0.9999800615698268,
  'end': 58,
  'entity': 'comparison',
  'extractor': 'CRFEntityExtractor',
  'start': 49,
  'value': 'less than'},
 {'confidence_entity': 0.9999210654370583,
  'end': 61,
  'entity': 'number',
  'extractor': 'CRFEntityExtractor',
  'start': 59,
  'value': '15'}]

rst = ip.fill_in_components('batch_attribute_query', entities)
print('======================================')
pprint(rst)
print('======================================')

entities = [{'confidence_entity': 0.9999900483771906,
  'end': 8,
  'entity': 'attribute',
  'extractor': 'CRFEntityExtractor',
  'start': 0,
  'value': 'geometry'},
 {'confidence_entity': 0.989110556379127,
  'end': 12,
  'entity': 'species',
  'extractor': 'CRFEntityExtractor',
  'start': 9,
  'value': 'ch4'}]

intent = 'item_attribute_query'


rst = ip.fill_in_components(intent, entities)
print('======================================')
pprint(rst)
print('======================================')