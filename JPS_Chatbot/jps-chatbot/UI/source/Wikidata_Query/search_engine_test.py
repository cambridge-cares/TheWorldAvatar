from UI.source.Wikidata_Query.SearchEngine import SearchEngine

se = SearchEngine()

entities = {'entities': {'attribute': 'geometry', 'species': 'c=c=c=c'},
            'type': 'item_attribute_query'}

rst = se.parse_entities(entities)
print(rst)