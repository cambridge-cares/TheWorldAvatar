from Chatbot.SPARQLQuery import SPARQLQuery

query = '''
SELECT ?oLabel ?v ?unitLabel
        WHERE {
        ?o wdt:P31 wd:Q159226 . # class    
        ?o wdt:P2102 ?x . # attribute_2  
        ?o p:P361/psv:P361 ?value . # attribute_1 x 2 
        ?value wikibase:quantityAmount ?v .
        ?value wikibase:quantityUnit ?unit .
        SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
        FILTER(?x > -100 ).  } # comparison numerical_value 
'''

q2 = '''
       SELECT ?oLabel ?v ?unitLabel
        WHERE {
        ?o wdt:P31 wd:Q20054492 . # class    
        ?o wdt:P2101 ?x . # attribute_2  
        ?o p:P2067/psv:P2067 ?value . # attribute_1 x 2 
        ?value wikibase:quantityAmount ?v .
        ?value wikibase:quantityUnit ?unit .
        SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
        FILTER(?x > -100 ).  } # comparison numerical_value  

'''
sq = SPARQLQuery()
sq.query(query)

qs = [query, q2]
sq.process_multiple_queries(qs)
