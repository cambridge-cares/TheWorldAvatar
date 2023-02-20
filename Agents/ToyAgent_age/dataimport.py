from SPARQLWrapper import SPARQLWrapper, CSV, JSON, POST
import uuid
def upload_data():
  for i in range(5):
    name = ['robot_1','robot_2','robot_3','robot_4','robot_5']
    ages = [30, 15, 25, 40, 21]
    birthdays = ['1992-02-19', '2007-08-05', '1997-11-12', '1983-06-22', '2002-04-30']

    NAME = name[i]
    AGE = ages[i]
    BIRTHDAY = birthdays[i]
    
    ppl_id = 'Person_' + str(uuid.uuid4())
    age_id = 'Age_' + str(uuid.uuid4())
    birthday_id = 'Birthday_' + str(uuid.uuid4())

    query = f"""
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX ex: <http://example.org/people#>
    PREFIX age: <http://example.org/age#>
    PREFIX ageid: <http://example.org/ageid#>
    PREFIX bday: <http://example.org/birthday#>
            INSERT DATA {{
            ex:{ppl_id} a ex:Person ;
                ex:hasName '{str(NAME)}';
                ex:hasAge ageid:{age_id};
                ex:hasBirthday bday:{birthday_id} .

            ageid:{age_id} a xsd:integer ;
                xsd:integer {AGE} .

            bday:{birthday_id} a ex:Birthday ;
                bday:hasDate '{str(BIRTHDAY)}'^^xsd:date .
            }}
    """
    print(query)
    LOCAL_KG_SPARQL = "http://localhost:8080/blazegraph/namespace/test/sparql"
    sparql = SPARQLWrapper(LOCAL_KG_SPARQL)
    sparql.setMethod(POST)  # POST query, not GET
    sparql.setQuery(query)
    ret = sparql.query()

upload_data()