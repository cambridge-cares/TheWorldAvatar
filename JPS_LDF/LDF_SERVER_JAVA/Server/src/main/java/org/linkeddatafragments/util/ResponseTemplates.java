package org.linkeddatafragments.util;

public class ResponseTemplates {

	public final static String OntokinHandshake =   "{\n"
    		+ "  \"@graph\" : [ {\n"
    		+ "    \"@id\" : \"_:b0\",\n"
    		+ "    \"@type\" : \"http://www.w3.org/2002/07/owl#Restriction\"\n"
    		+ "  }, {\n"
    		+ "    \"@id\" : \"_:b1\",\n"
    		+ "    \"property\" : \"rdf:subject\",\n"
    		+ "    \"variable\" : \"subject\"\n"
    		+ "  }, {\n"
    		+ "    \"@id\" : \"_:b2\",\n"
    		+ "    \"first\" : \"http://www.theworldavatar.com/kb/ontokin/ontokin.owl#Product\",\n"
    		+ "    \"rest\" : {\n"
    		+ "      \"@list\" : [ \"http://www.theworldavatar.com/kb/ontokin/ontokin.owl#Reactant\" ]\n"
    		+ "    }\n"
    		+ "  }, {\n"
    		+ "    \"@id\" : \"_:b4\",\n"
    		+ "    \"property\" : \"rdf:predicate\",\n"
    		+ "    \"variable\" : \"predicate\"\n"
    		+ "  }, \n"
    		+ "  {\n"
    		+ "    \"@id\" : \"_:b10\",\n"
    		+ "    \"property\" : \"rdf:product\",\n"
    		+ "    \"variable\" : \"product\"\n"
    		+ "  },\n"
    		+ "  {\n"
    		+ "    \"@id\" : \"_:b5\",\n"
    		+ "    \"@type\" : \"http://www.w3.org/2002/07/owl#Restriction\",\n"
    		+ "    \"allValuesFrom\" : \"http://www.theworldavatar.com/kb/ontokin/ontokin.owl#Element\",\n"
    		+ "    \"onProperty\" : \"http://www.theworldavatar.com/kb/ontokin/ontokin.owl#indicatesNumberOf\"\n"
    		+ "  }, {\n"
    		+ "    \"@id\" : \"_:b6\",\n"
    		+ "    \"first\" : \"http://www.theworldavatar.com/kb/ontokin/ontokin.owl#FallOffModelCoefficient\",\n"
    		+ "    \"rest\" : \"_:b7\"\n"
    		+ "  }, {\n"
    		+ "    \"@id\" : \"_:b8\",\n"
    		+ "    \"mapping\" : [ \"_:b1\", \"_:b4\", \"_:b9\"],\n"
    		+ "    \"template\" : \"http://localhost:8080/ldfserver/ontokin{?subject,predicate,object}\"\n"
    		+ "  }, {\n"
    		+ "    \"@id\" : \"_:b9\",\n"
    		+ "    \"property\" : \"rdf:object\",\n"
    		+ "    \"variable\" : \"object\"\n"
    		+ "  }, {\n"
    		+ "    \"@id\" : \"http://localhost:8080/ldfserver/ontokin\",\n"
    		+ "    \"@type\" : [ \"hydra:Collection\", \"hydra:PagedCollection\" ],\n"
    		+ "    \"void:triples\" : 22262124,\n"
    		+ "    \"firstPage\" : \"http://localhost:8080/ldfserver/ontokin?page=1\",\n"
    		+ "    \"hydra:itemsPerPage\" : 10000,\n"
    		+ "    \"nextPage\" : \"http://localhost:8080/ldfserver/ontokin?page=2\",\n"
    		+ "    \"hydra:totalItems\" : 22262124\n"
    		+ "  }, {\n"
    		+ "    \"@id\" : \"http://localhost:8080/ldfserver/ontokin#dataset\",\n"
    		+ "    \"@type\" : [ \"void:Dataset\", \"hydra:Collection\" ],\n"
    		+ "    \"subset\" : \"http://localhost:8080/ldfserver/ontokin\",\n"
    		+ "    \"hydra:itemsPerPage\" : {\n"
    		+ "      \"@type\" : \"xsd:long\",\n"
    		+ "      \"@value\" : \"10000\"\n"
    		+ "    },\n"
    		+ "    \"search\" : \"_:b8\"\n"
    		+ "  } ],\n"
    		+ "  \"@context\" : {\n"
    		+ "    \"firstPage\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#firstPage\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"nextPage\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#nextPage\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"triples\" : {\n"
    		+ "      \"@id\" : \"http://rdfs.org/ns/void#triples\",\n"
    		+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#integer\"\n"
    		+ "    },\n"
    		+ "    \"totalItems\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#totalItems\",\n"
    		+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#integer\"\n"
    		+ "    },\n"
    		+ "    \"itemsPerPage\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#itemsPerPage\",\n"
    		+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#integer\"\n"
    		+ "    },\n"
    		+ "    \"variable\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#variable\"\n"
    		+ "    },\n"
    		+ "    \"property\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#property\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"first\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/1999/02/22-rdf-syntax-ns#first\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"rest\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/1999/02/22-rdf-syntax-ns#rest\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"allValuesFrom\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/2002/07/owl#allValuesFrom\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"onProperty\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/2002/07/owl#onProperty\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"search\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#search\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"subset\" : {\n"
    		+ "      \"@id\" : \"http://rdfs.org/ns/void#subset\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"template\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#template\"\n"
    		+ "    },\n"
    		+ "    \"mapping\" : {\n"
    		+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#mapping\",\n"
    		+ "      \"@type\" : \"@id\"\n"
    		+ "    },\n"
    		+ "    \"hydra\" : \"http://www.w3.org/ns/hydra/core#\",\n"
    		+ "    \"dbpedia-owl\" : \"http://dbpedia.org/ontology/\",\n"
    		+ "    \"void\" : \"http://rdfs.org/ns/void#\",\n"
    		+ "    \"rdf\" : \"http://www.w3.org/1999/02/22-rdf-syntax-ns#\",\n"
    		+ "    \"xsd\" : \"http://www.w3.org/2001/XMLSchema#\",\n"
    		+ "    \"rdfs\" : \"http://www.w3.org/2000/01/rdf-schema#\",\n"
    		+ "    \"dbpedia\" : \"http://dbpedia.org/resource/\",\n"
    		+ "    \"dbpprop\" : \"http://dbpedia.org/property/\",\n"
    		+ "    \"foaf\" : \"http://xmlns.com/foaf/0.1/\",\n"
    		+ "    \"dc\" : \"http://purl.org/dc/terms/\"\n"
    		+ "  }\n"
    		+ "}";
	 
	public final static String OntoCompChemHandshake = "{\r\n"
			+ "  \"@graph\" : [ {\r\n"
			+ "    \"@id\" : \"_:b0\",\r\n"
			+ "    \"mapping\" : [ \"_:b1\", \"_:b2\", \"_:b3\" ],\r\n"
			+ "    \"template\" : \"http://localhost:8080/ldfserver/ontocompchem{?subject,predicate,object}\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"_:b1\",\r\n"
			+ "    \"property\" : \"rdf:subject\",\r\n"
			+ "    \"variable\" : \"subject\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"_:b2\",\r\n"
			+ "    \"property\" : \"rdf:predicate\",\r\n"
			+ "    \"variable\" : \"predicate\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"_:b3\",\r\n"
			+ "    \"property\" : \"rdf:object\",\r\n"
			+ "    \"variable\" : \"object\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"http://localhost:8080/ldfserver/ontocompchem\",\r\n"
			+ "    \"@type\" : [ \"hydra:Collection\", \"hydra:PagedCollection\" ],\r\n"
			+ "    \"void:triples\" : 16088466,\r\n"
			+ "    \"firstPage\" : \"http://localhost:8080/ldfserver/ontocompchem?page=1\",\r\n"
			+ "    \"hydra:itemsPerPage\" : 10,\r\n"
			+ "    \"nextPage\" : \"http://localhost:8080/ldfserver/ontocompchem?page=2\",\r\n"
			+ "    \"hydra:totalItems\" : 16088466\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"http://localhost:8080/ldfserver/ontocompchem#dataset\",\r\n"
			+ "    \"@type\" : [ \"void:Dataset\", \"hydra:Collection\" ],\r\n"
			+ "    \"subset\" : \"http://localhost:8080/ldfserver/ontocompchem\",\r\n"
			+ "    \"hydra:itemsPerPage\" : {\r\n"
			+ "      \"@type\" : \"xsd:long\",\r\n"
			+ "      \"@value\" : \"10\"\r\n"
			+ "    },\r\n"
			+ "    \"search\" : \"_:b0\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"http://purl.org/gc/Gaussian-n\",\r\n"
			+ "    \"@type\" : \"http://www.w3.org/2002/07/owl#Class\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"http://purl.org/gc/cm-1\",\r\n"
			+ "    \"@type\" : [ \"http://data.nasa.gov/qudt/owl/qudt#FrequencyUnit\", \"http://www.w3.org/2002/07/owl#NamedIndividual\", \"http://www.w3.org/2002/07/owl#Thing\" ]\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"http://theworldavatar.com/data/ontocompchem/00027432-1ba5-349d-aa94-ca8f8a1b57f6/00027432-1ba5-349d-aa94-ca8f8a1b57f6.g09\",\r\n"
			+ "    \"@type\" : [ \"http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#OutputSource\", \"http://www.w3.org/2002/07/owl#NamedIndividual\", \"http://www.w3.org/2002/07/owl#Thing\" ]\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"http://theworldavatar.com/data/ontocompchem/0002b9e1-a175-3bcf-a0aa-8d9c4d95cc6d/0002b9e1-a175-3bcf-a0aa-8d9c4d95cc6d.g09\",\r\n"
			+ "    \"@type\" : [ \"http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#OutputSource\", \"http://www.w3.org/2002/07/owl#NamedIndividual\", \"http://www.w3.org/2002/07/owl#Thing\" ]\r\n"
			+ "  } ],\r\n"
			+ "  \"@context\" : {\r\n"
			+ "    \"template\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#template\"\r\n"
			+ "    },\r\n"
			+ "    \"mapping\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#mapping\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"variable\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#variable\"\r\n"
			+ "    },\r\n"
			+ "    \"property\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#property\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"firstPage\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#firstPage\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"nextPage\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#nextPage\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"triples\" : {\r\n"
			+ "      \"@id\" : \"http://rdfs.org/ns/void#triples\",\r\n"
			+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#integer\"\r\n"
			+ "    },\r\n"
			+ "    \"totalItems\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#totalItems\",\r\n"
			+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#integer\"\r\n"
			+ "    },\r\n"
			+ "    \"itemsPerPage\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#itemsPerPage\",\r\n"
			+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#integer\"\r\n"
			+ "    },\r\n"
			+ "    \"search\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#search\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"subset\" : {\r\n"
			+ "      \"@id\" : \"http://rdfs.org/ns/void#subset\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"hydra\" : \"http://www.w3.org/ns/hydra/core#\",\r\n"
			+ "    \"dbpedia-owl\" : \"http://dbpedia.org/ontology/\",\r\n"
			+ "    \"void\" : \"http://rdfs.org/ns/void#\",\r\n"
			+ "    \"rdf\" : \"http://www.w3.org/1999/02/22-rdf-syntax-ns#\",\r\n"
			+ "    \"xsd\" : \"http://www.w3.org/2001/XMLSchema#\",\r\n"
			+ "    \"rdfs\" : \"http://www.w3.org/2000/01/rdf-schema#\",\r\n"
			+ "    \"dbpedia\" : \"http://dbpedia.org/resource/\",\r\n"
			+ "    \"dbpprop\" : \"http://dbpedia.org/property/\",\r\n"
			+ "    \"foaf\" : \"http://xmlns.com/foaf/0.1/\",\r\n"
			+ "    \"dc\" : \"http://purl.org/dc/terms/\"\r\n"
			+ "  }\r\n"
			+ "}\r\n"
			+ "";
	
	public final static String Ontokin_last_page = "{\r\n"
			+ "  \"@graph\" : [ {\r\n"
			+ "    \"@id\" : \"_:b0\",\r\n"
			+ "    \"property\" : \"rdf:object\",\r\n"
			+ "    \"variable\" : \"object\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"_:b1\",\r\n"
			+ "    \"property\" : \"rdf:subject\",\r\n"
			+ "    \"variable\" : \"subject\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"_:b2\",\r\n"
			+ "    \"mapping\" : [ \"_:b1\", \"_:b3\", \"_:b0\" ],\r\n"
			+ "    \"template\" : \"http://localhost:8080/ldfserver/ontokin{?subject,predicate,object}\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"_:b3\",\r\n"
			+ "    \"property\" : \"rdf:predicate\",\r\n"
			+ "    \"variable\" : \"predicate\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"http://localhost:8080/ldfserver/ontokin#dataset\",\r\n"
			+ "    \"@type\" : [ \"void:Dataset\", \"hydra:Collection\" ],\r\n"
			+ "    \"subset\" : \"http://localhost:8080/ldfserver/ontokin?predicate=http%3A%2F%2Fwww.theworldavatar.com%2Fkb%2Fontokin%2Fontokin.owl%23hasEquationxRandPx&page=xpage2x\",\r\n"
			+ "    \"itemsPerPage\" : \"10\",\r\n"
			+ "    \"search\" : \"_:b2\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"http://localhost:8080/ldfserver/ontokin?predicate=http%3A%2F%2Fwww.theworldavatar.com%2Fkb%2Fontokin%2Fontokin.owl%23hasEquationxRandPx&page=xpage2x\",\r\n"
			+ "    \"@type\" : [ \"hydra:Collection\", \"hydra:PagedCollection\" ],\r\n"
			+ "    \"void:triples\" : 330746,\r\n"
			+ "    \"firstPage\" : \"http://localhost:8080/ldfserver/ontokin?predicate=http%3A%2F%2Fwww.theworldavatar.com%2Fkb%2Fontokin%2Fontokin.owl%23hasEquationxRandPx&page=1\",\r\n"
			+ "    \"hydra:itemsPerPage\" : 10,\r\n"
			+ "    \"previousPage\" : \"http://localhost:8080/ldfserver/ontokin?predicate=http%3A%2F%2Fwww.theworldavatar.com%2Fkb%2Fontokin%2Fontokin.owl%23hasEquationxRandPx&page=xpage1x\",\r\n"
			+ "    \"hydra:totalItems\" : 330746\r\n"
			+ "  }85564251 ],\r\n"
			+ "  \"@context\" : {\r\n"
			+ "    \"hasEquation\" : {\r\n"
			+ "      \"@id\" : \"http://www.theworldavatar.com/kb/ontokin/ontokin.owl#hasEquation\"\r\n"
			+ "    },\r\n"
			+ "    \"variable\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#variable\"\r\n"
			+ "    },\r\n"
			+ "    \"property\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#property\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"search\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#search\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"subset\" : {\r\n"
			+ "      \"@id\" : \"http://rdfs.org/ns/void#subset\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"itemsPerPage\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#itemsPerPage\",\r\n"
			+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n"
			+ "    },\r\n"
			+ "    \"template\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#template\"\r\n"
			+ "    },\r\n"
			+ "    \"mapping\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#mapping\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"firstPage\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#firstPage\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"previousPage\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#previousPage\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"nextPage\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#nextPage\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"triples\" : {\r\n"
			+ "      \"@id\" : \"http://rdfs.org/ns/void#triples\",\r\n"
			+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#integer\"\r\n"
			+ "    },\r\n"
			+ "    \"totalItems\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#totalItems\",\r\n"
			+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#integer\"\r\n"
			+ "    },\r\n"
			+ "    \"hydra\" : \"http://www.w3.org/ns/hydra/core#\",\r\n"
			+ "    \"dbpedia-owl\" : \"http://dbpedia.org/ontology/\",\r\n"
			+ "    \"void\" : \"http://rdfs.org/ns/void#\",\r\n"
			+ "    \"rdf\" : \"http://www.w3.org/1999/02/22-rdf-syntax-ns#\",\r\n"
			+ "    \"xsd\" : \"http://www.w3.org/2001/XMLSchema#\",\r\n"
			+ "    \"rdfs\" : \"http://www.w3.org/2000/01/rdf-schema#\",\r\n"
			+ "    \"dbpedia\" : \"http://dbpedia.org/resource/\",\r\n"
			+ "    \"dbpprop\" : \"http://dbpedia.org/property/\",\r\n"
			+ "    \"foaf\" : \"http://xmlns.com/foaf/0.1/\",\r\n"
			+ "    \"dc\" : \"http://purl.org/dc/terms/\"\r\n"
			+ "  }\r\n"
			+ "}\r\n"
			+ "";

	public final static String Ontokin_next_page = "{\r\n"
			+ "  \"@graph\" : [ {\r\n"
			+ "    \"@id\" : \"_:b0\",\r\n"
			+ "    \"property\" : \"rdf:predicate\",\r\n"
			+ "    \"variable\" : \"predicate\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"_:b1\",\r\n"
			+ "    \"property\" : \"rdf:object\",\r\n"
			+ "    \"variable\" : \"object\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"_:b2\",\r\n"
			+ "    \"property\" : \"rdf:subject\",\r\n"
			+ "    \"variable\" : \"subject\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"_:b3\",\r\n"
			+ "    \"mapping\" : [ \"_:b2\", \"_:b0\", \"_:b1\" ],\r\n"
			+ "    \"template\" : \"http://localhost:8080/ldfserver/ontokin{?subject,predicate,object}\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"http://localhost:8080/ldfserver/ontokin#dataset\",\r\n"
			+ "    \"@type\" : [ \"void:Dataset\", \"hydra:Collection\" ],\r\n"
			+ "    \"subset\" : \"http://localhost:8080/ldfserver/ontokin?predicate=http%3A%2F%2Fwww.theworldavatar.com%2Fkb%2Fontokin%2Fontokin.owl%23hasEquationxRandPx&page=xpage2x\",\r\n"
			+ "    \"hydra:itemsPerPage\" : {\r\n"
			+ "      \"@type\" : \"xsd:long\",\r\n"
			+ "      \"@value\" : \"10000\"\r\n"
			+ "    },\r\n"
			+ "    \"search\" : \"_:b3\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"http://localhost:8080/ldfserver/ontokin?predicate=http%3A%2F%2Fwww.theworldavatar.com%2Fkb%2Fontokin%2Fontokin.owl%23hasEquationxRandPx&page=xpage2x\",\r\n"
			+ "    \"@type\" : [ \"hydra:Collection\", \"hydra:PagedCollection\" ],\r\n"
			+ "    \"void:triples\" : 330746,\r\n"
			+ "    \"firstPage\" : \"http://localhost:8080/ldfserver/ontokin?predicate=http%3A%2F%2Fwww.theworldavatar.com%2Fkb%2Fontokin%2Fontokin.owl%23hasEquationxRandPx&page=1\",\r\n"
			+ "    \"hydra:itemsPerPage\" : 10000,\r\n"
			+ "    \"nextPage\" : \"http://localhost:8080/ldfserver/ontokin?predicate=http%3A%2F%2Fwww.theworldavatar.com%2Fkb%2Fontokin%2Fontokin.owl%23hasEquationxRandPx&page=xpage3x\",\r\n"
			+ "    \"previousPage\" : \"http://localhost:8080/ldfserver/ontokin?predicate=http%3A%2F%2Fwww.theworldavatar.com%2Fkb%2Fontokin%2Fontokin.owl%23hasEquationxRandPx&page=xpage1x\",\r\n"
			+ "    \"hydra:totalItems\" : 330746\r\n"
			+ "  }85564251 ],\r\n"
			+ "  \"@context\" : {\r\n"
			+ "    \"variable\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#variable\"\r\n"
			+ "    },\r\n"
			+ "    \"property\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#property\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"hasEquation\" : {\r\n"
			+ "      \"@id\" : \"http://www.theworldavatar.com/kb/ontokin/ontokin.owl#hasEquation\"\r\n"
			+ "    },\r\n"
			+ "    \"firstPage\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#firstPage\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"previousPage\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#previousPage\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"nextPage\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#nextPage\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"triples\" : {\r\n"
			+ "      \"@id\" : \"http://rdfs.org/ns/void#triples\",\r\n"
			+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#integer\"\r\n"
			+ "    },\r\n"
			+ "    \"totalItems\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#totalItems\",\r\n"
			+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#integer\"\r\n"
			+ "    },\r\n"
			+ "    \"itemsPerPage\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#itemsPerPage\",\r\n"
			+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#integer\"\r\n"
			+ "    },\r\n"
			+ "    \"template\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#template\"\r\n"
			+ "    },\r\n"
			+ "    \"mapping\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#mapping\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"search\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#search\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"subset\" : {\r\n"
			+ "      \"@id\" : \"http://rdfs.org/ns/void#subset\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"hydra\" : \"http://www.w3.org/ns/hydra/core#\",\r\n"
			+ "    \"dbpedia-owl\" : \"http://dbpedia.org/ontology/\",\r\n"
			+ "    \"void\" : \"http://rdfs.org/ns/void#\",\r\n"
			+ "    \"rdf\" : \"http://www.w3.org/1999/02/22-rdf-syntax-ns#\",\r\n"
			+ "    \"xsd\" : \"http://www.w3.org/2001/XMLSchema#\",\r\n"
			+ "    \"rdfs\" : \"http://www.w3.org/2000/01/rdf-schema#\",\r\n"
			+ "    \"dbpedia\" : \"http://dbpedia.org/resource/\",\r\n"
			+ "    \"dbpprop\" : \"http://dbpedia.org/property/\",\r\n"
			+ "    \"foaf\" : \"http://xmlns.com/foaf/0.1/\",\r\n"
			+ "    \"dc\" : \"http://purl.org/dc/terms/\"\r\n"
			+ "  }\r\n"
			+ "}\r\n"
			+ "";
	
	public final static String Ontokin_first_page = "{\r\n"
			+ "  \"@graph\" : [ {\r\n"
			+ "    \"@id\" : \"_:b0\",\r\n"
			+ "    \"mapping\" : [ \"_:b1\", \"_:b2\", \"_:b3\" ],\r\n"
			+ "    \"template\" : \"http://localhost:8080/ldfserver/ontokin{?subject,predicate,object}\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"_:b1\",\r\n"
			+ "    \"property\" : \"rdf:subject\",\r\n"
			+ "    \"variable\" : \"subject\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"_:b2\",\r\n"
			+ "    \"property\" : \"rdf:predicate\",\r\n"
			+ "    \"variable\" : \"predicate\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"_:b3\",\r\n"
			+ "    \"property\" : \"rdf:object\",\r\n"
			+ "    \"variable\" : \"object\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"http://localhost:8080/ldfserver/ontokin#dataset\",\r\n"
			+ "    \"@type\" : [ \"void:Dataset\", \"hydra:Collection\" ],\r\n"
			+ "    \"subset\" : \"http://localhost:8080/ldfserver/ontokin?predicate=http%3A%2F%2Fwww.theworldavatar.com%2Fkb%2Fontokin%2Fontokin.owl%23hasEquationxRandPx\",\r\n"
			+ "    \"hydra:itemsPerPage\" : {\r\n"
			+ "      \"@type\" : \"xsd:long\",\r\n"
			+ "      \"@value\" : \"10000\"\r\n"
			+ "    },\r\n"
			+ "    \"search\" : \"_:b0\"\r\n"
			+ "  }, {\r\n"
			+ "    \"@id\" : \"http://localhost:8080/ldfserver/ontokin?predicate=http%3A%2F%2Fwww.theworldavatar.com%2Fkb%2Fontokin%2Fontokin.owl%23hasEquationxRandPx\",\r\n"
			+ "    \"@type\" : [ \"hydra:Collection\", \"hydra:PagedCollection\" ],\r\n"
			+ "    \"void:triples\" : 330746,\r\n"
			+ "    \"firstPage\" : \"http://localhost:8080/ldfserver/ontokin?predicate=http%3A%2F%2Fwww.theworldavatar.com%2Fkb%2Fontokin%2Fontokin.owl%23hasEquationxRandPx&page=1\",\r\n"
			+ "    \"hydra:itemsPerPage\" : 10000,\r\n"
			+ "    \"nextPage\" : \"http://localhost:8080/ldfserver/ontokin?predicate=http%3A%2F%2Fwww.theworldavatar.com%2Fkb%2Fontokin%2Fontokin.owl%23hasEquationxRandPx&page=2\",\r\n"
			+ "    \"hydra:totalItems\" : 330746\r\n"
			+ "  } 85564251 ],\r\n"
			+ "  \"@context\" : {\r\n"
			+ "    \"template\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#template\"\r\n"
			+ "    },\r\n"
			+ "    \"mapping\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#mapping\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"variable\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#variable\"\r\n"
			+ "    },\r\n"
			+ "    \"property\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#property\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"hasEquation\" : {\r\n"
			+ "      \"@id\" : \"http://www.theworldavatar.com/kb/ontokin/ontokin.owl#hasEquation\"\r\n"
			+ "    },\r\n"
			+ "    \"firstPage\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#firstPage\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"nextPage\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#nextPage\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"triples\" : {\r\n"
			+ "      \"@id\" : \"http://rdfs.org/ns/void#triples\",\r\n"
			+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#integer\"\r\n"
			+ "    },\r\n"
			+ "    \"totalItems\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#totalItems\",\r\n"
			+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#integer\"\r\n"
			+ "    },\r\n"
			+ "    \"itemsPerPage\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#itemsPerPage\",\r\n"
			+ "      \"@type\" : \"http://www.w3.org/2001/XMLSchema#integer\"\r\n"
			+ "    },\r\n"
			+ "    \"search\" : {\r\n"
			+ "      \"@id\" : \"http://www.w3.org/ns/hydra/core#search\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"subset\" : {\r\n"
			+ "      \"@id\" : \"http://rdfs.org/ns/void#subset\",\r\n"
			+ "      \"@type\" : \"@id\"\r\n"
			+ "    },\r\n"
			+ "    \"hydra\" : \"http://www.w3.org/ns/hydra/core#\",\r\n"
			+ "    \"dbpedia-owl\" : \"http://dbpedia.org/ontology/\",\r\n"
			+ "    \"void\" : \"http://rdfs.org/ns/void#\",\r\n"
			+ "    \"rdf\" : \"http://www.w3.org/1999/02/22-rdf-syntax-ns#\",\r\n"
			+ "    \"xsd\" : \"http://www.w3.org/2001/XMLSchema#\",\r\n"
			+ "    \"rdfs\" : \"http://www.w3.org/2000/01/rdf-schema#\",\r\n"
			+ "    \"dbpedia\" : \"http://dbpedia.org/resource/\",\r\n"
			+ "    \"dbpprop\" : \"http://dbpedia.org/property/\",\r\n"
			+ "    \"foaf\" : \"http://xmlns.com/foaf/0.1/\",\r\n"
			+ "    \"dc\" : \"http://purl.org/dc/terms/\"\r\n"
			+ "  }\r\n"
			+ "}\r\n"
			+ "";
	
	 
	
}
