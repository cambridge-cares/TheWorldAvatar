package org.linkeddatafragments.fragments;

import java.io.ByteArrayInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.json.JSONArray;
import org.json.JSONObject;

import dk.brics.automaton.Automaton;
import dk.brics.automaton.RegExp;

public class SPARQLRegex {
	
	public static String head_first_page = "{\r\n"
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
	
	
		
		public static String next_page = "{\r\n"
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
	
		
		public static String last_page = "{\r\n"
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
			
			
			/*public static void main(String[] args) throws IOException {
		// TODO Auto-generated method stub
		
		Path fileName = Path.of("D:\\JPS_On_GitHub\\TheWorldAvatar\\JPS_LDF\\CACHE_SERVER\\Result");
        String content = Files.readString(fileName);
		long startTime = System.nanoTime();
		String[] rs = {"C2H5O","O2"};
		String[] ps = {"IC3H7O2"};
		filter(content, rs, ps);
		long stopTime = System.nanoTime();
		System.out.println("Time taken: " + (stopTime - startTime)/1000/1000);

	}*/

	
	public static String filter(String pageNumber, String content, JSONArray reactants, JSONArray products) throws IOException {
		
		String previousPage = String.valueOf(Integer.valueOf(pageNumber) - 1);
		String nextPage = 	  String.valueOf(Integer.valueOf(pageNumber) + 1);
		
		int totalPage =   (int) Math.ceil((double)(330746 / ILinkedDataFragmentRequest.TRIPLESPERPAGE)); 
		
		String lastPage = String.valueOf(totalPage);
		String productReactantString = ""; // &products=&reactants=H2&reactants=O2 xRandPx
		
		
		
		
		System.out.println("========================================== last page =============================");
		System.out.println("calculated last page:" + lastPage);
		System.out.println("received page number:" + pageNumber); 
		
		
		long startTime = System.nanoTime();
        content = content.split("\"@context\"")[0].replace("  } ], ", "").replace("} ],", "");
        String[] array_triples = content.split("}, \\{");
        
        List<String> triples = new ArrayList<>(Arrays.asList(array_triples));

        
        // ========================== templates ============================
        String reactant_template = ".* %s .*=\\].*|.*\\\"%s .*=\\].*";
        String product_template =  ".*=] %s .*|.*=].* %s\\\".*";
        // =================================================================
        
        System.out.println("=================== going through the filter ==================");
        System.out.println("reactants:\n" + reactants.toString());
        System.out.println("products:\n" + products.toString());
        
        String itemsPerPage = String.valueOf(ILinkedDataFragmentRequest.TRIPLESPERPAGE);
        
        List<Automaton> autos = new ArrayList<Automaton>(); 
        String reactantsInURL = "";
        for (String reactant: new TreeSet<String>(Arrays.asList(toStringArray(reactants)))) {
        	
         	if (!(reactant.contentEquals("")||reactant.contentEquals("placeholder"))) {
        		reactantsInURL = reactantsInURL + "&reactants=" + reactant ; 
        		System.out.println("reactantsInURL:" + reactantsInURL);
         		 
         		
	        	RegExp rr = new RegExp(String.format(reactant_template, reactant, reactant));
	        	autos.add(rr.toAutomaton());
	        	System.out.println("======= adding regex for reactant =========");
	        	System.out.println(rr.toString());
        	}else {
         		if(toStringArray(reactants).length==1) {
             		reactantsInURL = "&reactants=placeholder"; // this means the reactants array is empty. put a placeholder in it. placeholder
         		}
        	}
        }
        String productsInURL = "";

        for (String product: new TreeSet<String>(Arrays.asList(toStringArray(products)))) {
         	if (!(product.contentEquals("")||product.contentEquals("placeholder"))) {

        		productsInURL = productsInURL + "&products=" + product ; 

            	RegExp pr = new RegExp(String.format(product_template, product, product));
            	autos.add(pr.toAutomaton());
	        	System.out.println("======= adding regex for product =========");
	        	System.out.println(pr.toString());
        	}else {
         		if(toStringArray(products).length==1) {
         			productsInURL = "&products=placeholder";
         		}
        		
        	}
        }
  	  	
        productReactantString = productsInURL + reactantsInURL;
        
        System.out.println("========= productReactantString ==============");
        System.out.println(productReactantString);
        
        long hit_counter = 0;

  	    System.out.println("Length of array: " + triples.size());
  	  	// List<String> valid_triples = new ArrayList<String>();
  	    List<String> candidates = new ArrayList<String>();

        
        for (Automaton a:autos) {

        	for (String t: triples) {
            	boolean f = t.contains("hasEquation") && t.contains("=]");
            	if (f) {
        			if (a.run(t)) {
        				candidates.add(t);
        			}
            	}
        	}   	 
        	
        	System.out.println("============== size of triples =================");
        	System.out.println(triples.size());
        	if (candidates.size() <= 100) {
        		System.out.println(candidates);
        	}
        	
        	System.out.println("============== size of candidates =================");
        	System.out.println(candidates.size());

        	
        	// Collections.copy(valid_triples,candidates);
        	triples = candidates;
        	candidates = new ArrayList<String>();
        }
        
        List<String> valid_triples = triples;

        
//        
//        for (String t : triples) {
//        	boolean f = t.contains("hasEquation") && t.contains("=]");
////        	
//        	if (f) { // it is a target triple 
//        		
//        		boolean flag = true; 
//        		for (Automaton a: autos) {
//        			if (!a.run(t)) {
//        				flag = false;
//        			}
//        		}
//        		
//        		if (flag) {
//        			hit_counter++;
//        			valid_triples.add(t);
//        		}
//        	}
//        }
        
        
      
        
        // System.out.println(String.format(head, body));
        
      //  if (!valid_triples.isEmpty()) {
        	long length = valid_triples.size();

        	long stopTime = System.nanoTime();
            System.out.println("Time taken: " + (stopTime - startTime)/1000/1000 + " ms");
            System.out.println("Hit number  " + valid_triples.size());

       // }
            
     
 
      	 if (!valid_triples.isEmpty()) { // 
      		 if (length == 1) {
           		 itemsPerPage = "1";
      			 String single_content = String.format(", {%s}", valid_triples.get(0));
      			 //return String.format(head_first_page,  itemsPerPage,itemsPerPage, single_content);
         		 
         		 if(!pageNumber.contentEquals("1")) {
         			 
         			if(pageNumber.contentEquals(lastPage)) {
         				System.out.println("======= last page ======");
         				return  last_page.replace("85564251", single_content).replace("xpage1x", previousPage).replace("xpage2x", pageNumber).replace("xpage3x", nextPage).replace("xRandPx",productReactantString);

         			} 
         			else {
          				System.out.println("======= next_page ======");

         				return  next_page.replace("85564251", single_content).replace("xpage1x", previousPage).replace("xpage2x", pageNumber).replace("xpage3x", nextPage).replace("xRandPx",productReactantString);
         			}
         		 }else {
       				System.out.println("======= first page single_content ======");

        			return  head_first_page.replace("85564251", single_content).replace("xRandPx",productReactantString);
         		 }
      			
      		 }
      		 else {
      			 itemsPerPage = String.valueOf(hit_counter);
      			 String multiple_content = String.format(", {%s}", String.join("},{", valid_triples));
      			// return String.format(head_first_page, itemsPerPage,itemsPerPage, multiple_content);
      			 
         		 if(!pageNumber.contentEquals("1")) {
         			 
          			if(pageNumber.contentEquals(lastPage)) {
         				System.out.println("======= last page ======");
         				return  last_page.replace("85564251", multiple_content).replace("xpage1x", previousPage).replace("xpage2x", pageNumber).replace("xpage3x", nextPage).replace("xRandPx",productReactantString);

          			}
          			else { 
          				System.out.println("======= next_page ======");

          				return  next_page.replace("85564251", multiple_content).replace("xpage1x", previousPage).replace("xpage2x", pageNumber).replace("xpage3x", nextPage).replace("xRandPx",productReactantString);
          			} 
         		 }else {
       				System.out.println("======= first page multiple_content ======");
       					
 
       				
        			return  head_first_page.replace("85564251", multiple_content).replace("xRandPx", productReactantString); 
         		 }      			 
      		 }
      	 }
      	 else {
       		 itemsPerPage = "1";
      		 String placeholder = ", {\r\n"
      		 		+ "    \"@id\" : \"http://www.theworldavatar.com/kb/ontokin/USC-Mech_II-IPA.owl#ChemicalReaction_1241014703427071_83\",\r\n"
      		 		+ "    \"hasEquation\" : \"H2 + O2 [=] H + HO2\"\r\n"
      		 		+ "  }";
      		 
      		 if(!pageNumber.contentEquals("1")) {
       			if(pageNumber.contentEquals(lastPage)) {
     				System.out.println("======= last page ======");
     				return  last_page.replace("85564251", placeholder).replace("xpage1x", previousPage).replace("xpage2x", pageNumber).replace("xpage3x", nextPage).replace("xRandPx",productReactantString);

       			}
       			else {
      				System.out.println("======= next_page ======");

     			return  next_page.replace("85564251", placeholder).replace("xpage1x", previousPage).replace("xpage2x", pageNumber).replace("xpage3x", nextPage).replace("xRandPx",productReactantString);
       			}
      		 }else {
  				System.out.println("======= first page placeholder ======");
     			return  head_first_page.replace("85564251", placeholder).replace("xRandPx",productReactantString);
      		 }
      	 }
 	}
	
	public static String[] toStringArray(JSONArray array) {
	    if(array==null)
	        return null;

	    String[] arr=new String[array.length()];
	    for(int i=0; i<arr.length; i++) {
	        arr[i]=array.optString(i);
	    }
	    return arr;
	}
	
 
}
