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
import org.linkeddatafragments.util.ResponseTemplates;

import dk.brics.automaton.Automaton;
import dk.brics.automaton.RegExp;

public class SPARQLRegex {
	
	public static String ontokin_first_page = ResponseTemplates.Ontokin_first_page;
	public static String ontokin_next_page = ResponseTemplates.Ontokin_next_page;
	public static String last_page = ResponseTemplates.Ontokin_last_page;


	
	public static String filter(String ontology, String pageNumber, String content, JSONArray reactants, JSONArray products) throws IOException {
		 
		int totalPage =   (int) Math.ceil((double)(330746 / ILinkedDataFragmentRequest.TRIPLESPERPAGE)); 
		String lastPage = String.valueOf(totalPage);
		String productReactantString = ""; // &products=&reactants=H2&reactants=O2 xRandPx
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
        	}else {
         		if(toStringArray(products).length==1) {
         			productsInURL = "&products=placeholder";
         		}        		
        	}
        }
        productReactantString = productsInURL + reactantsInURL;
        long hit_counter = 0;
  	    List<String> candidates = new ArrayList<String>();

  	    
  	    if (ontology.contentEquals("ontokin")) {
  	        for (Automaton a:autos) {

  	        	for (String t: triples) {
  	            	boolean f = t.contains("hasEquation") && t.contains("=]");
  	            	if (f) {
  	        			if (a.run(t)) {
  	        				candidates.add(t);
  	        			}
  	            	}
  	        	}   	 
  	  
  	         	triples = candidates;
  	        	candidates = new ArrayList<String>();
  	        }
  	    }else {
  	    	
        	RegExp pr = new RegExp("C8H14");
        	autos.add(pr.toAutomaton());
        	
        	 
  	        for (Automaton a:autos) {

  	        	for (String t: triples) {
  	            	boolean f = t.contains("hasName");
  	            	if (f) {
  	        			if (a.run(t)) {
  	        				candidates.add(t);
  	        			}
  	            	}
  	        	}   	 
  	  
  	         	triples = candidates;
  	        	candidates = new ArrayList<String>();
  	        }
  	    }

        
        List<String> valid_triples = triples;
        
 
        	long length = valid_triples.size();

        	long stopTime = System.nanoTime();
            System.out.println("Time taken: " + (stopTime - startTime)/1000/1000 + " ms");
            System.out.println("Hit number  " + valid_triples.size());

 
      	 if (!valid_triples.isEmpty()) { // 
      		 if (length == 1) {
           		 itemsPerPage = "1";
      			 String single_content = String.format(", {%s}", valid_triples.get(0));
      			 return replaceComponents(lastPage, pageNumber, single_content, productReactantString);
      		 
      		 
      		 }
      		 else {
      			 itemsPerPage = String.valueOf(hit_counter);
      			 String multiple_content = String.format(", {%s}", String.join("},{", valid_triples));
      			 return replaceComponents(lastPage, pageNumber, multiple_content, productReactantString);
      		 }
      	 }
      	 else {
       		 itemsPerPage = "1";
      		 String placeholder = ", {\r\n"
      		 		+ "    \"@id\" : \"Empty\",\r\n"
      		 		+ "    \"hasEquation\" : \"Empty\"\r\n"
      		 		+ "  }";
      		 
  			 return replaceComponents(lastPage, pageNumber, placeholder, productReactantString);
      	 }
 	}
	
	
	public static String replaceComponents(String lastPage, String pageNumber, String content, String productReactantString) {
		String previousPage = String.valueOf(Integer.valueOf(pageNumber) - 1);
		String nextPage = 	  String.valueOf(Integer.valueOf(pageNumber) + 1);
		
		 if(!pageNumber.contentEquals("1")) {
 			 
			if(pageNumber.contentEquals(lastPage)) {
				return  last_page.replace("85564251", content).replace("xpage1x", previousPage).replace("xpage2x", pageNumber).replace("xpage3x", nextPage).replace("xRandPx",productReactantString);
			} 
			else {
				return  ontokin_next_page.replace("85564251", content).replace("xpage1x", previousPage).replace("xpage2x", pageNumber).replace("xpage3x", nextPage).replace("xRandPx",productReactantString);
			}
		 }else {
			return  ontokin_first_page.replace("85564251", content).replace("xRandPx",productReactantString);
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
