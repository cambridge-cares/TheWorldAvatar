package uk.ac.cam.cares.jps.agent.ukdigitaltwin.querystringbuilder;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Iterator;
import java.util.LinkedList;

import uk.ac.cam.cares.jps.base.query.sparql.Paths;
import uk.ac.cam.cares.jps.base.query.sparql.PrefixToUrlMap;

/**
 * PathBuilder developed for constructing the predicates (properties paths) used in building the SPARQL 
 * query string of the UK digital Twin 
 * 
 * @author Wanni Xie (wx243@cam.ac.uk)
 * 
 */
public class PathBuilder implements Paths{
	private int costFuncOrder; 
	
	public static ArrayList<List<String>> buildPFOrOPFModelVarValusePredicateEdge(String varKey, int... costFuncOrder){
		PathBuilder pb = new PathBuilder(costFuncOrder[0]);
		if(!PowerFlowModelVariable.PowerFlowModelVariablesMap.containsKey(varKey)) {
			System.out.print("The variable key is not contained in the PF model variable keys");
			return null;
		}
		
		/*
		 * for (int i = 0; i < chars.length; i++) { System.out.print(chars[i]); }
		 */
// TODO: think about adding labels		
		List<String> var = pb.PowerFlowModelVariablesMap.get(varKey);
	    ArrayList<List<String>> PredicateEdge = new ArrayList<List<String>>();
	    if(varKey == pb.genCostFuncKey && var.contains("lable") && var.contains("genCostcn-2") && costFuncOrder.length == 1) {
	    	for(String v : var) {
	    		if (v=="genCostcn-2") {
	    			for(int i =0; i<costFuncOrder[0]; i++) {
	    				List<String> pe = Arrays.asList(OCPMATH, "hasModelVariable", RDF, "type", OPSMODE, v, RDFS, "label", OCPSYST, "hasValue", OCPSYST, "numericalValue");
						PredicateEdge.add(pe);
	    			}
	    			
	    		}
	    		
	    	}
	    	for(int i = 0; i< var.length - 1; i++) {
				String[] pe = {OCPMATH, "hasModelVariable", RDF, "type", OPSMODE, modelVar[i], OCPSYST, "hasValue", OCPSYST, "numericalValue"};
				PredicateEdge.add(pe);
				}		
	    }
	    
	    
		if(prefix == "OPSMODE") {
			for(int i = 0; i< varKey.length; i++) {
				String[] pe = {OCPMATH, "hasModelVariable", RDF, "type", OPSMODE, modelVar[i], OCPSYST, "hasValue", OCPSYST, "numericalValue"};
				PredicateEdge.add(pe);
				}			
		} else {
			System.out.print("The prefix does not math to the Power Flow model.");
			return null;			
		}
		return PredicateEdge;
	}
	
	public static ArrayList<String[]> buildPFOrOPFModelVarValusePredicateEdge(String varKey, int... costFuncOrder){
		PathBuilder pb = new PathBuilder(costFuncOrder[0]);
		if(!pb.PowerFlowModelVariablesMap.containsKey(varKey)) {
			System.out.print("The variable key is not contained in the PF model variable keys");
			return null;
		}
		
		/*
		 * for (int i = 0; i < chars.length; i++) { System.out.print(chars[i]); }
		 */
// TODO: think about adding labels		
		List<String> var = pb.PowerFlowModelVariablesMap.get(varKey);
	    ArrayList<String[]> PredicateEdge = new ArrayList<String[]>();
	    if(varKey == pb.genCostFuncKey && var.contains("lable") && var.contains("genCostcn-2") && costFuncOrder.length == 1) {
	    	for(String v : var) {
	    		if (v=="genCostcn-2") {
	    			for(int i =0; i<costFuncOrder[0]; i++) {
	    				String[] pe = {OCPMATH, "hasModelVariable", RDF, "type", OPSMODE, v, RDFS, "label", OCPSYST, "hasValue", OCPSYST, "numericalValue"};
						PredicateEdge.add(pe);
	    			}
	    			
	    		}
	    		
	    	}
	    	for(int i = 0; i< var.length - 1; i++) {
				String[] pe = {OCPMATH, "hasModelVariable", RDF, "type", OPSMODE, modelVar[i], OCPSYST, "hasValue", OCPSYST, "numericalValue"};
				PredicateEdge.add(pe);
				}		
	    }
	    
	    
		if(prefix == "OPSMODE") {
			for(int i = 0; i< varKey.length; i++) {
				String[] pe = {OCPMATH, "hasModelVariable", RDF, "type", OPSMODE, modelVar[i], OCPSYST, "hasValue", OCPSYST, "numericalValue"};
				PredicateEdge.add(pe);
				}			
		} else {
			System.out.print("The prefix does not math to the Power Flow model.");
			return null;			
		}
		return PredicateEdge;
	}
	
	public static ArrayList<String[]> buildPFOrOPFModelVarValusePredicateEdge(String prefix, String...modelVar){
	    ArrayList<String[]> PredicateEdge = new ArrayList<String[]>();
		if(prefix == "OPSMODE") {
			for(int i = 0; i< modelVar.length; i++) {
				String[] pe = {OCPMATH, "hasModelVariable", RDF, "type", OPSMODE, modelVar[i], OCPSYST, "hasValue", OCPSYST, "numericalValue"};
				PredicateEdge.add(pe);
				}			
		} else {
			System.out.print("The prefix does not math to the Power Flow model.");
			return null;			
		}
		return PredicateEdge;
	}
	
	 public static void main(String[] args) {
		 ArrayList<String[]> testres = buildPFOrOPFModelVarValusePredicateEdge("OPSMODE", "BusNumber", "Pg");		
		  for(int i = 0; i < testres.size(); i++) {
			  String[] res = testres.get(i);
		  System.out.println(Arrays.toString(res)); }
		 
		 
	 }
}
