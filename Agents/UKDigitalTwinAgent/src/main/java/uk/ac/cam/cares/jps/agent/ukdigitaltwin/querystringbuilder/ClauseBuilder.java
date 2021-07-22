package uk.ac.cam.cares.jps.agent.ukdigitaltwin.querystringbuilder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;

/**
 * PathBuilder developed for constructing the predicates (properties paths) used in building the SPARQL 
 * query string of the UK digital Twin 
 * 
 * @author Wanni Xie (wx243@cam.ac.uk)
 * 
 */
public class ClauseBuilder implements Prefixes{
	
	public ArrayList<List<String>> PathArray = new ArrayList<List<String>>(); // used to construct the query string body
	public List<String> VariablesList = new ArrayList<String>(); // used in constructing the select clause
	public List<String> PrefixAbbrList = new ArrayList<String>(); // prefix abbreviation list
	
	public String entityName;
	public String entityTypePrefix;
	public String entityType;
	
	public  ClauseBuilder(String varKey, PowerFlowModelVariable pfmv, String entityName, String entityTypePrefix, String entityType){		
			
		if(entityName.contains("?")) {
			this.entityName = entityName;
		} else {
			this.entityName = "?" + entityName;}
		this.entityTypePrefix = entityTypePrefix;
		this.entityType = entityType;
		
		// check model variable key
		if(!pfmv.PowerFlowModelVariablesMap.containsKey(varKey)) {			
			System.out.print("The variable key is not contained in the PF model variable keys");
			PathArray = null;
			VariablesList = null;
			PrefixAbbrList = null;
		} else {
		PrefixAbbrList = Arrays.asList(OCPMATH, RDF, OPSMODE, RDFS, OCPSYST, OCPSYST);
		// case 1: build the query path for generator CostFunc
		if(varKey == pfmv.genCostFuncKey) {
			String paraClassName;
			if(pfmv.piecewiseOrPolynomial == true) { // the cost objective function is piecewise
				paraClassName = pfmv.GenCostFuncPara[0];} //The parameter class is genCostcn-1	
			else {
				paraClassName = pfmv.GenCostFuncPara[1]; //The parameter class is genCostcn-2	
				}
			List<String> varList = new ArrayList<String>(); 
			for(String varType: pfmv.PowerFlowModelVariablesMap.get(varKey)) {
				varList.add(varType);	
				List<String> path = Arrays.asList("OCPMATH:hasModelVariable", "rdf:type", "OPSMODE:" + varType, "OCPSYST:hasValue", "OCPSYST:numericalValue");
			    PathArray.add(path);
			}

			for(int i = 0; i<= pfmv.pointsOfPiecewiseOrcostFuncOrder; i++) {
					String paraLabel = pfmv.GenCostFuncParameterLabels[i];
					varList.add(paraLabel);	
					List<String> path = Arrays.asList("OCPMATH:hasModelVariable", "rdf:type", "OPSMODE:" + paraClassName,
							"rdfs:label", "\"" + paraLabel + "\"","OCPSYST:hasValue", "OCPSYST:numericalValue");
					PathArray.add(path);					
				}
			VariablesList = varList;												
		} 
		
		//Case 2: build the query path for busModel, branchModel, and genModel
		if (varKey != pfmv.genCostFuncKey) {
			List<String> varList = pfmv.PowerFlowModelVariablesMap.get(varKey);
			for(String varType: varList) {
			    List<String> path = Arrays.asList(OCPMATH, "hasModelVariable", RDF, "type", OPSMODE, varType, OCPSYST, "hasValue", OCPSYST, "numericalValue");
			    PathArray.add(path);}	
			VariablesList = varList;	
		}
		}
	}
	
	 public static void main(String[] args) {
		  
		  PowerFlowModelVariable pfmv = new PowerFlowModelVariable(false, 2);	
		  ClauseBuilder pb = new ClauseBuilder("GenCostFuncVariables", pfmv);
		  ArrayList<List<String>> pa = pb.PathArray;
		  List<String> var = pb.VariablesList;
		  for(int i = 0; i < pa.size(); i++) { 
			   List<String> res = pa.get(i);
			   System.out.println(res); 
			   }
		  System.out.println(var); 
	 }
}
