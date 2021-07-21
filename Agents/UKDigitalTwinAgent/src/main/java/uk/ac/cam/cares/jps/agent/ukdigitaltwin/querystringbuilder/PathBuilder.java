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
public class PathBuilder implements Prefixes{
	
	public ArrayList<List<String>> PathArray = new ArrayList<List<String>>();
	
	public  PathBuilder(String varKey, PowerFlowModelVariable pfmv){		
			
		// check model variable key
		if(!pfmv.PowerFlowModelVariablesMap.containsKey(varKey)) {			
			System.out.print("The variable key is not contained in the PF model variable keys");
			PathArray = null;
		}
		
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
				List<String> path = Arrays.asList(OCPMATH, "hasModelVariable", RDF, "type", OPSMODE, varType, OCPSYST, "hasValue", OCPSYST, "numericalValue");
			    PathArray.add(path);
			}

			for(int i = 0; i<= pfmv.pointsOfPiecewiseOrcostFuncOrder; i++) {
					String paraLabel = pfmv.GenCostFuncParameterLabels[i];
					varList.add(paraLabel);	
					List<String> path = Arrays.asList(OCPMATH, "hasModelVariable", RDF, "type", OPSMODE, paraClassName,
							RDFS, "label", "\"" + paraLabel + "\"", OCPSYST, "hasValue", OCPSYST, "numericalValue");
					PathArray.add(path);					
				}
			PathArray.add(varList);												
		} 
		
		//Case 2: build the query path for busModel, branchModel, and genModel
		if (varKey != pfmv.genCostFuncKey) {
			List<String> varList = pfmv.PowerFlowModelVariablesMap.get(varKey);
			for(String varType: varList) {
			    List<String> path = Arrays.asList(OCPMATH, "hasModelVariable", RDF, "type", OPSMODE, varType, OCPSYST, "hasValue", OCPSYST, "numericalValue");
			    PathArray.add(path);}	
			PathArray.add(varList);	
		} 
	}

	public static ArrayList<List<String>> buildPFOrOPFModelVarValusePredicateEdge(String varKey, boolean piecewiseOrPolynomial, int pointsOfPiecewiseOrcostFuncOrder){		
		// Initialise the PredicateEdge, model variable map
		ArrayList<List<String>> PredicateEdge = new ArrayList<List<String>>();
		PowerFlowModelVariable pfmv = new PowerFlowModelVariable(piecewiseOrPolynomial, pointsOfPiecewiseOrcostFuncOrder);	
		
		// check model variable key
		if(!pfmv.PowerFlowModelVariablesMap.containsKey(varKey)) {			
			System.out.print("The variable key is not contained in the PF model variable keys");
			return null;
		}
		
		// case 1: build the query path for generator CostFunc
		if(varKey == pfmv.genCostFuncKey) {
			String paraClassName;
			if(piecewiseOrPolynomial == true) { // the cost objective function is piecewise
				paraClassName = pfmv.GenCostFuncPara[0];} //The parameter class is genCostcn-1	
			else {
				paraClassName = pfmv.GenCostFuncPara[1]; //The parameter class is genCostcn-2	
				}
			List<String> varList = new ArrayList<String>();
			for(String varType: pfmv.PowerFlowModelVariablesMap.get(varKey)) {
				varList.add(varType);
				List<String> path = Arrays.asList(OCPMATH, "hasModelVariable", RDF, "type", OPSMODE, varType, OCPSYST, "hasValue", OCPSYST, "numericalValue");
				PredicateEdge.add(path);
			}
			for(int i = 0; i<= pointsOfPiecewiseOrcostFuncOrder; i++) {
					String paraLabel = pfmv.GenCostFuncParameterLabels[i];
					varList.add(paraLabel);	
					List<String> path = Arrays.asList(OCPMATH, "hasModelVariable", RDF, "type", OPSMODE, paraClassName,
							RDFS, "label", "\"" + paraLabel + "\"", OCPSYST, "hasValue", OCPSYST, "numericalValue");
					PredicateEdge.add(path);					
				}
			PredicateEdge.add(varList);												
		} 
		
		//Case 2: build the query path for busModel, branchModel, and genModel
		if (varKey != pfmv.genCostFuncKey) {
			List<String> varList = pfmv.PowerFlowModelVariablesMap.get(varKey);
			for(String varType: varList) {
			    List<String> path = Arrays.asList(OCPMATH, "hasModelVariable", RDF, "type", OPSMODE, varType, OCPSYST, "hasValue", OCPSYST, "numericalValue");
				PredicateEdge.add(path);}	
			PredicateEdge.add(varList);	
		} 
		return PredicateEdge;			
	}
	
	 public static void main(String[] args) {
		
		/*
		 * ArrayList<List<String>> testres =
		 * buildPFOrOPFModelVarValusePredicateEdge("GenCostFuncVariables", false, 2);
		 * for(int i = 0; i < testres.size(); i++) { List<String> res = testres.get(i);
		 * System.out.println(res); }
		 */
		  
		  PowerFlowModelVariable pfmv = new PowerFlowModelVariable(false, 2);	
		  PathBuilder pb = new PathBuilder("GenCostFuncVariables", pfmv);
		  ArrayList<List<String>> pa = pb.PathArray;
		  for(int i = 0; i < pa.size(); i++) { 
			   List<String> res = pa.get(i);
			   System.out.println(res); 
			   }
		  
	 }
}
