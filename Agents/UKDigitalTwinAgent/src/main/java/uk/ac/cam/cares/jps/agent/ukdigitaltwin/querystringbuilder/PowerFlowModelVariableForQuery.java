package uk.ac.cam.cares.jps.agent.ukdigitaltwin.querystringbuilder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;

/**
 * PowerFlowModelVariable class stores the model variables of power flow analysis according to
 * the model variables format of MATPOWER.
 * It used to construct the query strings.
 * 
 * @author Wanni Xie (wx243@cam.ac.uk)
 * 
 */

public class PowerFlowModelVariableForQuery implements Prefixes {
	
	public final boolean queryFlag = true;
	
	public final String busEntityName = "Model_EBus";
	public final String branchEntityName = "Model_ELine";
	public final String genEntityName = "Model_EGen";
	
	public final String entityTypeName = "Submodel";
	public final String entityTypePrefix = OCPMATH;
	public final String entityType = entityTypePrefix + ":" + entityTypeName;
	
	public final String busModelKey = "BusModelVariables";
	public final String branchModelKey = "BranchModelVariables";
	public final String genModelKey = "GeneratorModelVariables";
	public final String genCostFuncKey = "GenCostFuncVariables";
	
	public final List<String> PrefixAbbrList = Arrays.asList(OCPMATH, RDF, OPSMODE, RDFS, OCPSYST, OCPSYST);
	
	public final String varNameIdentifier = "?varName_";
	public final List<String> queryModelVariableSentence = Arrays.asList(OCPMATH + ":hasModelVariable", varNameIdentifier, OCPSYST + ":hasValue", "?varValue_", OCPSYST + ":numericalValue", "?ValueOf");
	
	public final List<String> BusModelVariables = Arrays.asList("BusNumber", "BusType", "PdBus", "GdBus", "Gs", "Bs", "Area", 
			"Vm", "Va", "baseKV", "Zone", "VmMax", "VmMin");
	public final List<String> BranchModelVariables = Arrays.asList("BusFrom", "BusTo", "R", "X", "B", "RateA", "RateB", "RateC", 
			"RatioCoefficient", "Angle", "BranchStatus", "AngleMin", "AngleMax");
	public final List<String> GeneratorModelVariables = Arrays.asList("BusNumber", "Pg", "Qg", "QMax", "QMin", "Vg", "mBase", "Status",
			"PMax", "PMin", "Pc1", "Pc2", "QC1Min", "QC1Max", "QC2Min", "QC2Max", "Rampagc", "Ramp10", "Ramp30", "Rampq", "APF"); 
	private final List<String> GenCostFuncVariablesBASE = Arrays.asList("CostModel", "StartCost", "StopCost", "genCostn");
	public final String variableTypePrefix = OPSMODE;
	
	private final String[] GenCostFuncPara = {"genCostcn-1", "genCostcn-2"};
	private final String[] GenCostFuncParameterLabelList = {"Parameter_a", "Parameter_b", "Parameter_c", "Parameter_d", "Parameter_e", "Parameter_f"};
	
	public String GenCostFuncParaName;
	public List<String> GenCostFuncParaLabel = new ArrayList<String>();
	public List<String> GenCostFuncVariables = new ArrayList<String>();

	HashMap<String, List<String>> PowerFlowModelVariablesMap = new HashMap<String, List<String>>();
	HashMap<String, String> PowerFlowModelEntityMap = new HashMap<String, String>();
	HashMap<String, List<String>> labelMap = new HashMap<String, List<String>>();
	
	public PowerFlowModelVariableForQuery(boolean piecewiseOrPolynomial, int pointsOfPiecewiseOrcostFuncOrder) {  // True: piecewise;  False: Polynomial;
		
		if(piecewiseOrPolynomial == true) { // the cost objective function is piecewise
			this.GenCostFuncParaName = GenCostFuncPara[0];} //The parameter class is genCostcn-1	
		else {
			this.GenCostFuncParaName = GenCostFuncPara[1]; //The parameter class is genCostcn-2	
			}
		
		for(int i = 0; i<= pointsOfPiecewiseOrcostFuncOrder; i++) {
			String paraLabel = GenCostFuncParameterLabelList[i];
			this.GenCostFuncParaLabel.add(paraLabel);					
		}
	
		for(String var: GenCostFuncVariablesBASE) {
			this.GenCostFuncVariables.add(var);
		}
		for(String label: this.GenCostFuncParaLabel) {
			this.GenCostFuncVariables.add(label);
		}
		
		PowerFlowModelVariablesMap.put(busModelKey, BusModelVariables);
		PowerFlowModelVariablesMap.put(branchModelKey, BranchModelVariables);
		PowerFlowModelVariablesMap.put(genModelKey, GeneratorModelVariables);
		PowerFlowModelVariablesMap.put(genCostFuncKey, GenCostFuncVariablesBASE);
		
		PowerFlowModelEntityMap.put(busModelKey, busEntityName);
		PowerFlowModelEntityMap.put(branchModelKey, branchEntityName);
		PowerFlowModelEntityMap.put(genModelKey, genEntityName);
		PowerFlowModelEntityMap.put(genCostFuncKey, genEntityName);
		
		labelMap.put(this.GenCostFuncParaName, this.GenCostFuncParaLabel);

	}	
}
