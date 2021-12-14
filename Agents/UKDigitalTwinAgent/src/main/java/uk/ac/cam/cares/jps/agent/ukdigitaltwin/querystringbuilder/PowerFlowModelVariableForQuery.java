package uk.ac.cam.cares.jps.agent.ukdigitaltwin.querystringbuilder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;

import javax.swing.Spring;

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
	
	// top node identifier and type
	public final String busEntityCollectionName = "Model_EBus_Collection";
	public final String branchEntityCollectionName = "Model_ELine_Collection";
	public final String genEntityCollectionName = "Model_EGen_Collection";
	
	public final String entityCollectionTypePrefix = OCPMATH;
	public final String entityCollectionTypeName = "Submodel";
	public final String entityCollectionType = entityCollectionTypePrefix + ":" + entityCollectionTypeName;
	
	// model entity and type
	public final String busEntityName = "Model_EBus";
	public final String branchEntityName = "Model_ELine";
	public final String genEntityName = "Model_EGen";
	
	public final String entityTypePrefix = OPSMODE;
	public final String busentityTypeName = "ElectricalBusModel";
	public final String branchentityTypeName = "ElectricalBranchModel";
	public final String genentityTypeName = "ElectricalGeneratorModel";
	public final String busentityType = entityTypePrefix + ":" + busentityTypeName;
	public final String branchentityType = entityTypePrefix + ":" + branchentityTypeName;
	public final String genentityType = entityTypePrefix + ":" + genentityTypeName;
	
	// model variables key for the data map
	public final String busModelKey = "BusModelVariables";
	public final String branchModelKey = "BranchModelVariables";
	public final String genModelKey = "GeneratorModelVariables";
	public final String genCostFuncKey = "GenCostFuncVariables";
	
	// prefix list
	public final List<String> PrefixAbbrList = Arrays.asList(OCPMATH, RDF, OPSMODE, RDFS, OCPSYST, OCPSYST);
	
	// query the top node sentence
	public List<String> queryModelTopNodeSentence = Arrays.asList(RDF + ":type", entityCollectionType, RDFS + ":label", "^" + OCPSYST + ":isExclusivelySubsystemOf");
	
	//query Model Variable Sentence
	public final String varNameIdentifier = "?varName_";
	public final List<String> queryModelVariableSentence = Arrays.asList(OCPMATH + ":hasModelVariable", varNameIdentifier, OCPSYST + ":hasValue", "?varValue_", OCPSYST + ":numericalValue", "?ValueOf");
	
	// model top node label (number of bus, number of branches)
	public final String electricityLabel = "UK_Electrical_Grid_";
	public final String busLabel = "_Bus_";
	public final String branchLabel = "_Branch_Model";
	public String UK_Electrical_Grid_Label;
	
	// model variables
	public final List<String> BusModelVariables = Arrays.asList("BusNumber", "BusType", "PdBus", "GdBus", "Gs", "Bs", "Area", 
			"Vm", "Va", "baseKV", "Zone", "VmMax", "VmMin");
	public final List<String> BranchModelVariables = Arrays.asList("BusFrom", "BusTo", "R", "X", "B", "RateA", "RateB", "RateC", 
			"RatioCoefficient", "Angle", "BranchStatus", "AngleMin", "AngleMax");
	public final List<String> GeneratorModelVariables = Arrays.asList("BusNumber", "Pg", "Qg", "QMax", "QMin", "Vg", "mBase", "Status",
			"PMax", "PMin", "Pc1", "Pc2", "QC1Min", "QC1Max", "QC2Min", "QC2Max", "Rampagc", "Ramp10", "Ramp30", "Rampq", "APF"); 
	private final List<String> GenCostFuncVariablesBASE = Arrays.asList("CostModel", "StartCost", "StopCost", "genCostn");
	public final String variableTypePrefix = OPSMODE;
	
	// gen cost parameters
	private final String[] GenCostFuncPara = {"genCostcn-1", "genCostcn-2"};
	private final String[] GenCostFuncParameterLabelList = {"Parameter_a", "Parameter_b", "Parameter_c", "Parameter_d", "Parameter_e", "Parameter_f"};
	
	public String GenCostFuncParaName;
	public List<String> GenCostFuncParaLabel = new ArrayList<String>();
	public List<String> GenCostFuncVariables = new ArrayList<String>();

	HashMap<String, String> PowerFlowModelEntityKeyMap = new HashMap<String, String>();
	HashMap<String, String> PowerFlowModelTopNodeMap = new HashMap<String, String>();
	HashMap<String, List<String>> PowerFlowModelVariablesMap = new HashMap<String, List<String>>();
	HashMap<String, String> PowerFlowModelEntityMap = new HashMap<String, String>();
	LinkedHashMap<String, List<String>> labelMap = new LinkedHashMap<String, List<String>>();
	HashMap<String, String> labelVarCalssNameSpaceMap = new HashMap<String, String>();
	HashMap<String, List<String>> labeledVariable_querySentence = new HashMap<String, List<String>>();
	
	public PowerFlowModelVariableForQuery(boolean piecewiseOrPolynomial, int pointsOfPiecewiseOrcostFuncOrder, String numOfBus, String numOfBranch) {  // True: piecewise;  False: Polynomial;
		
		// initial the GenCostFuncVariables
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
		
		// PowerFlowModelEntityKeyMap.put()
		
		
		PowerFlowModelTopNodeMap.put(busEntityName, busEntityCollectionName);
		PowerFlowModelTopNodeMap.put(branchEntityName, branchEntityCollectionName);
		PowerFlowModelTopNodeMap.put(genEntityName, genEntityCollectionName);
		
		PowerFlowModelVariablesMap.put(busModelKey, BusModelVariables);
		PowerFlowModelVariablesMap.put(branchModelKey, BranchModelVariables);
		PowerFlowModelVariablesMap.put(genModelKey, GeneratorModelVariables);
		PowerFlowModelVariablesMap.put(genCostFuncKey, GenCostFuncVariablesBASE);
		
		// entity name and its related variables, e.g. bus: bus variables
		/*
		 * PowerFlowModelVariablesMap.put(busEntityName, BusModelVariables);
		 * PowerFlowModelVariablesMap.put(branchEntityName, BranchModelVariables);
		 * PowerFlowModelVariablesMap.put(genEntityName, GeneratorModelVariables);
		 * PowerFlowModelVariablesMap.put(genEntityName, GenCostFuncVariables);
		 * //!!!!!!!!!
		 */		
		// entity name and its related type, e.g. bus rdf:type ElectricalBusModel
		PowerFlowModelEntityMap.put(busEntityName, busentityType);
		PowerFlowModelEntityMap.put(branchEntityName, branchentityType);
		PowerFlowModelEntityMap.put(genEntityName, genentityType);
		// PowerFlowModelEntityMap.put(genCostFuncKey, genEntityName);
		
		labelMap.put(this.GenCostFuncParaName, this.GenCostFuncParaLabel);
		labelVarCalssNameSpaceMap.put(this.GenCostFuncParaName, variableTypePrefix);
		labeledVariable_querySentence.put(this.GenCostFuncParaName, queryModelVariableSentence);
		
		// initial the label (version of the model)
		this.UK_Electrical_Grid_Label = this.electricityLabel + numOfBus.replaceAll("\\s+","") + this.busLabel + numOfBranch.replaceAll("\\s+","") + this.branchLabel; 
		
		// initial the top node query sentence
		//queryModelTopNodeSentence.add(3, this.UK_Electrical_Grid_Label);
	}	
	
	 public static void main(String[] args) {
		 PowerFlowModelVariableForQuery pfmv = new PowerFlowModelVariableForQuery(false, 2, "10", "14");
		 System.out.println(pfmv.queryModelTopNodeSentence); 
		 
	 }
}
