package uk.ac.cam.cares.jps.agent.ukdigitaltwin.querystringbuilder;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

/**
 * PowerFlowModelVariable class stores the model variables of power flow analysis according to
 * the model variables format of MATPOWER.
 * It used to construct the query strings.
 * 
 * @author Wanni Xie (wx243@cam.ac.uk)
 * 
 */

public class PowerFlowModelVariable {
	String busModelKey = "BusModelVariables";
	String branchModelKey = "BranchModelVariables";
	String genModelKey = "GeneratorModelVariables";
	String genCostFuncKey = "GenCostFuncVariables";
	
	List<String> BusModelVariables = Arrays.asList("BusNumber", "BusType", "PdBus", "GdBus", "Gs", "Bs", "Area", 
			"Vm", "Va", "baseKV", "Zone", "VmMax", "VmMin");
	List<String> BranchModelVariables = Arrays.asList("BusFrom", "BusTo", "R", "X", "B", "RateA", "RateB", "RateC", 
			"RatioCoefficient", "Angle", "BranchStatus", "AngleMin", "AngleMax");
	List<String> GeneratorModelVariables = Arrays.asList("BusNumber", "Pg", "Qg", "QMax", "QMin", "Vg", "mBase", "Status",
			"PMax", "PMin", "Pc1", "Pc2", "QC1Min", "QC1Max", "QC2Min", "QC2Max", "Rampagc", "Ramp10", "Ramp30", "Rampq", "APF"); 
	List<String> GenCostFuncVariables = Arrays.asList("CostModel", "StartCost", "StopCost", "genCostn");
	
	String[] GenCostFuncPara = {"genCostcn-1", "genCostcn-2"};
	String[] GenCostFuncParameterLabels = {"Parameter_a", "Parameter_b", "Parameter_c", "Parameter_d", "Parameter_e", "Parameter_f"};

	HashMap<String, List<String>> PowerFlowModelVariablesMap = new HashMap<String, List<String>>();
	
	boolean piecewiseOrPolynomial; // True: piecewise;  False: Polynomial;
	int pointsOfPiecewiseOrcostFuncOrder;
	
	public PowerFlowModelVariable(boolean piecewiseOrPolynomial, int pointsOfPiecewiseOrcostFuncOrder) {
		
		PowerFlowModelVariablesMap.put(busModelKey, BusModelVariables);
		PowerFlowModelVariablesMap.put(branchModelKey, BranchModelVariables);
		PowerFlowModelVariablesMap.put(genModelKey, GeneratorModelVariables);
		PowerFlowModelVariablesMap.put(genCostFuncKey, GenCostFuncVariables);
		
		this.piecewiseOrPolynomial = piecewiseOrPolynomial;
		this.pointsOfPiecewiseOrcostFuncOrder = pointsOfPiecewiseOrcostFuncOrder;		
	}	
}
