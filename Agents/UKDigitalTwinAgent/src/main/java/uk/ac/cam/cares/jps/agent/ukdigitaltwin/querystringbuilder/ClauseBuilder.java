package uk.ac.cam.cares.jps.agent.ukdigitaltwin.querystringbuilder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import uk.ac.cam.cares.jps.agent.ukdigitaltwin.tools.Printer;

import uk.ac.cam.cares.jps.base.query.sparql.PrefixToUrlMap;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;

/**
 * ClauseBuilder developed for constructing the query string clause blocks in building, which can be used in both query and update request.
 * 
 * @author Wanni Xie (wx243@cam.ac.uk)
 * 
 */
public class ClauseBuilder implements Prefixes{

	public String entityName;
	public String entityType;
	
	public ArrayList<List<String>> prefixList = new ArrayList<List<String>>();
	public List<String> selectClause = new ArrayList<String>(); // pattern: queried entity + queried attributes 
	public ArrayList<List<String>> insertClause = new ArrayList<List<String>>();
	public ArrayList<List<String>> whereClause = new ArrayList<List<String>>(); 
	public ArrayList<List<String>> deleteClause = new ArrayList<List<String>>();
	public ArrayList<List<String>> optionalClause = new ArrayList<List<String>>();
	public ArrayList<List<String>> filterClause = new ArrayList<List<String>>();
	
	public boolean queryFlag;
	public boolean updateFlag;
	public boolean distinctFlag = true;
	public boolean reducedFlag = false;
	public int limit = -1;
	
	public  ClauseBuilder(boolean queryFlag, boolean updateFlag) {
		if((queryFlag && updateFlag) || (!queryFlag && !updateFlag)) {
			System.out.print("The queryFlag and updateFlag cannot be both true or false.");
		    System.exit(0);
		}else {
			this.queryFlag = queryFlag;
			this.updateFlag = updateFlag;
		}		
	}
	
	/**
	 * queryClauseBuilder is designed for creating the QUERY clauses
	 * 
	 * @entityName : the name of the query entity.
	 * @entityType : the query entity type, e.g. "j1:Generator".
	 * @PrefixAbbrList : a list contains the abbreviations of Prefix used in the query. The abbreviation pattern follows the PrefixToUrlMap.
	 * @VariablesList : a list contains the query variables used to construct the selectClause. The attributes are queried of the query entity
	 * @variableTypePrefix : the abbreviated Prefix of variables type.
	 * @varNameIdentifier : an identifier used to distinct the object which will be used in the rdf:type triple. 
	 * @querySentence : a sentence pattern collection used to construct the query body (e.g. whereClause).
	 * @labelMap : if label is needed in constructing the query body, a label map need to be specified. The key is the variable whose going to be labeled. 
	 *             The value is the list of label. If no labels are need, it is set to null.
	 *  
	 * All the arguments could be passed from attribute values of an instance of a java class, e.g. PowerFlow which will be queried/updated.
	 */
	//TODO: overload ClauseBuilder with querySentence, insertSentence, deleteSentence....; overload the method for no arguments
	public void queryClauseBuilder(String entityName, String entityType, List<String> PrefixAbbrList, 
			HashMap<String, List<String>> classPrefix_unlabeledVariable, String varNameIdentifier, HashMap<String, List<String>> Variable_querySentence, HashMap<String, List<String>> labeledVariable_labels, HashMap<String, String> labeledVariable_classPrefix ){		
		
		// check query mode: 		
		if(!this.queryFlag){
			System.out.print("The method is for query only.");
		    System.exit(0);}
		
		//check query sentence		
		for(String key: Variable_querySentence.keySet()) {
			int sentenceLen = Variable_querySentence.get(key).size();
			if (sentenceLen % 2 != 0) {
				System.out.print("The lenght of querySentence is " + sentenceLen + " of variable " + key + ".");
				System.out.print("The lenght of querySentence should be even integer and it should be formated in p-o pairs.");
			    System.exit(0);}
		}	
		
		//check the format of entityType
		if(!entityType.contains(":") || entityType.indexOf(":") == 0 || entityType.indexOf(":") == entityType.length() - 1) {
			System.out.print("The entityType should be given in the farmat as nameSpace:className.");
		    System.exit(0);
		}
		
		//check the length of labeledVariable_labels and labeledVariable_classPrefix
		if(labeledVariable_labels!=null && labeledVariable_classPrefix !=null && labeledVariable_labels.size() != labeledVariable_classPrefix.size()) {
			System.out.print("The length of labeledVariable_labels and labeledVariable_classPrefix should be equal.");
		    System.exit(0);
		}
		if((labeledVariable_labels!=null && labeledVariable_classPrefix == null) || (labeledVariable_labels == null && labeledVariable_classPrefix != null)) {
			System.out.print("The labeledVariable_labels and labeledVariable_classPrefix should be both null or both having content.");
		    System.exit(0);
		}
			
		//set up prefixList	
		for(String pre: PrefixAbbrList) {
			String prefixiri = PrefixToUrlMap.getPrefixUrl(pre);
			List<String> prefixPair = Arrays.asList(pre, prefixiri);
			this.prefixList.add(prefixPair);
		}		
		
		// initialise selectClause and whereClause with the query entity
		  //case 1: set the selectClause and whereClause without labels
		if(entityName.indexOf("?") != 0) {
			this.entityName = "?" + entityName;}
		this.entityType = entityType;
		this.selectClause.add(this.entityName);	
		List<String> entityTypeTriple = Arrays.asList(this.entityName, "a", this.entityType);
		this.whereClause.add(entityTypeTriple);
 
		for(String var : Variable_querySentence.keySet()) {//set the selectClause
			List<String> querySentence = Variable_querySentence.get(var);
			String selectName = querySentence.get(querySentence.size() - 1) + var;
			this.selectClause.add(selectName);
			for(int i = -1; i < querySentence.size() - 2; i+=2) {// set up whereClause
				if(i == -1) {
					List<String> spo = Arrays.asList(this.entityName, querySentence.get(i+1), querySentence.get(i+2) + var);
					this.whereClause.add(spo);
				} else {
					List<String> spo = Arrays.asList(querySentence.get(i)+var, querySentence.get(i+1), querySentence.get(i+2) + var);
					this.whereClause.add(spo);					
				}				
			}							
		}
		for(String classPrefix : classPrefix_unlabeledVariable.keySet()) {
			List<String> varInSameClassPrefix = classPrefix_unlabeledVariable.get(classPrefix);
			for(String var : varInSameClassPrefix) {
				List<String> varTypeTriple = Arrays.asList(varNameIdentifier+var, "a", classPrefix + ":" + var);
				this.whereClause.add(varTypeTriple);
			}			
		}		
		
		//case 2: set up selectClause and whereClause with labels
		if(labeledVariable_labels != null) {									
			for(String key: labeledVariable_labels.keySet()) {// key is the variable name needed to be labeled
				List<String> querySentence = Variable_querySentence.get(key);
				for(String label:labeledVariable_labels.get(key)) {
					for(int i = -1; i < querySentence.size()-2; i+=2) {// add labels
						if(i == -1) {
							List<String> spo = Arrays.asList(this.entityName, querySentence.get(i+1), querySentence.get(i+2) + label);
							this.whereClause.add(spo);
						} else {
							List<String> spo = Arrays.asList(querySentence.get(i)+ label, querySentence.get(i+1), querySentence.get(i+2) + label);
							this.whereClause.add(spo);					
						}	
				    }
					List<String> labelTriple = Arrays.asList(varNameIdentifier + label, RDFS + ":label", "\"" + label + "\"");
					this.whereClause.add(labelTriple);
					List<String> varTypeTriple = Arrays.asList(varNameIdentifier + label, "a",  labeledVariable_classPrefix.get(key) + ":" + key);					
					this.whereClause.add(varTypeTriple);
					String selectName = querySentence.get(querySentence.size() - 1) + label;
					this.selectClause.add(selectName);
			     }				
			}			
		}
		}
	
	public void queryClauseBuilder(String entityName, String entityType, List<String> PrefixAbbrList, 
			List<String> VariablesList, String variableTypePrefix, String varNameIdentifier, List<String> querySentence, HashMap<String, List<String>>labelMap ){		
		
		// check query mode: 		
		if(!this.queryFlag){
			System.out.print("The method is for query only.");
		    System.exit(0);}
		
		//check query sentence
		int sentenceLen = querySentence.size();
		if (sentenceLen % 2 != 0) {
			System.out.print("The lenght of querySentence is " + sentenceLen + " .");
			System.out.print("The lenght of querySentence should be even integer and it should be formated in p-o pairs.");
		    System.exit(0);}
		
		//set up prefixList	
		for(String pre: PrefixAbbrList) {
			String prefixiri = PrefixToUrlMap.getPrefixUrl(pre);
			List<String> prefixPair = Arrays.asList(pre, prefixiri);
			this.prefixList.add(prefixPair);
		}		
		
		// initialise selectClause and whereClause with the query entity
		if(!entityName.contains("?")) {
			this.entityName = "?" + entityName;}
		this.entityType = entityType;
		this.selectClause.add(this.entityName);	
		List<String> entityTypeTriple = Arrays.asList(this.entityName, "a", this.entityType);
		this.whereClause.add(entityTypeTriple);
		
		//case 1: set the selectClause and whereClause without labels
		if(labelMap == null) {
		for(String var : VariablesList) {//set the selectClause
			String selectName = querySentence.get(querySentence.size() - 1) + var;
			this.selectClause.add(selectName);
		}				
		for(String var : VariablesList) {// set up whereClause
			for(int i = -1; i < querySentence.size() - 2; i+=2) {
				if(i == -1) {
					List<String> spo = Arrays.asList(this.entityName, querySentence.get(i+1), querySentence.get(i+2) + var);
					this.whereClause.add(spo);
				} else {
					List<String> spo = Arrays.asList(querySentence.get(i)+var, querySentence.get(i+1), querySentence.get(i+2) + var);
					this.whereClause.add(spo);					
				}				
			}
			List<String> varTypeTriple = Arrays.asList(varNameIdentifier+var, "a", variableTypePrefix + ":" + var);
			this.whereClause.add(varTypeTriple);	
		  }
		}		
		
		//case 2: set up selectClause and whereClause with labels
		if(labelMap != null) {
			for(String var : VariablesList) {//set the selectClause
				String selectName = querySentence.get(querySentence.size() - 1) + var;
				this.selectClause.add(selectName);}		
			for(String var : VariablesList) {// set up whereClause
				for(int i = -1; i < querySentence.size() - 2; i+=2) {
					if(i == -1) {
						List<String> spo = Arrays.asList(this.entityName, querySentence.get(i+1), querySentence.get(i+2) + var);
						this.whereClause.add(spo);
					} else {
						List<String> spo = Arrays.asList(querySentence.get(i)+var, querySentence.get(i+1), querySentence.get(i+2) + var);
						this.whereClause.add(spo);					
					}				
				}
				List<String> varTypeTriple = Arrays.asList(varNameIdentifier+var, "a", variableTypePrefix + ":" + var);
				this.whereClause.add(varTypeTriple);	
			  }					
			for(String key: labelMap.keySet()) {// key is the variable name needed to be labeled
				for(String label:labelMap.get(key)) {
					for(int i = -1; i < querySentence.size()-2; i+=2) {// add labels
						if(i == -1) {
							List<String> spo = Arrays.asList(this.entityName, querySentence.get(i+1), querySentence.get(i+2) + label);
							this.whereClause.add(spo);
						} else {
							List<String> spo = Arrays.asList(querySentence.get(i)+ label, querySentence.get(i+1), querySentence.get(i+2) + label);
							this.whereClause.add(spo);					
						}	
				    }
					List<String> labelTriple = Arrays.asList(varNameIdentifier + label, RDFS + ":label", "\"" + label + "\"");
					List<String> varTypeTriple = Arrays.asList(varNameIdentifier + label, "a",  variableTypePrefix + ":" + key);
					this.whereClause.add(labelTriple);
					this.whereClause.add(varTypeTriple);
					String selectName = querySentence.get(querySentence.size() - 1) + label;
					this.selectClause.add(selectName);
			     }				
			}			
		}
		}
	
	 public static void main(String[] args) {
		  
		 PowerFlowModelVariableForQuery pfmv = new PowerFlowModelVariableForQuery(false, 2);	
		  ClauseBuilder pb = new ClauseBuilder(true, false);
		  List<String> vl = pfmv.PowerFlowModelVariablesMap.get(pfmv.genCostFuncKey);
		  pb.queryClauseBuilder(pfmv.genEntityName, pfmv.entityType, pfmv.PrefixAbbrList, vl, pfmv.variableTypePrefix,
				  pfmv.varNameIdentifier, pfmv.queryModelVariableSentence, pfmv.labelMap);
		  ArrayList<List<String>> wc  = pb.whereClause;
		  Printer.printArrayList(wc);
		  ArrayList<List<String>> pl  = pb.prefixList;
		  Printer.printArrayList(pl);
		  System.out.println(pb.selectClause); 
	 }
	 
	 
}
