package uk.ac.cam.cares.jps.agent.ukdigitaltwin.querystringbuilder;

import java.util.*;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.sparql.lang.sparql_11.ParseException;

import uk.ac.cam.cares.jps.agent.ukdigitaltwin.querystringbuilder.ClauseBuilder;

/**
 * SPARQLQueryBuilder developed for constructing any query strings used in UK digital Twin 
 * 
 * @author Wanni Xie (wx243@cam.ac.uk)
 *
 */
public class SPARQLQueryBuilder {
	
	public ClauseBuilder queryClause;
	
	public SPARQLQueryBuilder(ClauseBuilder queryClause) {
		this.queryClause = queryClause;	
	}
	
	/**
	 * This method is used to generate the most common used query string with prefixList, selectClause, whereClause.
	 * distinctFlag and reducedFlag are used to identify the select constrain which can not be both true. 
	 * The limit is set as -1 by default which means no limitation is set to the query result. It is only valid when limit is positive integer.  
	 * The filterClause and optionalClause are optional clauses.
	 * No arguments are passed as it takes the instance of ClauseBuilder
	 */
	public String queryStringBuilder() {
		SelectBuilder sb = new SelectBuilder();
		for (List<String> i : this.queryClause.prefixList) {
			sb.addPrefix(i.get(0), i.get(1));
		}

		for (List<String> i : this.queryClause.whereClause) {
			sb.addWhere(i.get(0), i.get(1), i.get(2));
		}
		
		for (String i : this.queryClause.selectClause) {
			sb.addVar(i);
			}
		
		sb.setDistinct(this.queryClause.distinctFlag);
		sb.setReduced(this.queryClause.reducedFlag);
		
		if(this.queryClause.limit > 0) {
			sb.setLimit(this.queryClause.limit);
		}
		
		if(this.queryClause.filterClause.size() > 0) {
			for (String i : this.queryClause.filterClause) {
				try {
					sb.addFilter(i);
				} catch (ParseException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		
		if(this.queryClause.optionalClause.size() > 0) {
			for (List<String> i : this.queryClause.optionalClause) {
				sb.addOptional(i.get(0), i.get(1), i.get(2));
			}
		}
		
		String queryString = sb.build().toString();
		return queryString;
	}
	
	
	/**
	 * Overload static method do not rely on the ClauseBuilder
	 */
	public static String queryStringBuilder(String[][] Prefix, String[] Select, String[][] Where, String[] filterClause, String[][] optionalClause, 
			boolean distinctFlag, boolean reducedFlag, int limit) {
		
		if((distinctFlag && reducedFlag) || (!distinctFlag && !reducedFlag)) {
			System.out.print("The distinctFlag and reducedFlag cannot be both true or false.");
		    return null;
		    }			
				
		SelectBuilder sb = new SelectBuilder();
		for (String[] i : Prefix) {
			sb.addPrefix(i[0], i[1]);
		}

		for (String[] i : Where) {
			sb.addWhere(i[0], i[1], i[2]);
		}

		if (Select != null) {
			for (String i : Select) {
				sb.addVar(i);}
		}
		
		sb.setDistinct(distinctFlag);
		sb.setReduced(reducedFlag);
		
		if(limit > 0) {
			sb.setLimit(limit);
		}
		
		if(filterClause != null) {
			for (String i : filterClause) {
				try {
					sb.addFilter(i);
				} catch (ParseException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		
		if(optionalClause != null) {
			for (String[] i : optionalClause) {
				sb.addOptional(i[0], i[1], i[2]);
			}
		}

		String queryString = sb.build().toString();
		return queryString;		
	}
	
	 public static void main(String[] args) {
		 
		 PowerFlowModelVariableForQuery pfmv = new PowerFlowModelVariableForQuery(false, 2, "10", "14");	
		 ClauseBuilder pb = new ClauseBuilder(true, false, pfmv.genEntityName, pfmv.PowerFlowModelEntityMap.get(pfmv.genEntityName));
		 List<String> vl = pfmv.PowerFlowModelVariablesMap.get(pfmv.genCostFuncKey);
		 HashMap<String, List<String>> classPre_var = new HashMap<String, List<String>>();
		 classPre_var.put(pfmv.variableTypePrefix, vl);
		 LinkedHashMap<String, List<String>> unlabeledVariable_querySentence = new LinkedHashMap<String, List<String>>();
		 for(int i = 0 ; i < vl.size(); i++) {
			  unlabeledVariable_querySentence.put(vl.get(i), pfmv.queryModelVariableSentence);			  
		  }		
		 pb.prefixClauseBuilder(pfmv.PrefixAbbrList);
		 pb.selectClauseAndWhereClauseBuilderWithoutLabels(pfmv.varNameIdentifier, classPre_var, unlabeledVariable_querySentence);
		 pb.selectClauseAndWhereClauseBuilderWithLabels(pfmv.varNameIdentifier, pfmv.labelMap, pfmv.labelVarCalssNameSpaceMap, pfmv.labeledVariable_querySentence);
		 
//		 pb.filterClause =  Arrays.asList("?PrimaryFuel = OCPMATH:Biomass");
//		 List<String> optionalScentence = Arrays.asList("?Model_EGen", "a", "OCPMATH:Biomass");
//		 System.out.printf("The optionalScentence is ", pb.optionalClause);
//		 pb.optionalClause.add(optionalScentence);
//		 System.out.println(pb.optionalClause); 
		 SPARQLQueryBuilder sqb = new SPARQLQueryBuilder(pb);
		 String querystring = sqb.queryStringBuilder();
		 System.out.println(querystring); 

	 }

}


