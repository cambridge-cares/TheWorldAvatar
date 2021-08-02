package uk.ac.cam.cares.jps.agent.ukdigitaltwin.querystringbuilder;

import java.util.*;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.sparql.lang.sparql_11.ParseException;

import uk.ac.cam.cares.jps.base.query.sparql.QueryBuilder;

import uk.ac.cam.cares.jps.base.query.sparql.PrefixToUrlMap;

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
	 * This method is used to generate the most common used query string with prefixList, selectClause, and whereClause
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
		
		if(this.queryClause.filterClause != null) {
			for (String i : this.queryClause.filterClause) {
				try {
					sb.addFilter(i);
				} catch (ParseException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		
		if(this.queryClause.optionalClause != null) {
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
	public static String queryStringBuilder(String[][] Prefix, String[] Select, String[][] Where, boolean distinctFlag, boolean reducedFlag, int limit) {
		
		if((distinctFlag && reducedFlag) || (!distinctFlag && !reducedFlag)) {
			System.out.print("The distinctFlag and reducedFlag cannot be both true or false.");
		    System.exit(0);
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

		String queryString = sb.build().toString();
		return queryString;		
	}
	
	 public static void main(String[] args) {
		 
		 PowerFlowModelVariableForQuery pfmv = new PowerFlowModelVariableForQuery(false, 2);	
		 ClauseBuilder pb = new ClauseBuilder(true, false);
		 List<String> vl = pfmv.PowerFlowModelVariablesMap.get(pfmv.genCostFuncKey);
		 pb.queryClauseBuilder(pfmv.genEntityName, pfmv.entityType, pfmv.PrefixAbbrList, vl, pfmv.variableTypePrefix,
				  pfmv.varNameIdentifier, pfmv.queryModelVariableSentence, pfmv.labelMap);
		 //TODO: debug the adding filterClause function
		 //  pb.filterClause =  Arrays.asList("?PrimaryFuel = power_plant:Biomass");
		 SPARQLQueryBuilder sqb = new SPARQLQueryBuilder(pb);
		 String querystring = sqb.queryStringBuilder();
		 System.out.println(querystring); 

	 }

}


