package uk.ac.cam.cares.jps.agent.ukdigitaltwin.querystringbuilder;

import java.util.*;

import org.apache.jena.arq.querybuilder.SelectBuilder;
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
	public ArrayList<List<String>> prefixList = new ArrayList<List<String>>();
	public List<String> selectClause = new ArrayList<String>();
	public ArrayList<List<String>> whereClause = new ArrayList<List<String>>();
	
	public String entityName;
	public String entityTypePrefix;
	public String entityType;

	public SPARQLQueryBuilder(ClauseBuilder queryClause, String entityName, String entityTypePrefix, String entityType) {
		this.queryClause = queryClause;	
		if(entityName.contains("?")) {
			this.entityName = entityName;
		} else {
			this.entityName = "?" + entityName;}
		this.entityTypePrefix = entityTypePrefix;
		this.entityType = entityType;
	}
	
	public ArrayList<List<String>> prefixBuilder(){
		
		List<String> prefixContainer = this.queryClause.PrefixAbbrList;
		for(String pre: prefixContainer) {
			String prefixiri = PrefixToUrlMap.getPrefixUrl(pre);
			List<String> prefixPair = Arrays.asList(pre, prefixiri);
			this.prefixList.add(prefixPair);
		}		
		if(!prefixContainer.contains(this.entityTypePrefix)) {
		String entityTypePrefixiri = PrefixToUrlMap.getPrefixUrl(entityTypePrefix);
		List<String> entityTypePrefixPair = new ArrayList<String>();
		if(entityTypePrefixiri!= null) {
		entityTypePrefixPair = Arrays.asList(entityTypePrefix, entityTypePrefixiri);
		}
		this.prefixList.add(entityTypePrefixPair);
		}
		return this.prefixList;
	}
	
	public List<String> selectClauseBuilder(){
		
		List<String> selectClauseContainer = this.queryClause.VariablesList;		
		this.selectClause.add(this.entityName);		
		for(String var: selectClauseContainer) {
			String selectName = "?ValueOf" + var;
			this.selectClause.add(selectName);
		}
		return this.selectClause;
	}
	
	public ArrayList<List<String>> whereClauseBuilder(){
		
		ArrayList<List<String>> whereClauseContainer = this.queryClause.PathArray;
		List<String> entityTypeTriple = Arrays.asList(this.entityName, "a", this.entityTypePrefix + ":" + this.entityType);
		this.whereClause.add(entityTypeTriple);
		
		for(int i = 0; i <= whereClauseContainer.size(); i++) {
			//TODO: how to find varType?
			
			List<String> hasModelVariableTriple = Arrays.asList(this.entityName, "a", this.entityTypePrefix + ":" + this.entityType);
			if(whereClauseContainer.get(i).contains("rdfs:label")) {
				
			}
		}
		
		
		for(String var: selectClauseContainer) {
			String selectName = "?ValueOf" + var;
			this.whereClause.add(selectName);
		}
		return this.whereClause;
	}
	
	
	public static String queryString(String[][] Prefix, String[] Select, String[][] Where) {
		SelectBuilder sb = new SelectBuilder();
		for (String[] i : Prefix) {
			sb.addPrefix(i[0], i[1]);
		}

		for (String[] i : Where) {
			sb.addWhere(i[0], i[1], i[2]);
		}

		if (Select != null) {
			for (String i : Select) {
				sb.addVar(i);
			}
		}

		String queryString = sb.build().toString();
		return queryString;
	}
	
	 public static void main(String[] args) {
		 PowerFlowModelVariable pv = new PowerFlowModelVariable(false, 2);
		 ClauseBuilder cb = new ClauseBuilder("GenCostFuncVariables", pv);
		 SPARQLQueryBuilder sqb= new SPARQLQueryBuilder(cb, "Model_EBus", "OCPMATH", "Submodel");
		 ArrayList<List<String>> pl = sqb.prefixBuilder();
		 for(int i = 0; i < pl.size(); i++) { 
			   List<String> res = pl.get(i);
			   System.out.println(res); 
			   }
	 }

}


