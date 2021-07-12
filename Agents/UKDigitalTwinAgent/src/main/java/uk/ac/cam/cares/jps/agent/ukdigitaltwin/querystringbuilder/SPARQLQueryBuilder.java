package uk.ac.cam.cares.jps.agent.ukdigitaltwin.querystringbuilder;

import java.awt.List;
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
	
	// private static final String mathematical_model = "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#";
	// private static final String PowerSystemModel = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#";
	private static final String[] Select_BusInfo_UKDT = {"?BusNumbervalue", "?typevalue", "?activepowervalue", "?reactivepowervalue", "?Gsvalue",
			"?Bsvalue", "?areavalue", "?VoltMagvalue", "?VoltAnglevalue", "?BaseKVvalue", "?Zonevalue", "?VMaxvalue", "?VMinvalue"};
	private static final String[] Prefix_BusInfo_UKDT = {"OCPMATH", "OPSMODE", "OCPSYST"};
	private static final String[] a_BusInfo_UKDT = {"?Model_EBus", Prefix_BusInfo_UKDT[0], "Submodel"};
	
	
	 static String mathematical_model = PrefixToUrlMap.getPrefixUrl("OCPMATH");
	 
	 public static void main(String[] args) {
		 System.out.print(mathematical_model);	 
	 }
	 
	private static final String [][] pre = { { "j5", "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#" },
			{ "j3", "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#" },
			{ "j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" } };
	
	public static String[][] prefixBuilder(ArrayList<String> prefixKey){
		String[][] prefixContainer = null;
		Iterator<String> preIter = prefixKey.iterator();
		while(preIter.hasNext()) {
			
		}
		return prefixContainer;
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

}
