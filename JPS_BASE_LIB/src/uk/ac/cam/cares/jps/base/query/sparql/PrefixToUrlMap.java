package uk.ac.cam.cares.jps.base.query.sparql;

import java.util.HashMap;
import java.util.Map;

public class PrefixToUrlMap implements Prefixes {

	private static PrefixToUrlMap instance;
	private Map<String, String> mapPrefixToUrl = new HashMap<String, String>();
	
	private PrefixToUrlMap() {
	}
	
	public static synchronized PrefixToUrlMap getInstance() {
		if (instance == null) {
			instance = new PrefixToUrlMap();
			instance.init();
		}
		return instance;
	}
	
	private synchronized void init() {
		
		// prefixes for OntoCape
		mapPrefixToUrl.put(OCPBEHA, "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#");
		mapPrefixToUrl.put(OCPGEOM, "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#");
		mapPrefixToUrl.put(OCPMATE, "http://www.theworldavatar.com/ontology/ontocape/material/material.owl#");
		mapPrefixToUrl.put(OCPMATH, "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#");
		mapPrefixToUrl.put(OCPPHAS, "http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#");
		mapPrefixToUrl.put(OCPSPAC, "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#");
		mapPrefixToUrl.put(OCPSYST, "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#");
		mapPrefixToUrl.put(OCPTECH, "http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#");
		mapPrefixToUrl.put(OCPTOPO, "http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#");
		
		// prefixes for OntoPowerSys
		mapPrefixToUrl.put(OPSBEHA, "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#");
		mapPrefixToUrl.put(OPSMODE, "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#");
		mapPrefixToUrl.put(OPSREAL, "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#");
		
		// prefixes for other ontologies
		mapPrefixToUrl.put("OXXLAND", "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#");
	}
	
	public static String getPrefixUrl(String prefix) {
		return getInstance().mapPrefixToUrl.get(prefix);
	}
}