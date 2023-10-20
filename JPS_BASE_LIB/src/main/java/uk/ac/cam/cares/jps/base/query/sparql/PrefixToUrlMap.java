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
		
		//prefixes for standard ontologies
		mapPrefixToUrl.put(DBPEDIA_O, "http://dbpedia.org/ontology/");
		mapPrefixToUrl.put(DBPEDIA_P, "http://dbpedia.org/property/");
		mapPrefixToUrl.put(DBPEDIA_R, "http://dbpedia.org/resource/");
		mapPrefixToUrl.put(DC, "http://purl.org/dc/elements/1.1/");
		mapPrefixToUrl.put(DCAM, "http://purl.org/dc/dcam/");
		mapPrefixToUrl.put(DCTERMS, "http://purl.org/dc/terms/");
		mapPrefixToUrl.put(FOAF, "http://xmlns.com/foaf/0.1/");
		mapPrefixToUrl.put(OWL, "http://www.w3.org/2002/07/owl#");
		mapPrefixToUrl.put(RDF, "http://www.w3.org/1999/02/22-rdf-syntax-ns#");
		mapPrefixToUrl.put(RDFS, "http://www.w3.org/2000/01/rdf-schema#");
		mapPrefixToUrl.put(SKOS, "http://www.w3.org/2004/02/skos/core#");
		mapPrefixToUrl.put(TIME, "https://www.w3.org/2006/time#");
		mapPrefixToUrl.put(WIKIDATA, "http://www.wikidata.org/entity/");
		mapPrefixToUrl.put(WIKIDATAT, "http://www.wikidata.org/prop/direct/");
		mapPrefixToUrl.put(XSD, "http://www.w3.org/2001/XMLSchema#");
		
		// prefixes for OntoCape
		mapPrefixToUrl.put(OCPBEHA, "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#");
		mapPrefixToUrl.put(OCPPERF, "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_performance/economic_performance.owl#");
		mapPrefixToUrl.put(OCPGEOM, "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#");
		mapPrefixToUrl.put(OCPMATE, "http://www.theworldavatar.com/ontology/ontocape/material/material.owl#");
		mapPrefixToUrl.put(OCPMATH, "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#");
		mapPrefixToUrl.put(OCPPHAS, "http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#");
		mapPrefixToUrl.put(OCPSPAC, "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#");
		mapPrefixToUrl.put(OCPSYST, "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#");
		mapPrefixToUrl.put(OCPTECH, "http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#");
		mapPrefixToUrl.put(OCPTOPO, "http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#");
		//########the following are added by @wx243
		mapPrefixToUrl.put(OCPDSIU, "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#");
		mapPrefixToUrl.put(OCPST, "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#");
		mapPrefixToUrl.put(OCPNTSYS, "http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#");
		mapPrefixToUrl.put(OCPPHYDMS, "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/physical_dimension/physical_dimension.owl#");
		mapPrefixToUrl.put(OCPCOORSYS, "http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#");
		mapPrefixToUrl.put(OCPSIU, "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#");
		mapPrefixToUrl.put(OCPMATHRLT, "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/mathematical_relation/mathematical_relation.owl#");
		mapPrefixToUrl.put(MATAMDLTOPO, "http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#");
				
		// prefixes for OntoPowerSys
		mapPrefixToUrl.put(OPSBEHA, "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#");
		mapPrefixToUrl.put(OPSMODE, "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#");
		mapPrefixToUrl.put(OPSREAL, "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#");
		//########the following are added by @wx243
		mapPrefixToUrl.put(OPSFUNC, "http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#");
		mapPrefixToUrl.put(OPSPERFM, "http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#");	
		
		// prefixes for other JPS ontologies
		mapPrefixToUrl.put(JPSAGEN, "http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#");
		mapPrefixToUrl.put(JPSLAND, "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#");
		mapPrefixToUrl.put(ONTOCITYGML, "http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#");
		mapPrefixToUrl.put(ONTOSPECIES, "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#");
		
		// special prefixes
		// Blazegraph Geo Service
		mapPrefixToUrl.put(BLAZEGRAPH_GEO, "http://www.bigdata.com/rdf/geospatial#");
		
		// prefixes for OntoEIP
		mapPrefixToUrl.put(OEIPSYSREAL, "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#");
		mapPrefixToUrl.put(OEIPSYSFUNC, "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_function.owl#");
		mapPrefixToUrl.put(OEIPPOWPLANT, "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#");
		mapPrefixToUrl.put(OEIPUPSV1, "http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#");
		mapPrefixToUrl.put(OEISYSPERFM, "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#");
		mapPrefixToUrl.put(OEISYSRQRM, "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_requirement.owl#");
		
		
		
	}
	
	public static String getPrefixUrl(String prefix) {
		return getInstance().mapPrefixToUrl.get(prefix);
	}
	
	public static String getPrefixForSPARQL(String prefix) {
		return "PREFIX " + prefix + ":<" + PrefixToUrlMap.getPrefixUrl(prefix) + ">\r\n";
	}
	
	public static String getPrefixesForSPARQL(String... prefixes) {
		StringBuffer b = new StringBuffer();
		for (String prefix : prefixes) {
			b.append(getPrefixForSPARQL(prefix));
		}
		return b.toString();
	}
}