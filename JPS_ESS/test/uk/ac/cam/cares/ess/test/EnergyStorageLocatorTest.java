package uk.ac.cam.cares.ess.test;

import java.io.IOException;
import java.util.List;

import org.apache.jena.ontology.OntModel;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.ess.BatteryEntityCreator;

public class EnergyStorageLocatorTest extends TestCase {
	
	
	public static String ENIRI = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
	OntModel model = BatteryEntityCreator.readModelGreedy(ENIRI);
	
	public void testprepareSelectedBranch() {
		
		List<String[]> SelectedBranch=new BatteryEntityCreator().prepareSelectedBranch(model,0.3);
		System.out.println("size= "+SelectedBranch.size()); //assume size=7
			
	}
	
	public void testcreateOwl() throws IOException {
		
		String resultofbattery="http://www.jparksimulator.com/kb/batterycatalog/VRB.owl#VRB";
		new BatteryEntityCreator().createBatteryOwlFile(model,resultofbattery,0.3);
		
	}
	
	public void testQueryparentclass() {
		String gencoordinate = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
				+ "SELECT ?entity ?class ?parent "
				+ "WHERE {?entity  a  ?class ."
				+ "?entity   j6:hasStateOfCharge ?dt ." 
				+ "?class rdfs:subClassOf ?parent ."
				+ "}";
		 
		
		String batIRI="http://www.jparksimulator.com/kb/batterycatalog/VRB.owl#VRB";
		 String result = new QueryBroker().queryFile(batIRI, gencoordinate);
		 String[] keys = {"entity", "class","parent"};
		 List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		 System.out.println(resultList.size());
		 System.out.println("classtype= "+resultList.get(0)[2]);
	}

}
