package uk.ac.cam.cares.ess.test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.ontology.OntModel;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.ess.BatteryEntityCreator;
import uk.ac.cam.cares.jps.ess.EnergyStorageSystem;

public class EnergyStorageLocatorTest extends TestCase {
	
	
	public static String ENIRI = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
	OntModel model = EnergyStorageSystem.readModelGreedy(ENIRI);
	
	public void testprepareSelectedBranch() {
		
		List<String[]> SelectedBranch=new BatteryEntityCreator().prepareSelectedBranch(model,0.3);
		System.out.println("size= "+SelectedBranch.size()); //assume size=7
			
	}
	/** test createBatteryOwlFile() of BatteryEntityCreator
	 * 
	 * @throws IOException
	 */
	public void testcreateOwl() throws IOException {
		
		String resultofbattery="http://www.jparksimulator.com/kb/batterycatalog/VRB.owl#VRB";
		new BatteryEntityCreator().createBatteryOwlFile(model,resultofbattery,0.3);
		
	}
	
	public void testQueryparentclass() {
		String branchoutputInfo  = new SelectBuilder().addPrefix("j1","http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#" )
				.addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addPrefix("j3", "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#")
				.addPrefix("j4", "http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#")
				.addPrefix("j5", "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#")				
				.addVar("?entity").addVar("?vplossvalue").addVar("?bus1").addVar("?bus2")
				.addWhere("?entity" ,"a", "j1:UndergroundCable")
				.addWhere("?entity" ,"j2:isModeledBy", "?model")
				.addWhere("?model" ,"j5:hasModelVariable", "?ploss")
				
				.addWhere("?ploss" ,"a", "j3:PLoss")
				.addWhere("?ploss" ,"j2:hasValue", "?vploss")
				.addWhere("?vploss" ,"j2:numericalValue", "?vplossvalue")
				
				.addWhere("?entity" ,"j4:hasInput", "?bus1")
				.addWhere("?entity" ,"j4:hasOutput", "?bus2").buildString();
		
		
		
		List<String[]> newresult= new ArrayList<String[]>();
		List<String[]> resultList= EnergyStorageSystem.queryResult(model,branchoutputInfo  );
		System.out.println(resultList.size());
	}

}
