package uk.ac.cam.cares.jps.wte.test;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.ontology.OntModel;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.wte.FCQuerySource;

public class FCQuerySourceTest {
	static String iriofnetwork=null;
	//Can't figure out how to test readModelGreedy individually
	@Before
	public void setUp() {
		iriofnetwork ="http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem";
		
	}
	
	@Test
	public void testGetFCQuery() {
		SelectBuilder sb = FCQuerySource.getFCQuery();
		OntModel model = FCQuerySource.readModelGreedy(iriofnetwork);
		List<String[]> foodcourts = FCQuerySource.queryResult( model, sb.build().toString()) ;
		assertEquals(foodcourts.size(), 109);
	}
	
	@Test
	public void testGetOffsiteWasteTreatmentQuery() {
		String query = FCQuerySource.getOffsiteWasteTreatmentQuery();
		OntModel model = FCQuerySource.readModelGreedy(iriofnetwork);
		List<String[]> wTF = FCQuerySource.queryResult( model, query) ;
		assertEquals(wTF.size(), 3);
	}
	
	@Test
	public void testGetOnsiteWasteTreatmentQuery() {
		String query = FCQuerySource.getOnsiteWasteTreatmentQuery();
		OntModel model = FCQuerySource.readModelGreedy(iriofnetwork);
		List<String[]> wTF = FCQuerySource.queryResult( model, query) ;
		assertEquals(wTF.size(), 1); //That particular one which ends in zero
	}
	
	@Test
	public void testGetTechQuery() {
		SelectBuilder sb = FCQuerySource.getTechQuery();
		OntModel model = FCQuerySource.readModelGreedy(iriofnetwork);
		List<String[]> techType = FCQuerySource.queryResult( model, sb.build().toString()) ;
		assertEquals(techType.size(), 5);
		//Why five? Onsite has two types (but one isn't selected in) while offsite has five types
	}
	
}
