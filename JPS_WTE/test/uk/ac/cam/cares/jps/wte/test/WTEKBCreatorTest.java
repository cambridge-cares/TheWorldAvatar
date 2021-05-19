package uk.ac.cam.cares.jps.wte.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.wte.FCQuerySource;
import uk.ac.cam.cares.jps.wte.WTEKBCreator;
import uk.ac.cam.cares.jps.wte.WastetoEnergyAgent;

public class WTEKBCreatorTest {
	private String iriofnetwork =null;
	private OntModel model = null;
	private String prefix = null;
	private WTEKBCreator cre = null;
	
	@Before
	public void setUp() {
		iriofnetwork ="http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem";
		model = FCQuerySource.readModelGreedy(iriofnetwork);
		prefix="http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/";
		String scenarioName = "WTEKBCreatorTest";
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
		JPSHttpServlet.enableScenario(scenarioUrl);	
		cre = new WTEKBCreator();
		cre.initOWLClasses(model);
		
	}
	
	/** test doConversionTransport. Should return a string and not error. 
	 * 
	 */
	@Test
	public void testdoConversionTransport() {	
		String mainobjectname="TransportSystem-001";
		String transportiri=cre.doConversionTransport(model,prefix, mainobjectname);
		assertNotNull(transportiri);
		
		List<String[]> resultList =  FCQuerySource.queryResult(model, WastetoEnergyAgent.getTransportQuery());
		System.out.println("size of result="+resultList.size()); 
        assertEquals(1, resultList.size());
        
	}
	
	/** test doConversionFC. Should return a string and not error. 
	 * @throws IOException 
	 * 
	 */
	@Test
	public void testdoConversionFC() throws IOException {	
		String csv = new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/fcdetails.csv");
		List<String[]> readingFromCSV = MatrixConverter.fromCsvToArray(csv);		
		String[]data=readingFromCSV.get(1);
		String filePath=AgentLocator.getPathToWorkingDir(this)+"/wastetemplate.owl";
		FileInputStream inFile = new FileInputStream(filePath);
		Reader in = new InputStreamReader(inFile, "UTF-8");
		OntModel jenaOwlModel2 = ModelFactory.createOntologyModel();
		jenaOwlModel2.read(in, null);
		cre.initOWLClasses(jenaOwlModel2);
		String fcname="FoodCourt-"+String.format("%03d", 1); 
		String irioffc=cre.doConversionFC(jenaOwlModel2,prefix, fcname,data);
		assertNotNull(irioffc);
	
	}
	
	/** test doConversionTransport. Should return a string and not error. 
	 * 
	 */
	@Test
	public void testdoConversionWTF() {	
		String mainobjectname="OffsiteWasteTreatment-3.owl";
		String wtfiri=cre.doConversionWTF(model,prefix, mainobjectname);
		assertNotNull(wtfiri);
        
	}
	
	/** test doConversionFC. Should return a string and not error. 
	 * @throws IOException 
	 * 
	 */
	@Test
	public void testdoConversionOnSite() throws IOException {
		String wtfname="OnSiteWasteTreatment-"+String.format("%03d", 1); ; 
		String filePath=AgentLocator.getPathToWorkingDir(this)+"/wastetemplate.owl";
		FileInputStream inFile = new FileInputStream(filePath);
		Reader in = new InputStreamReader(inFile, "UTF-8");
		OntModel jenaOwlModel = ModelFactory.createOntologyModel();
		jenaOwlModel.read(in, null);
		cre.initOWLClasses(jenaOwlModel);
		String[] inputdata = {"0","103.8400222", "1.367196532"}; //entity's coordinates
		String[] outputdata = {"3"};
		String WTFTechOnsiteQuery = FCQuerySource.getTechQuery() 
				.addWhere("?entity" ,"a", "j1:OnsiteWasteTreatmentFacility")
				.addWhere("?Tech1" ,"a", "j1:OnSiteDigester").buildString();
		List<String[]> propertydata = FCQuerySource.queryResult(model, WTFTechOnsiteQuery);
		String iriofwtf=cre.doConversionOnSite(jenaOwlModel,prefix, wtfname,inputdata,outputdata,propertydata);		
		assertNotNull(iriofwtf);
	
	}
	/** test doConversionWasteSystem. Creates top node of wasteSystem. Should return a string and not error. 
	 * @throws IOException 
	 * startConversion and executeConversion are not used outside of WTEKBCreator, hence they are not tested. 
	 */
	@Test
	public void testdoConversionWasteSystem() throws IOException {
		String filePath=AgentLocator.getPathToWorkingDir(this)+"/wastetemplate.owl";
		FileInputStream inFile = new FileInputStream(filePath);
		Reader in = new InputStreamReader(inFile, "UTF-8");
		String sysname="SingaporeWasteSystem"; 
		OntModel jenaOwlModel = ModelFactory.createOntologyModel();
		jenaOwlModel.read(in, null);
		cre.initOWLClasses(jenaOwlModel);
		String transportiri = "http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/TransportSystem-001.owl#TransportSystem-001.owl";
		List <String> foodcourt= new ArrayList<String>();
		List <String> wtf= new ArrayList<String>();
		foodcourt.add("http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/FoodCourt-001.owl#FoodCourt-001");
		wtf.add("http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/OffsiteWasteTreatment-1.owl#OffsiteWasteTreatment-1");
		cre.doConversionWasteSystem(jenaOwlModel,prefix, sysname,transportiri, foodcourt,wtf);	
		String content = JenaHelper.writeToString(jenaOwlModel);
		assertNotNull(content);	
	}
}
