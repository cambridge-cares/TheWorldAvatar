package uk.ac.cam.cares.jps.powsys.retrofit.test;

import java.io.File;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.RDFNode;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.query.sparql.JenaModelWrapper;
import uk.ac.cam.cares.jps.base.query.sparql.Paths;
import uk.ac.cam.cares.jps.base.query.sparql.PrefixToUrlMap;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.base.util.MiscUtil;
import uk.ac.cam.cares.jps.powsys.retrofit.BatteryRetrofit;
import uk.ac.cam.cares.jps.powsys.retrofit.RenewableGeneratorRetrofit;
import uk.ac.cam.cares.jps.powsys.retrofit.RetrofitAgent;

public class TestRetrofitAgent extends TestCase implements Prefixes, Paths {
	private String ENIRI
	= "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
	private JSONArray pvgeniris = new JSONArray()
			.put("http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/PV-001.owl#PV-001");
	private String scenarioName = "testESSTRIAL01";	
	private String baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\testESSTRIAL01";
	private JSONArray batteryiris = new JSONArray()
			.put("http://localhost:8080/jps/kb/f288d618-e936-448b-8582-57cfc1ca827f/sgp/jurongisland/jurongislandpowernetwork/VRB-059.owl#VRB-059")
			.put("http://localhost:8080/jps/kb/f288d618-e936-448b-8582-57cfc1ca827f/sgp/jurongisland/jurongislandpowernetwork/VRB-011.owl#VRB-011.owl");
	private void assertPropertyValue(double expected, String url, String... path) {
		OntModel model = JenaHelper.createModel(url);
		JenaModelWrapper w = new JenaModelWrapper(model, null);
		RDFNode o = w.getPropertyValue(url, path);
		double actual = o.asLiteral().getDouble();
		assertEquals(expected, actual);
	}
	
	private void assertPropertyValue(String expectedUrl, String url, String... path) {
		OntModel model = JenaHelper.createModel(url);
		JenaModelWrapper w = new JenaModelWrapper(model, null);
		RDFNode o = w.getPropertyValue(url, path);
		String actualUrl = o.asResource().getURI();
		assertEquals(expectedUrl, actualUrl);
	}
	
	public void testCompleteOnePowerGenerator() {
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSCoordinateCompleteOnePowerGenerator"); 
		JPSHttpServlet.enableScenario(scenarioUrl);	
		//new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		// copy NPP generator OWL test file into the scenario bucket 
		String source = AgentLocator.getCurrentJpsAppDirectory(this) + "/res" + "/NucGenerator_1_B0.owl";
		File file = new File(source);
		System.out.println("MY EXISTS: " + file.exists() + ", " + file.getAbsolutePath());
		String powerGenerator = KeyValueManager.getServerAddress() +"/jps/kb/bd1c6d1d-f875-4c50-a7e1-cc28919f1fe7/nuclearpowerplants/NucGenerator_1_B0.owl#NucGenerator_1_B0";
		new QueryBroker().put(powerGenerator, file);

		assertPropertyValue(1.270333, powerGenerator, PGISCOORDX);
		
		OntModel model = JenaHelper.createModel(powerGenerator);
		new RetrofitAgent().completePowerGenerator(model, powerGenerator);
		
		assertPropertyValue(1.270333, powerGenerator, PGISCOORDX);
		assertPropertyValue(103.719167, powerGenerator, PGISCOORDY);
		//String[] pathActivePowerGenerated = new String[] {OPSBEHA, "hasActivePowerGenerated", OCPSYST, "hasValue", OCPSYST, "numericalValue"};
		//assertPropertyValue(225.0, powerGenerator, pathActivePowerGenerated);
		String pgIri = PrefixToUrlMap.getPrefixUrl(OPSMODE) + "Pg";
		String[] pathPg = new String[] {OCPSYST, "isModeledBy", OCPMATH, "hasModelVariable", pgIri, OCPSYST, "hasValue", OCPSYST, "numericalValue"};
		assertPropertyValue(225.0, powerGenerator, pathPg);
		
		String[] path = new String[] {OCPSYST, "isSubsystemOf"};
		String expectedPowerPlant = "http://localhost:8080/jps/kb/bd1c6d1d-f875-4c50-a7e1-cc28919f1fe7/nuclearpowerplants/NucPP_1.owl#NucPP_1";
		assertPropertyValue(expectedPowerPlant, powerGenerator, path);
	}
	
	public void testretrofitgen() {
		JSONObject jo = new JSONObject();
		JSONArray value1 = new JSONArray();
		JSONArray value2 = new JSONArray();
		jo.put("electricalnetwork", "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
		//value1.put("http://www.jparksimulator.com/kb/sgp/pvsingaporenetwork/PV1.owl#PV1");
		//value1.put("http://www.jparksimulator.com/kb/sgp/pvsingaporenetwork/EGen-200.owl#EGen-200");
		value1.put("http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/PV-001.owl#PV-001");
		jo.put("RenewableEnergyGenerator", value1);
		jo.put("substitutionalgenerators", value2);
		//AgentCaller.executeGet("JPS_POWSYS/retrofit", jo.toString());
		List<String> RenewableGenerators = MiscUtil.toList(value1);
		new RetrofitAgent().retrofitGenerator(jo.getString("electricalnetwork"), RenewableGenerators);
		//AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/retrofitGenerator", jo.toString());
	}
	/** test validateInput() of RenewableGeneratorRetrofitAgent()
	 * 
	 */
	public void testRenewableGeneratorRetrofitValidateInput() {
		JSONObject jo = new JSONObject();
		jo.put("electricalnetwork",ENIRI);
		jo.put("RenewableEnergyGenerator", pvgeniris);
		assertTrue(new RenewableGeneratorRetrofit().validateInput(jo));
	
	}
	/** test Agent calling of RenewableGeneratorRetrofitAgent
	 * 
	 * @throws JSONException
	 */
	public void testRenewableGeneratorRetrofitAgent() throws JSONException {
		JSONObject jo = new JSONObject();
		jo.put("electricalnetwork", ENIRI);
		jo.put("RenewableEnergyGenerator", pvgeniris);
		String result = new ScenarioClient().call(scenarioName, "http://localhost:8080/JPS_POWSYS/RenewableGenRetrofit", jo.toString());
		File file2 = new File(baseUrl +"\\localhost_8080");
		assertTrue(file2.exists());
		File file3 = new File(baseUrl +"\\www_jparksimulator_com");
		assertTrue(file3.exists());
		System.out.println(result);
	}
	/** test validateInput() of BatteryRetrofitAgent()
	 * 
	 */
	public void testBatteryRetrofitValidateInput() {
		JSONObject jo = new JSONObject();
		jo.put("electricalnetwork",ENIRI);
		jo.put("batterylist", batteryiris);
		assertTrue(new BatteryRetrofit().validateInput(jo));
	
	}
	
}
