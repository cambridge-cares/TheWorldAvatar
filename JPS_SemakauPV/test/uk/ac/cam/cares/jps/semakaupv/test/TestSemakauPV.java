package uk.ac.cam.cares.jps.semakaupv.test;

import org.apache.jena.ontology.OntModel;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.semakaupv.SemakauPV;
import uk.ac.cam.cares.jps.semakaupv.SemakauVisualization;

public class TestSemakauPV extends TestCase {
	private String ENIRI="http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/SemakauElectricalNetwork.owl#SemakauElectricalNetwork";
	private String irioftempS="http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001";
    private String iriofirrS="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001";
    private String iriofwindS="http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001";
    private String pvgeneratorIRI="http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/PV-002.owl#PV-002";
	private String busIRI="http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/EBus-006.owl#EBus-006";
	
	/** test the updateOWLValue() function of SemakauPV
	 *  issues arise if generator "PV-002.owl" and bus "EBus-006.owl"
	 */
	public void testrunMods() {
		SemakauPV a= new SemakauPV();
		OntModel model = SemakauPV.readModelGreedy(ENIRI);
		JSONObject x=a.runMODS(model,iriofirrS);
		JSONObject result=a.updateOWLValue(x,"http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/","PV-002.owl","EBus-006.owl");
		System.out.println("result= "+x.toString());
		assertNotNull(result.get("gen"));
		assertNotNull(result.get("bus"));
	}
	
	/** run test method for Coordination Agent (requires both DES as well as underlying agent of SemakauPV)
	 * 
	 */
	public void testCoordination() {
		JSONObject jo = new JSONObject();
		jo.put("electricalnetwork", ENIRI);
		jo.put("irradiationsensor",iriofirrS);
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_SemakauPV/PeriodicCoordination", jo.toString());
		JSONObject jo1 = new JSONObject(resultStart);
		assertEquals(jo1.get("windspeedsensor"),iriofwindS);
		assertEquals(jo1.get("temperaturesensor"),irioftempS);
	}
	/** test validateInput method for SemakauPV.java
	 * 
	 */
	public void testValidateInputSemakauPV() {
		JSONObject jo = new JSONObject();
		jo.put("electricalnetwork", ENIRI);
		jo.put("irradiationsensor","avd");
		assertFalse(new SemakauPV().validateInput(jo));
		jo.put("irradiationsensor",iriofirrS);
		assertTrue(new SemakauPV().validateInput(jo));
	}
	/** test validateInput method for SemakauVisualization.java
	 * 
	 */
	public void testValidateInputSemakauVisualization() {
		JSONObject jo = new JSONObject();
		jo.put("pvgenerator", pvgeneratorIRI);
		jo.put("ebus",busIRI);
		assertFalse(new SemakauVisualization().validateInput(jo));//false as there is one missing
		jo.put("irradiationsensor",iriofirrS);
		assertTrue(new SemakauVisualization().validateInput(jo));
	}
	/** run test method for data points associated with those four graphs
	 * 
	 */
	public void testvisualizeData() {
		SemakauVisualization a = new SemakauVisualization();
		JSONObject jo = a.graphDataPoints(iriofirrS, pvgeneratorIRI, busIRI);
		assertNotNull(jo.get("VoltAng"));
		assertNotNull(jo.get("reactivePower"));
		assertNotNull(jo.get("activePower"));
		assertNotNull(jo.get("proptime"));
		assertNotNull(jo.get("VoltMag"));
		assertNotNull(jo.get("propVal"));
	}
	/** run test method for visualization data points
	 * 
	 */
	public void testVisualizationAgent() {
		JSONObject jo = new JSONObject();
		jo.put("pvgenerator", pvgeneratorIRI);
		jo.put("ebus",busIRI);
		jo.put("irradiationsensor",iriofirrS);
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_SemakauPV/SemakauVisualization", jo.toString());
		System.out.println(resultStart);
		jo = new JSONObject(resultStart);
		assertNotNull(jo.get("VoltAng"));
		assertNotNull(jo.get("reactivePower"));
		assertNotNull(jo.get("activePower"));
		assertNotNull(jo.get("proptime"));
		assertNotNull(jo.get("VoltMag"));
		assertNotNull(jo.get("propVal"));
	}
}
