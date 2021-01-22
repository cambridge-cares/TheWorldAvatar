package uk.ac.cam.cares.jps.semakaupv.test;

import org.apache.jena.ontology.OntModel;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.semakaupv.SemakauPV;
import uk.ac.cam.cares.jps.semakaupv.SemakauVisualization;

public class TestSemakauPV extends TestCase {
	String ENIRI="http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/SemakauElectricalNetwork.owl#SemakauElectricalNetwork";
	String irradSensorIRI="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001";
	
	public void testrunMods() {
		SemakauPV a= new SemakauPV();
		OntModel model = SemakauPV.readModelGreedy(ENIRI);
		JSONObject x=a.runMODS(model,irradSensorIRI);
		JSONObject result=a.updateOWLValue(x,"http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/","PV-002.owl","EBus-006.owl");
		
		System.out.println("result= "+x.toString());
		
		System.out.println("it's done");
	}
	
	/** run test method for Coordination Agent (requires both DES as well as underlying agent of SemakauPV)
	 * 
	 */
	public void testCoordination() {
		JSONObject jo = new JSONObject();
		jo.put("electricalnetwork", ENIRI);
		jo.put("irradiationsensor","http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001");
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_SemakauPV/startCoordinationSemakauPV", jo.toString());
	}
	/** run test method for data points associated with those four graphs
	 * 
	 */
	public void testvisualizeData() {
		SemakauVisualization a = new SemakauVisualization();
		String irradiationsensorIRI=
				"http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001";
		String pvgeneratorIRI="http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/PV-002.owl#PV-002";
		String busIRI="http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/EBus-006.owl#EBus-006";
		JSONObject jo = a.graphDataPoints(irradiationsensorIRI, pvgeneratorIRI, busIRI);
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
		jo.put("pvgenerator","http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/PV-002.owl#PV-002");
		jo.put("ebus","http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/EBus-006.owl#EBus-006");
		
		jo.put("irradiationsensor","http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001");
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
