package uk.ac.cam.cares.jps.dispersion.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.dispersion.sensor.StatisticAnalysisAgent;

public class StatisticAnalysisAgentTest extends TestCase {
	
	public void testPSICalculation() {
		
		double average=40.0;
		String key="OutsidePM25Concentration";
		double psi=new StatisticAnalysisAgent().calculatePSI(average,key);
		System.out.println("psi= "+psi);
	}
	
public void testPSICalculation2() {
		
		double average=279.76;
		String key="OutsideCOConcentration";
		double psi=new StatisticAnalysisAgent().calculatePSI(average,key);
		System.out.println("psi= "+psi);
	}
	
	public void testAgentCall() {
		JSONObject req= new JSONObject();
		req.put("airStationIRI", "http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-001.owl#AirQualityStation-001");
		String resp=AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/StatisticAnalysis", req.toString());
		System.out.println(resp);
	}
	
	public void testDirectCall() {
		JSONObject req= new JSONObject();
		req.put("airStationIRI", "http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-001.owl#AirQualityStation-001");
		String context="http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-001.owl#AirQualityStation-001";
		String dataPath = QueryBroker.getLocalDataPath()+"/statistic";
		new StatisticAnalysisAgent().executeProcess(context, dataPath);
		//System.out.println(resp);
	}
	
	

}
