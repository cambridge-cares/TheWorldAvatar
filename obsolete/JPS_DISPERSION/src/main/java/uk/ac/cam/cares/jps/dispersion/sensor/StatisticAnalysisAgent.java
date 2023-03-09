package uk.ac.cam.cares.jps.dispersion.sensor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet("/StatisticAnalysis")
public class StatisticAnalysisAgent extends JPSHttpServlet{
	/**
	 * Summary of Statistic Analysis agent:
	 * Calculates statistical properties of pollutants:
	 * mean, max, min and PSI = pollutant standards index
	 * PSI is calculated through a hard coded table and linear interpolation - very bad!
	 * Source of the PSI table:
	 * https://www.haze.gov.sg/docs/default-source/faq/computation-of-the-pollutant-standards-index-(psi).pdf
	 */
	 protected void setLogger() {
	        logger = LoggerFactory.getLogger(StatisticAnalysisAgent.class);
	    }
	    Logger logger = LoggerFactory.getLogger(StatisticAnalysisAgent.class);
	    Map<String, String> mapCO = new HashMap<>();
	    Map<String, String> mapPM25 = new HashMap<>();
	    Map<String, String> mapSO2 = new HashMap<>();
	    Map<String, String> mapO3 = new HashMap<>();
	    Map<String, String> mapNO2 = new HashMap<>(); 
	    Map<String, String> mapPM10 = new HashMap<>();
	    public static final String dataseturl=KeyValueManager.get(IKeys.DATASET_AIRQUALITY_URL);
	    
	    protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
	    	initializeMapPSI();
	    	JSONObject response= new JSONObject();
	    	String context=requestParams.get("airStationIRI").toString();
	    	String dataPath = QueryBroker.getLocalDataPath()+"/statistic";
	    	executeProcess(context, dataPath);
	    	response.put("airStationIRI",context);
	    	return response;
	    }
	    
	    public void executeProcess(String context, String dataPath) {
	    	String[]classname= {"OutsideCO2Concentration","OutsideCOConcentration","OutsideSO2Concentration","OutsideNO2Concentration","OutsideNOxConcentration","OutsideNOConcentration","OutsideO3Concentration","OutsideHCConcentration","OutsidePM1Concentration","OutsidePM10Concentration","OutsidePM25Concentration"};	    	

	    	int psimax=0;
	    	for(String propnameclass:classname) {
	    		System.out.println("loop here");
	    		List<String[]>input=prepareInput(context,propnameclass,dataPath);
	    		if(input.size()>1) {
		    		List<Double> newvalues=executeModel(input);
		    		   
		    		int psi=updateTheEndpoint(context,propnameclass,newvalues);
		    		if(psi>psimax) {
		    			psimax=psi;
		    		}
	    		}
	    	}
	    	
	    	updateOverallPSI(context,psimax);
	    };
	    
		private List<String[]> queryEndPointDataset(String querycontext) {
			String resultfromrdf4j = KnowledgeBaseClient.query(dataseturl, null, querycontext);
			String[] keys = JenaResultSetFormatter.getKeys(resultfromrdf4j);
			List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromrdf4j, keys);
			return listmap;
		}
		
		public void initializeMapPSI(){
			String a="0-50";
			String b="51-100";
			String c="101-200";
			String d="201-300";
			String e="301-400";
			String f="401-500";
			
			mapCO.put(a,"0.0-5.0");
			mapCO.put(b,"5.1-10.0");
			mapCO.put(c,"10.1-17.0");
			mapCO.put(d,"17.1-34.0");
			mapCO.put(e,"34.1-46.0");
			mapCO.put(f,"46.1-57.5");
			
			mapPM25.put(a,"0-12");
			mapPM25.put(b,"13-55");
			mapPM25.put(c,"56-150");
			mapPM25.put(d,"151-250");
			mapPM25.put(e,"251-350");
			mapPM25.put(f,"351-500");
			
			mapSO2.put(a,"0-80");
			mapSO2.put(b,"81-365");
			mapSO2.put(c,"366-800");
			mapSO2.put(d,"801-1600");
			mapSO2.put(e,"1601-2100");
			mapSO2.put(f,"2101-2620");
			
			mapO3.put(a,"0-118");
			mapO3.put(b,"119-157");
			mapO3.put(c,"158-235");
			mapO3.put(d,"236-785");
			mapO3.put(e,"786-980");
			mapO3.put(f,"981-1180");
			
			mapNO2.put(a,"-");
			mapNO2.put(b,"-");
			mapNO2.put(c,"1130");
			mapNO2.put(d,"1131-2260");
			mapNO2.put(e,"2261-3000");
			mapNO2.put(f,"3001-3750");
			
			mapPM10.put(a,"0-50");
			mapPM10.put(b,"51-150");
			mapPM10.put(c,"151-350");
			mapPM10.put(d,"351-420");
			mapPM10.put(e,"421-500");
			mapPM10.put(f,"501-600");
			
			
		}
		
	public double calculatePSI(double average, String key) {
		double psi = 0.0;
		PSITable tab= new PSITable();
		if (key.contains("COConc")) {
			double averageConv=average/1000;
			if (averageConv<=tab.getCoa()[1]) {
				psi=(averageConv-tab.getCoa()[0])*(50-0)/(tab.getCoa()[1]-tab.getCoa()[0])+0;
			}else if(tab.getCob()[0]<=averageConv&&averageConv<=tab.getCob()[1]) {
				psi=(averageConv-tab.getCob()[0])*(100-51)/(tab.getCob()[1]-tab.getCob()[0])+51;
			}else if(tab.getCoc()[0]<=averageConv&&averageConv<=tab.getCoc()[1]) {
				psi=(averageConv-tab.getCoc()[0])*(200-101)/(tab.getCoc()[1]-tab.getCoc()[0])+101;
			}else if(tab.getCod()[0]<=averageConv&&averageConv<=tab.getCod()[1]) {
				psi=(averageConv-tab.getCod()[0])*(300-201)/(tab.getCod()[1]-tab.getCod()[0])+201;
			}else if(tab.getCoe()[0]<=averageConv&&averageConv<=tab.getCoe()[1]) {
				psi=(averageConv-tab.getCoe()[0])*(400-301)/(tab.getCoe()[1]-tab.getCoe()[0])+301;
			}else if(tab.getCof()[0]<=averageConv&&averageConv<=tab.getCof()[1]) {
				psi=(averageConv-tab.getCof()[0])*(500-401)/(tab.getCof()[1]-tab.getCof()[0])+401;
			}

		} else if (key.contains("SO2Conc")) {
			if (average<=tab.getSo2a()[1]) {
				psi=(average-tab.getSo2a()[0])*(50-0)/(tab.getSo2a()[1]-tab.getSo2a()[0])+0;
			}else if(tab.getSo2b()[0]<=average&&average<=tab.getSo2b()[1]) {
				psi=(average-tab.getSo2b()[0])*(100-51)/(tab.getSo2b()[1]-tab.getSo2b()[0])+51;
			}else if(tab.getSo2c()[0]<=average&&average<=tab.getSo2c()[1]) {
				psi=(average-tab.getSo2c()[0])*(200-101)/(tab.getSo2c()[1]-tab.getSo2c()[0])+101;
			}else if(tab.getSo2d()[0]<=average&&average<=tab.getSo2d()[1]) {
				psi=(average-tab.getSo2d()[0])*(300-201)/(tab.getSo2d()[1]-tab.getSo2d()[0])+201;
			}else if(tab.getSo2e()[0]<=average&&average<=tab.getSo2e()[1]) {
				psi=(average-tab.getSo2e()[0])*(400-301)/(tab.getSo2e()[1]-tab.getSo2e()[0])+301;
			}else if(tab.getSo2f()[0]<=average&&average<=tab.getSo2f()[1]) {
				psi=(average-tab.getSo2f()[0])*(500-401)/(tab.getSo2f()[1]-tab.getSo2f()[0])+401;
			}

		} else if (key.contains("O3Conc")) {
			if (average<=tab.getO3a()[1]) {
				psi=(average-tab.getO3a()[0])*(50-0)/(tab.getO3a()[1]-tab.getO3a()[0])+0;
			}else if(tab.getO3b()[0]<=average&&average<=tab.getO3b()[1]) {
				psi=(average-tab.getO3b()[0])*(100-51)/(tab.getO3b()[1]-tab.getO3b()[0])+51;
			}else if(tab.getO3c()[0]<=average&&average<=tab.getO3c()[1]) {
				psi=(average-tab.getO3c()[0])*(200-101)/(tab.getO3c()[1]-tab.getO3c()[0])+101;
			}else if(tab.getO3d()[0]<=average&&average<=tab.getO3d()[1]) {
				psi=(average-tab.getO3d()[0])*(300-201)/(tab.getO3d()[1]-tab.getO3d()[0])+201;
			}else if(tab.getO3e()[0]<=average&&average<=tab.getO3e()[1]) {
				psi=(average-tab.getO3e()[0])*(400-301)/(tab.getO3e()[1]-tab.getO3e()[0])+301;
			}else if(tab.getO3f()[0]<=average&&average<=tab.getO3f()[1]) {
				psi=(average-tab.getO3f()[0])*(500-401)/(tab.getO3f()[1]-tab.getO3f()[0])+401;
			}

		} else if (key.contains("PM10Conc")) {
			if (average<=tab.getPm10a()[1]) {
				psi=(average-tab.getPm10a()[0])*(50-0)/(tab.getPm10a()[1]-tab.getPm10a()[0])+0;
			}else if(tab.getPm10b()[0]<=average&&average<=tab.getPm10b()[1]) {
				psi=(average-tab.getPm10b()[0])*(100-51)/(tab.getPm10b()[1]-tab.getPm10b()[0])+51;
			}else if(tab.getPm10c()[0]<=average&&average<=tab.getPm10c()[1]) {
				psi=(average-tab.getPm10c()[0])*(200-101)/(tab.getPm10c()[1]-tab.getPm10c()[0])+101;
			}else if(tab.getPm10d()[0]<=average&&average<=tab.getPm10d()[1]) {
				psi=(average-tab.getPm10d()[0])*(300-201)/(tab.getPm10d()[1]-tab.getPm10d()[0])+201;
			}else if(tab.getPm10e()[0]<=average&&average<=tab.getPm10e()[1]) {
				psi=(average-tab.getPm10e()[0])*(400-301)/(tab.getPm10e()[1]-tab.getPm10e()[0])+301;
			}else if(tab.getPm10f()[0]<=average&&average<=tab.getPm10f()[1]) {
				psi=(average-tab.getPm10f()[0])*(500-401)/(tab.getPm10f()[1]-tab.getPm10f()[0])+401;
			}

		} else if (key.contains("PM25Conc")) {
			System.out.println("it goes pm25");
			if (average<=tab.getPm25a()[1]) {
				psi=(average-tab.getPm25a()[0])*(50-0)/(tab.getPm25a()[1]-tab.getPm25a()[0])+0;
			}else if(tab.getPm25b()[0]<=average&&average<=tab.getPm25b()[1]) {
				psi=(average-tab.getPm25b()[0])*(100-51)/(tab.getPm25b()[1]-tab.getPm25b()[0])+51;
			}else if(tab.getPm25c()[0]<=average&&average<=tab.getPm25c()[1]) {
				psi=(average-tab.getPm25c()[0])*(200-101)/(tab.getPm25c()[1]-tab.getPm25c()[0])+101;
			}else if(tab.getPm25d()[0]<=average&&average<=tab.getPm25d()[1]) {
				psi=(average-tab.getPm25d()[0])*(300-201)/(tab.getPm25d()[1]-tab.getPm25d()[0])+201;
			}else if(tab.getPm25e()[0]<=average&&average<=tab.getPm25e()[1]) {
				psi=(average-tab.getPm25e()[0])*(400-301)/(tab.getPm25e()[1]-tab.getPm25e()[0])+301;
			}else if(tab.getPm25f()[0]<=average&&average<=tab.getPm25f()[1]) {
				psi=(average-tab.getPm25f()[0])*(500-401)/(tab.getPm25f()[1]-tab.getPm25f()[0])+401;
			}

		} else if (key.contains("NO2Conc")) {
			if (average<=tab.getNo2a()[1]) {
				psi=(average-tab.getNo2a()[0])*(50-0)/(tab.getNo2a()[1]-tab.getNo2a()[0])+0;
			}else if(tab.getNo2b()[0]<=average&&average<=tab.getNo2b()[1]) {
				psi=(average-tab.getNo2b()[0])*(100-51)/(tab.getNo2b()[1]-tab.getNo2b()[0])+51;
			}else if(tab.getNo2c()[0]<=average&&average<=tab.getNo2c()[1]) {
				psi=(average-tab.getNo2c()[0])*(200-101)/(tab.getNo2c()[1]-tab.getNo2c()[0])+101;
			}else if(tab.getNo2d()[0]<=average&&average<=tab.getNo2d()[1]) {
				psi=(average-tab.getNo2d()[0])*(300-201)/(tab.getNo2d()[1]-tab.getNo2d()[0])+201;
			}else if(tab.getNo2e()[0]<=average&&average<=tab.getNo2e()[1]) {
				psi=(average-tab.getNo2e()[0])*(400-301)/(tab.getNo2e()[1]-tab.getNo2e()[0])+301;
			}else if(tab.getNo2f()[0]<=average&&average<=tab.getNo2f()[1]) {
				psi=(average-tab.getNo2f()[0])*(500-401)/(tab.getNo2f()[1]-tab.getNo2f()[0])+401;
			}

		}

		return psi;
	}

		public List<Double> executeModel(List<String[]>input) {
			
			double sum=0.0;
			double min=Double.valueOf(input.get(1)[0]);
			double max=0.0;
			String type=input.get(0)[0];
			

			for(int x=1;x<input.size();x++) {
				if(Double.valueOf(input.get(x)[0])<min) {
					min=Double.valueOf(input.get(x)[0]);
				}
				else if(Double.valueOf(input.get(x)[0])>max) {
					max=Double.valueOf(input.get(x)[0]);
				}
				sum=sum+Double.valueOf(input.get(x)[0]);
			}
			System.out.println("sum="+sum);
			System.out.println("denominator= "+(input.size()-1));
			double mean=sum/(input.size()-1);
			
			double psi=calculatePSI(mean,type);
			

			//r code is executed
			List<Double>output= new ArrayList<Double>();
			output.add(mean);
			output.add(min);
			output.add(max);
			output.add(psi);
			return output;
		}
		
		public List<String[]> prepareInput(String context,String propnameclass,String dataPath) {
			//TODO: make the averaging time according to the psa table, for both the aqmesh and virtual sensor
			String limit="8";
			if(context.toLowerCase().contains("aqmesh")) {
				if(propnameclass.contains("PM")||propnameclass.contains("SO2")) {
					limit="288";
				}else if(propnameclass.contains("COConc")||propnameclass.contains("O3")) {
					limit="96";
				}else if(propnameclass.contains("NO2")) {
					limit="12";
				}
				
			}else {
				if(propnameclass.contains("PM")||propnameclass.contains("SO2")) {
					limit="24";
				}else if(propnameclass.contains("COConc")||propnameclass.contains("O3")) {
					limit="8";
				}else if(propnameclass.contains("NO2")) {
					limit="2";
				}
			}
			String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
					+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "SELECT ?propval ?proptimeval "
//					+ "WHERE " //it's replaced when named graph is used
					+ "{graph "+"<"+context+">"
					+ "{ "
					+ " ?prop a j4:"+propnameclass+" ."
					+ " ?prop   j2:hasValue ?vprop ."
					+ " ?vprop   j4:prescaledNumValue ?propval ."
					+ " ?vprop   j6:hasTime ?proptime ."
//					+ " ?proptime   j6:hasBeginning ?proptimestart ."
//					+ " ?proptime   j6:hasEnd ?proptimeend ."
					+ " ?proptime   j6:inXSDDateTime ?proptimeval ."
//					+ " ?proptimestart   j6:inXSDDateTimeStamp ?proptimestartval ." 
//					+ " ?proptimeend   j6:inXSDDateTimeStamp ?proptimeendval ."
					+ "}" 
					+ "}"
					+ "ORDER BY DESC(?proptimeval)LIMIT"+limit; //take 8 newest
			
			List<String[]> resultQuery =queryEndPointDataset(sensorinfo);
			String[]header= {propnameclass,"starttime"};
			resultQuery.add(0,header);
			String filename= propnameclass+"latestdata.csv"; //what is input file name
			//new QueryBroker().putLocal(dataPath + "/" + filename, MatrixConverter.fromArraytoCsv(resultQuery));
			
			return resultQuery;
		}
		
		public void updateOverallPSI(String context,int psifin) {
			String sparqlupdate = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
					+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "WITH <" + context + ">"
					+ "DELETE { "
					+ "<" + context+ "> j4:hasOverallPSI ?oldpsi ."
					+ "} "
					+ "INSERT {"
					+ "<" + context+ "> j4:hasOverallPSI \""+psifin+"\"^^xsd:integer ."
					+ "} "
					+ "WHERE { "
					+ "<" + context+ "> j4:hasOverallPSI ?oldpsi ."
					+ "}";
			
			KnowledgeBaseClient.update(dataseturl, null, sparqlupdate);
		}
		
		public int updateTheEndpoint(String context,String propnameclass, List<Double>newvalues) {
			String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
					+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "SELECT ?prop "
//					+ "WHERE " //it's replaced when named graph is used
					+ "{graph "+"<"+context+">"
					+ "{ "
					+ " ?prop a j4:"+propnameclass+" ."
//					+ " ?prop   j4:hasMeasuredPropertyMean ?vMean ." 
//					+ " ?prop   j4:hasMeasuredPropertyVariance ?vVariance ." 
//					+ " ?prop   j4:hasPSI ?vPSI ." 
//					+ " ?vprop   j6:hasTime ?proptime ."
//					+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." 
					+ "}" 
					+ "}"; 
//					+ "ORDER BY ASC(?proptimeval)LIMIT1";
			
			List<String[]> keyvaluemapold =queryEndPointDataset(sensorinfo);
			double mean=newvalues.get(0);
			double min=newvalues.get(1);
			double max=newvalues.get(2);
			double psi=newvalues.get(3);
			int psifin=(int) Math.round(psi);
			
			String sparqlupdate = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
					+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "WITH <" + context + ">"
					+ "DELETE { "
					+ "<" + keyvaluemapold.get(0)[0]+ "> j4:hasMeasuredPropertyMean ?oldmean ."
					+ "<" + keyvaluemapold.get(0)[0]+ "> j4:hasMeasuredPropertyMin ?oldmin ."
					+ "<" + keyvaluemapold.get(0)[0]+ "> j4:hasMeasuredPropertyMax ?oldmax ."
					+ "<" + keyvaluemapold.get(0)[0]+ "> j4:hasPSI ?oldpsi ."
					+ "} "
					+ "INSERT {"
					+ "<" + keyvaluemapold.get(0)[0]+ "> j4:hasMeasuredPropertyMean \""+mean+"\"^^xsd:double ."
					+ "<" + keyvaluemapold.get(0)[0]+ "> j4:hasMeasuredPropertyMin \""+min+"\"^^xsd:double ."
					+ "<" + keyvaluemapold.get(0)[0]+ "> j4:hasMeasuredPropertyMax \""+max+"\"^^xsd:double ."
					+ "<" + keyvaluemapold.get(0)[0]+ "> j4:hasPSI \""+psifin+"\"^^xsd:integer ."
					+ "} "
					+ "WHERE { "
					+ "<" + keyvaluemapold.get(0)[0]+ "> j4:hasMeasuredPropertyMean ?oldmean ."
					+ "<" + keyvaluemapold.get(0)[0]+ "> j4:hasMeasuredPropertyMin ?oldmin ."
					+ "<" + keyvaluemapold.get(0)[0]+ "> j4:hasMeasuredPropertyMax ?oldmax ."
					+ "<" + keyvaluemapold.get(0)[0]+ "> j4:hasPSI ?oldpsi ."
					+ "}";
			
			KnowledgeBaseClient.update(dataseturl, null, sparqlupdate);
			
			return psifin;
		}
}
