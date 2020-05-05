package uk.ac.cam.cares.jps.dispersion.sensor;

import java.util.List;

import javax.servlet.annotation.WebServlet;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
@WebServlet(urlPatterns = {"/AirQualitySensorAgent","/resetAirQualityRepository"})
public class AirQualitySensorAgent extends JPSHttpServlet {

	
	
	
	
	
	private List<String[]> queryEndPointDataset(String querycontext) {
		String dataseturl = KeyValueManager.get(IKeys.DATASET_WEATHER_URL); //later need to be changed
		String resultfromrdf4j = KnowledgeBaseClient.query(dataseturl, null, querycontext);
		String[] keys = JenaResultSetFormatter.getKeys(resultfromrdf4j);
		List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromrdf4j, keys);
		return listmap;
	}
	
	private String  getDataFromAPI() {
		
		String result="";
		
		return result;
	}
	
	public void executePeriodicUpdate() {
		String result=getDataFromAPI();
		//processed the input to have suitable format
		
		
		
	}
	
	
	
	public void updateRepoNewMethod(String context,String propnameclass, String scaledvalue,String prescaledvalue, String newtimestampstart,String newtimestampend) {
		String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "SELECT ?vprop ?proptimestart ?proptimeend "
//				+ "WHERE " //it's replaced when named graph is used
				+ "{graph "+"<"+context+">"
				+ "{ "
				+ " ?prop a j4:"+propnameclass+" ."
				+ " ?prop   j2:hasValue ?vprop ." 
				+ " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:hasBeginning ?proptimestart ."
				+ " ?proptime   j6:hasEnd ?proptimeend ."
				+ " ?proptimestart   j6:inXSDDateTimeStamp ?proptimestartval ." 
				+ " ?proptimeend   j6:inXSDDateTimeStamp ?proptimeendval ."
				+ "}" 
				+ "}" 
				+ "ORDER BY ASC(?proptimeendval)LIMIT1";
		
		List<String[]> keyvaluemapold =queryEndPointDataset(sensorinfo);
		
		String sparqlupdate = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "WITH <" + context + ">"
				+ "DELETE { "
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:scaledNumericalValue ?oldpropertydata ."
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:prescaledNumericalValue ?oldpropertydata2 ."
				+ "<" + keyvaluemapold.get(0)[1]+ "> j6:inXSDDateTimeStamp ?olddatastart ."
				+ "<" + keyvaluemapold.get(0)[2]+ "> j6:inXSDDateTimeStamp ?olddataend ."
				+ "} "
				+ "INSERT {"
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:scaledNumericalValue \""+scaledvalue+"\"^^xsd:double ."
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:prescaledNumericalValue \""+prescaledvalue+"\"^^xsd:double ."
				+ "<" + keyvaluemapold.get(0)[1]+ "> j6:inXSDDateTimeStamp \""+newtimestampstart+"\" ." 
				+ "<" + keyvaluemapold.get(0)[2]+ "> j6:inXSDDateTimeStamp \""+newtimestampend+"\" ." 
				+ "} "
				+ "WHERE { "
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:scaledNumericalValue ?oldpropertydata ."	
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:prescaledNumericalValue ?oldpropertydata2 ."	
				+ "<" + keyvaluemapold.get(0)[1]+ "> j6:inXSDDateTimeStamp ?olddatastart ."
				+ "<" + keyvaluemapold.get(0)[2]+ "> j6:inXSDDateTimeStamp ?olddataend ."
				+ "}";
		
			
			KnowledgeBaseClient.update(KeyValueManager.get(IKeys.DATASET_WEATHER_URL), null, sparqlupdate); //update the dataset

		
		
	}
	
	
	
}



