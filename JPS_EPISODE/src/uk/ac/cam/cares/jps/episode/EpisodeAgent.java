package uk.ac.cam.cares.jps.episode;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.query.QueryBroker;

public class EpisodeAgent extends DispersionModellingAgent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Override
	public void createWeatherInput() {	
		String dataPath = QueryBroker.getLocalDataPath();
		String filename="mcwind_input_singapore_20191118.txt";
		
		String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
				+ "SELECT ?entity ?class ?propval ?proptimeval "
//				+ "WHERE " //it's replaced when named graph is used
				+ "{graph ?graph"
				+ "{ "
				 
				+ "  ?entity j4:observes ?prop ." 
				+ " ?entity a ?class ."
				+ " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." 
				+ " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." 
				+ "}" 
				+ "}" 
				+ "ORDER BY DESC(?proptimeval)LIMIT 10";
		String dataseturl="";//which is the weather stn dataset
		String resultfromrdf4j = KnowledgeBaseClient.query(dataseturl, null, sensorinfo);
		 String[] keys = JenaResultSetFormatter.getKeys(resultfromrdf4j);
		 List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromrdf4j, keys);
		 
		 List<String[]> resultquery = new ArrayList<String[]>();
			String separator="\t";
			StringBuilder sb= new StringBuilder();
			
			
	        String[]header= {"*","yyyy","mm","dd","hh","FF1","DD1","T25m","DT","RH%","PP_mm","Cloud","Press","FF2","DD2"};
	        resultquery.add(0,header);
	        
	        
	        String []content= {"",};
	        
	        for(int v=0;v<resultquery.size();v++) {
		        for (int c=0;c<resultquery.get(0).length;c++) {
		        	sb.append(resultquery.get(v)[c]);
		        	if(resultquery.get(0).length-c!=1) {
		        		sb.append(separator);
		        	}
		        	else {
		        		sb.append("\n");
		        	}
		        }
	        	
	        }
	        new QueryBroker().putLocal(dataPath + "/"+filename, sb.toString()); 	
	}
}
