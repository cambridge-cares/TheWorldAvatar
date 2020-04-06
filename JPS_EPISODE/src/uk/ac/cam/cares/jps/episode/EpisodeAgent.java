package uk.ac.cam.cares.jps.episode;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.RepositoryConnection;

import uk.ac.cam.cares.jps.base.query.QueryBroker;

public class EpisodeAgent extends DispersionModellingAgent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Override
	public void createWeatherInput(String dataPath, String filename,List<String>stniri) {	
		
		String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>"
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#>"
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#>"
				+ "PREFIX j6:<http://www.w3.org/2006/time#>" 
				+ "SELECT ?entity ?class ?propval ?proptimeval ?graph "
//				+ "WHERE " //it's replaced when named graph is used
				+ "{ GRAPH ?graph "
				+ "{ "
				 
				+ "  ?entity j4:observes ?prop ." 
				+ " ?prop a ?class ."
				+ " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." 
				+ " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." 
				+ "}" 
				+ "}" 
				+ "ORDER BY DESC(?proptimeval)LIMIT 9";
//		String dataseturl="";//which is the weather stn dataset
//		String resultfromrdf4j = KnowledgeBaseClient.query(dataseturl, null, sensorinfo);
//		 String[] keys = JenaResultSetFormatter.getKeys(resultfromrdf4j);
//		 List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromrdf4j, keys);
		RepositoryConnection con = WeatherAgent.repo.getConnection();
		 List<String[]> listmap = new ArrayList<String[]>();
			TupleQuery query = con.prepareTupleQuery(QueryLanguage.SPARQL, sensorinfo);
			TupleQueryResult result = query.evaluate();
			int d = 0;
			try {
				while (result.hasNext()) {
					BindingSet bindingSet = result.next();
					//String iri = bindingSet.getValue("graph").stringValue();
					String classprop = bindingSet.getValue("class").stringValue();
					String propval = bindingSet.getValue("propval").stringValue();
					String dateval = bindingSet.getValue("proptimeval").stringValue();
					String graphval = bindingSet.getValue("graph").stringValue();
					String[] content = { classprop,propval,dateval,graphval};
					listmap.add(content);

					d++;
				}
				System.out.println("total data=" + d);

			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		 System.out.println("size="+listmap.size());
		 
		 List<String[]> resultquery = new ArrayList<String[]>();
			String separator="\t";
			StringBuilder sb= new StringBuilder();
			
			
	        String[]header= {"*","yyyy","mm","dd","hh","FF1","DD1","T25m","DT","RH%","PP_mm","Cloud","Press","FF2","DD2"};
	        resultquery.add(0,header);
	        String time=listmap.get(0)[2];
	        String[]content=new String[15];
	        content[0]="";
	        content[1]=time.split("-")[0];
	        content[2]=time.split("-")[1];
	        content[3]=time.split("-")[2].split("T")[0];
	        content[4]=time.split("-")[2].split("T")[1].split(":")[0];
	        for(int r=0;r<listmap.size();r++) {
//	        	System.out.println(listmap.get(r)[0]);
//	        	System.out.println(listmap.get(r)[3]);
	        	if(listmap.get(r)[3].toLowerCase().contains(stniri.get(1))) {
	        		System.out.println("it goes number 2");
	        		if(listmap.get(r)[0].toLowerCase().contains("speed"))
	        		content[13]=listmap.get(r)[1];
	        		else if(listmap.get(r)[0].toLowerCase().contains("direction")) {
	        			content[14]=listmap.get(r)[1];
	        		}
	        	}else if(listmap.get(r)[3].toLowerCase().contains(stniri.get(0))) {
	        		System.out.println("it goes number 1");
	        		if(listmap.get(r)[0].toLowerCase().contains("speed")) {
		        		content[5]=listmap.get(r)[1];
		        	}else if(listmap.get(r)[0].toLowerCase().contains("direction")) {
		        		content[6]=listmap.get(r)[1];
		        	}else if(listmap.get(r)[0].toLowerCase().contains("temperature")) {
		        		content[7]=listmap.get(r)[1];
		        	}else if(listmap.get(r)[0].toLowerCase().contains("humidity")) {
		        		String decimalhumidity=listmap.get(r)[1];
		        		double percent=Double.valueOf(decimalhumidity)*100;
		        		content[9]=""+percent;
		        	}else if(listmap.get(r)[0].toLowerCase().contains("precipitation")) {
		        		content[10]=listmap.get(r)[1];
		        	}else if(listmap.get(r)[0].toLowerCase().contains("cloud")) {
		        		content[11]=listmap.get(r)[1];
		        	}else if(listmap.get(r)[0].toLowerCase().contains("pressure")) {
		        		content[12]=listmap.get(r)[1];
		        	}
	        	}
	        }
	        content[8]="0.022";
	        resultquery.add(content);
	       
	       //convert to tsv
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
