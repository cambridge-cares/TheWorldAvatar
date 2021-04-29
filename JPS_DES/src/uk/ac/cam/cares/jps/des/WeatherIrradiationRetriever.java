package uk.ac.cam.cares.jps.des;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.TimeZone;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.apache.commons.lang.RandomStringUtils;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.query.Query;
import org.apache.jena.sparql.core.Var;
import org.json.JSONException;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.des.n.DESAgentNew;

@WebServlet(urlPatterns = {"/GetIrradiationandWeatherData" })
public class WeatherIrradiationRetriever extends JPSAgent{
	private static final String TWA_Ontology = "http://www.theworldavatar.com/ontology"; 
	private static final String TWA_upperlevel_system = TWA_Ontology+ "/ontocape/upper_level/system.owl#";
	
	private static final String TWA_CPS =  TWA_Ontology +"/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#";
	private static final String W3_TIME = "http://www.w3.org/2006/time#"; 
	private static final long serialVersionUID = 1L;
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		if (!validateInput(requestParams)) {
			throw new BadRequestException();
		}
		String baseUrl = requestParams.optString("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES"); //create unique uuid
        
		try {
	    	String iritempsensor=requestParams.getString("temperaturesensor");
	    	String iriirradiationsensor=requestParams.getString("irradiationsensor");
	    	readWritedatatoOWL(baseUrl,iritempsensor,iriirradiationsensor);

			return requestParams;
		} catch (Exception e) {
			throw new JPSRuntimeException("");
		}
 		
	}
	
	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            return false;
        }
        try {	      
	        
	        String irioftempF=requestParams.getString("temperaturesensor");	
	        boolean e = InputValidator.checkIfValidIRI(irioftempF);
	        String iriofirrF=requestParams.getString("irradiationsensor");
	        boolean r = InputValidator.checkIfValidIRI(iriofirrF);
	        // Till now, there is no system independent to check if a file path is valid or not. 
	        
	        return e&r;
        } catch (JSONException ex) {
        	return false;
        }

    }
	
	/** as the name exemplifies, read and write data to format. 
	 * In the future, rather than running a python script, another agent should be run to read and receive data from any source. 
	 * 
	 * @param folder
	 * @param iritempsensor
	 * @param iriirradiationsensor
	 * @param irispeedsensor
	 * @throws Exception
	 */
	public static void readWritedatatoOWL(String folder,String iritempsensor,String iriirradiationsensor) throws Exception  { 		
	
		new DESAgentNew().copyFromPython(folder, "ocrv1.py"); //TODO: In the future, this should be scripted to any collection or query from agent
		new DESAgentNew().runPythonScript("ocrv1.py",folder);

		DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss");
		LocalDateTime now = LocalDateTime.now();
		String com=dtf.format(now);		   
		String jsonres=new QueryBroker().readFileLocal(folder+"/data.json");
		JSONObject current= new JSONObject(jsonres);
		String temperature=current.optString("temperature","26");	
		String irradiance=current.optString("irradiance","0.00");		
		TimeZone timezone = TimeZone.getDefault();
		int offset = timezone.getRawOffset();
		String gmtTZ = String.format("%s%02d:%02d", 
		               offset < 0 ? "-" : "+", 
		               Math.abs(offset) / 3600000,
		               Math.abs(offset) / 60000 % 60);

		String timeInXSD =com + gmtTZ;
		//query the data from the existing owl file
		
		 
    	WhereBuilder whereB = new WhereBuilder().addPrefix("j2", TWA_upperlevel_system )
    			.addPrefix("j4", TWA_Ontology +"/ontosensor/OntoSensor.owl#")
    			.addPrefix("j5",TWA_CPS)
    			.addPrefix("j6", "http://www.w3.org/2006/time#").addWhere("?entity", "j4:observes", "?prop")
    			.addWhere("?prop", "j2:hasValue", "?vprop").addWhere("?vprop", "j2:numericalValue", "?propval")
    			.addWhere("?vprop", "j6:hasTime", "?proptime").addWhere("?proptime", "j6:inXSDDateTime", "?proptimeval");
   
    	
    	SelectBuilder sensorTemp = new SelectBuilder()
    			.addPrefix("j5",TWA_CPS)
    			.addVar("?vprop").addVar("?propval").addVar("?proptime").addVar("?proptimeval")
    			.addWhere("?entity","a", "j5:T-Sensor").addWhere(whereB).addOrderBy("?proptimeval");
    	Query q= sensorTemp.build(); 
    	String sensorInfo = q.toString();
    	
    	SelectBuilder sensorIrrad = new SelectBuilder()
    			.addPrefix("j5",TWA_CPS)
    			.addVar("?vprop").addVar("?propval").addVar("?proptime").addVar("?proptimeval")
    			.addWhere("?entity","a", "j5:Q-Sensor").addWhere(whereB).addOrderBy("?proptimeval");
    	
    	q= sensorIrrad.build(); 
    	String sensorInfo2 = q.toString();	
    	updateOWLFile(iritempsensor, sensorInfo,timeInXSD,temperature);
    	updateOWLFile(iriirradiationsensor, sensorInfo2,timeInXSD,irradiance);		
	}
	
	/** SubMethod for readWriteToOWL for each type of sensor
	 * TODO: There's a bug where the returned data comes in UTC format rather than GMT format, but I haven't gotten down to what could have caused this. 
	 * @param sensorIRI
	 * @param sparqlQuery
	 */
	private static void updateOWLFile(String sensorIRI, String sparqlQuery, String timeInXSD, String reading) {
		//To be removed afterwards. 
		String convertedIRI = DESAgentNew.tempIRItoFile(sensorIRI);
    	JSONObject requestParams = new JSONObject().put(JPSConstants.QUERY_SPARQL_QUERY, sparqlQuery)
					.put(JPSConstants.TARGETIRI, convertedIRI );
		String resultf = AgentCaller.executeGetWithJsonParameter(JPSConstants.KNOWLEDGE_BASE_URL, requestParams.toString());
		String[] keysf = {"vprop","propval","proptime","proptimeval"};
		List<String[]>  resultListfromquery = JenaResultSetFormatter.convertToListofStringArraysWithKeys(resultf, keysf);
		UpdateBuilder builder = new UpdateBuilder();
		
		int sizeOfUpdate = resultListfromquery.size();
		String p = "<"+W3_TIME+"inXSDDateTime>";
		String d = "<"+TWA_upperlevel_system+"numericalValue>";
		for (int i = 0; i < sizeOfUpdate-1; i ++ ) {//We stopped at element 48
			Var v = Var.alloc(RandomStringUtils.random(5, true, false));
			Var m = Var.alloc(RandomStringUtils.random(5, true, false)); //random string generate to prevent collision
			builder.addDelete("<"+resultListfromquery.get(i)[0]+">" ,d, v)
					.addInsert("<"+resultListfromquery.get(i)[0]+">" ,d, resultListfromquery.get(i+1)[1])
					.addWhere("<"+resultListfromquery.get(i)[0]+">" ,d,v)					
					.addDelete("<"+resultListfromquery.get(i)[2]+">" ,p, m)
					.addInsert("<"+resultListfromquery.get(i)[2]+">" ,p, resultListfromquery.get(i+1)[3])
					.addWhere("<"+resultListfromquery.get(i)[2]+">" ,p,m);
			if (i %3 == 0) {
				requestParams = new JSONObject().put(JPSConstants.QUERY_SPARQL_UPDATE, builder.build().toString())
						.put(JPSConstants.TARGETIRI ,convertedIRI);
				AgentCaller.executeGetWithJsonParameter(JPSConstants.KNOWLEDGE_BASE_URL, requestParams.toString());
				builder = new UpdateBuilder();
				
			}			
		}
		//final round
		builder.addDelete("<"+resultListfromquery.get(sizeOfUpdate-1)[0]+">" ,d, "?o")		
		.addInsert("<"+resultListfromquery.get(sizeOfUpdate-1)[0]+">" ,d, reading)
		.addWhere("<"+resultListfromquery.get(sizeOfUpdate-1)[0]+">" ,d,"?o")	
		.addDelete("<"+resultListfromquery.get(sizeOfUpdate-1)[2]+">" ,p, "?a")			
		.addInsert("<"+resultListfromquery.get(sizeOfUpdate-1)[2]+">" ,p, timeInXSD)
		.addWhere("<"+resultListfromquery.get(sizeOfUpdate-1)[2]+">" ,p,"?a");
	
		
		requestParams = new JSONObject().put(JPSConstants.QUERY_SPARQL_UPDATE, builder.build().toString())
				.put(JPSConstants.TARGETIRI ,convertedIRI);
		AgentCaller.executeGetWithJsonParameter(JPSConstants.KNOWLEDGE_BASE_URL, requestParams.toString());
	}
	
	

}
