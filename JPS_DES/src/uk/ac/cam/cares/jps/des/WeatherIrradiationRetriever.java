package uk.ac.cam.cares.jps.des;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.query.Query;
import org.json.JSONException;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.des.n.DESAgentNew;

@WebServlet(urlPatterns = {"/GetIrradiationandWeatherData" })
public class WeatherIrradiationRetriever extends JPSAgent{
	private static final long serialVersionUID = 1L;
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    requestParams = processRequestParameters(requestParams, null);
	    return requestParams;
	}
	@Override 
	public JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {

		if (!validateInput(requestParams)) {
			throw new BadRequestException("WeatherIrradiationAgent: Input parameters not found.\n");
		}
		String baseUrl = requestParams.optString("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES"); //create unique uuid
        
		try {
	    	String iritempsensor=requestParams.getString("temperaturesensor");
	    	String iriirradiationsensor=requestParams.getString("irradiationsensor");
	    	String irispeedsensor=requestParams.getString("windspeedsensor");
	    	readWritedatatoOWL(baseUrl,iritempsensor,iriirradiationsensor,irispeedsensor);

			return requestParams;
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return requestParams;
 		
	}
	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {
	        String iriofwindF = requestParams.getString("windspeedsensor");
	        boolean w = InputValidator.checkIfValidIRI(iriofwindF);
	        
	        String irioftempF=requestParams.getString("temperaturesensor");
	
	        boolean e = InputValidator.checkIfValidIRI(irioftempF);
	        String iriofirrF=requestParams.getString("irradiationsensor");
	        boolean r = InputValidator.checkIfValidIRI(iriofirrF);
	        // Till now, there is no system independent to check if a file path is valid or not. 
	        
	        return w&e&r;
        } catch (JSONException ex) {
        	ex.printStackTrace();
        	throw new JSONException("Sensor not present in getString");
        }

    }
	public static void readWritedatatoOWL(String folder,String iritempsensor,String iriirradiationsensor,String irispeedsensor) throws Exception  { 		
		//TODO: I can't figure out how to get this to run without having to copy over a file to create
		//the location within this folder. If I leave the line below commented
		// the folder isn't created, and I get an error. 
		new DESAgentNew().copyFromPython(folder, "ocrv1.py");
		String res =  new DESAgentNew().runPythonScript("ocrv1.py",folder);

		DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss");
		LocalDateTime now = LocalDateTime.now();
		String com=dtf.format(now);
		String date=com.split("/")[2].split(" ")[0];
		   
		String jsonres=new QueryBroker().readFileLocal(folder+"/data.json");
		JSONObject current= new JSONObject(jsonres);
		String year=current.optString("year",com.split("/")[0]);
		String month=current.optString("month",com.split("/")[1]);

		String day=current.optString("date",date);
		String time=current.optString("time",com.split("/")[2].split(" ")[1]);
		String temperature=current.optString("temperature","26");	

		String timeInXSD =year + "-" +  month +"-"+ day+"T"+ time +"+08:00";
		//query the data from the existing owl file
		
		 
    	WhereBuilder whereB = new WhereBuilder().addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
    			.addPrefix("j4", "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#")
    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
    			.addPrefix("j6", "http://www.w3.org/2006/time#").addWhere("?entity", "j4:observes", "?prop")
    			.addWhere("?prop", "j2:hasValue", "?vprop").addWhere("?vprop", "j2:numericalValue", "?propval")
    			.addWhere("?vprop", "j6:hasTime", "?proptime").addWhere("?proptime", "j6:inXSDDateTime", "?proptimeval");
   
    	
    	SelectBuilder sensorTemp = new SelectBuilder()
    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
    			.addVar("?vprop").addVar("?propval").addVar("?proptime").addVar("?proptimeval")
    			.addWhere("?entity","a", "j5:T-Sensor").addWhere(whereB).addOrderBy("?proptimeval");
    	Query q= sensorTemp.build(); 
    	String sensorInfo = q.toString();
    	
    	SelectBuilder sensorIrrad = new SelectBuilder()
    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
    			.addVar("?vprop").addVar("?propval").addVar("?proptime").addVar("?proptimeval")
    			.addWhere("?entity","a", "j5:Q-Sensor").addWhere(whereB).addOrderBy("?proptimeval");
    	
    	q= sensorIrrad.build(); 
    	String sensorInfo2 = q.toString();
    	
    	SelectBuilder sensorWind = new SelectBuilder()
    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
    			.addVar("?propval").addVar("?proptimeval")
    			.addWhere("?entity","a", "j5:F-Sensor").addWhere(whereB).addOrderBy("?proptimeval");    	
    	q= sensorWind.build(); 
    	
    	String sensorInfo3 = q.toString();
    	String result = new QueryBroker().queryFile(iritempsensor, sensorInfo);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultListfromquerytemp = JenaResultSetFormatter.convertToListofStringArrays(result, keys);

    	updateOWLFile(iritempsensor, sensorInfo,timeInXSD,temperature);
    	updateOWLFile(iriirradiationsensor, sensorInfo2,timeInXSD,temperature);
    	updateOWLFile(irispeedsensor, sensorInfo3,timeInXSD,temperature);
    	
	
		
	}
	/** SubMethod for readWriteToOWL for each type of sensor
	 * 
	 * @param sensorIRI
	 * @param sparqlQuery
	 */
	private static void updateOWLFile(String sensorIRI, String sparqlQuery, String timeInXSD, String reading) {
		//To be removed afterwards. 
		String convertedIRI = DESAgentNew.tempIRItoFile(sensorIRI);
    	JSONObject requestParams = new JSONObject().put(JPSConstants.QUERY_SPARQL_QUERY, sparqlQuery)
					.put(JPSConstants.TARGETIRI, convertedIRI );
		String resultf = AgentCaller.executeGetWithJsonParameter("jps/kb", requestParams.toString());
		String[] keysf = {"vprop","propval","proptime","proptimeval"};
		List<String[]>  resultListfromquery = JenaResultSetFormatter.convertToListofStringArraysWithKeys(resultf, keysf);
		UpdateBuilder builder = new UpdateBuilder();
		
		int sizeOfUpdate = resultListfromquery.size();
		String p = "<http://www.w3.org/2006/time#inXSDDateTime>";
		String d = "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>";
		for (int i = 0; i < sizeOfUpdate-2; i ++ ) {//We stopped at element 46
			builder.addDelete("<"+resultListfromquery.get(i)[0]+">" ,d, "?o")
					.addInsert("<"+resultListfromquery.get(i)[0]+">" ,d, resultListfromquery.get(i+1)[1])
					.addWhere("<"+resultListfromquery.get(i)[0]+">" ,d,"?o")
					
					.addDelete("<"+resultListfromquery.get(i)[2]+">" ,p, "?a")
					.addInsert("<"+resultListfromquery.get(i)[2]+">" ,p, resultListfromquery.get(i+1)[3])
					.addWhere("<"+resultListfromquery.get(i)[2]+">" ,p,"?a");
			if (i %3 == 0) {
				requestParams = new JSONObject().put(JPSConstants.QUERY_SPARQL_UPDATE, builder.build().toString())
						.put(JPSConstants.TARGETIRI ,convertedIRI);
				AgentCaller.executeGetWithJsonParameter("jps/kb", requestParams.toString());
				builder = new UpdateBuilder();
				
			}
		}
		//final round
		builder.addInsert("<"+resultListfromquery.get(sizeOfUpdate-1)[0]+">" ,d, reading)
		.addWhere("<"+resultListfromquery.get(sizeOfUpdate-1)[0]+">" ,d,"?o")		
		.addInsert("<"+resultListfromquery.get(sizeOfUpdate-1)[2]+">" ,p, timeInXSD)
		.addWhere("<"+resultListfromquery.get(sizeOfUpdate-1)[2]+">" ,p,"?a");
	
		
		requestParams = new JSONObject().put(JPSConstants.QUERY_SPARQL_UPDATE, builder.build().toString())
				.put(JPSConstants.TARGETIRI ,convertedIRI);
		AgentCaller.executeGetWithJsonParameter("jps/kb", requestParams.toString());
	}
	


}
