package uk.ac.cam.cares.jps.des;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.apache.commons.lang.RandomStringUtils;
import org.apache.jena.arq.querybuilder.Order;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.query.Query;
import org.apache.jena.sparql.core.Var;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MiscUtil;
import uk.ac.cam.cares.jps.des.n.DESAgentNew;
@WebServlet(urlPatterns = {"/GetForecastData" })
public class ForecastAgent extends JPSAgent{
	private static final long serialVersionUID = 1L;
	private static String SolCastURL = null;
	private static String AccuWeatherURL = null;
	
	public ForecastAgent() {
		String fileName = AgentLocator.getCurrentJpsAppDirectory(this) + "\\resources\\config.properties";
		try (InputStream input = new FileInputStream(fileName)) {

            Properties prop = new Properties();
            //load a properties file from class path, inside static method
            prop.load(input);

            SolCastURL = prop.getProperty("SolCastURL");
            AccuWeatherURL = prop.getProperty("AccuWeatherURL");

        } catch (IOException ex) {
            throw new JPSRuntimeException("");
        }
	}
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		if (!validateInput(requestParams)) {
    		throw new BadRequestException();
    	}
    	try {
            String irioftempF=requestParams.getString("temperatureforecast");
            String iriofirrF=requestParams.getString("irradiationforecast");
    		nextForecastDaySolcast(irioftempF,iriofirrF);
    		nextForecastDayTemperature(irioftempF);
		
		} catch (Exception e) {
			throw new JPSRuntimeException("");
		}
    	return requestParams;
    }
	
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            return false;
        }
        String iriofnetwork = requestParams.getString("electricalnetwork");
        boolean q = InputValidator.checkIfValidIRI(iriofnetwork);

        String iriofdistrict = requestParams.getString("district");
        boolean w = InputValidator.checkIfValidIRI(iriofdistrict);
        
        String irioftempF=requestParams.getString("temperatureforecast");

        boolean e = InputValidator.checkIfValidIRI(irioftempF);
        String iriofirrF=requestParams.getString("irradiationforecast");
        boolean r = InputValidator.checkIfValidIRI(iriofirrF);
        
        return q&w&e&r;
       

    }
    
	/** reads data from Solcast.com.au (need API key) and stores in ArrayList<String[]>
	 * Length of 24 hours, as we don't want a thirty minute update. 
	 * @return ArrayList[temperature, irradiation, timeInXSD]
	 * @throws IOException
	 */
	public static ArrayList<String[]> callSolarAPI() {
		JSONObject jo =  new JSONObject(AgentCaller.getRequestBody(SolCastURL));
		JSONArray arr = jo.getJSONArray("forecasts");
	    DecimalFormat doubSF = new DecimalFormat("##.#");
	    ArrayList<String[]> ans = new ArrayList<String[]>();	    
		for (int i = 0; i< 48; i+= 2) {
        	JSONObject object = arr.getJSONObject(i);
        	int temp= object.getInt("air_temp");
        	String tempera = doubSF.format(temp);
        	int ghi = object.getInt("ghi");
        	String irrad = doubSF.format(ghi);//because windspeed is in km/h and not m/s
        	String periodEnd = (object.getString("period_end").split("\\."))[0]+"Z";
        	String timeInXSD = MiscUtil.convertToTimeZoneXSD(periodEnd);
        	String[] lstInit = {tempera, irrad, timeInXSD};
        	ans.add(lstInit);
        }   
        return ans;     
	}
	/** reads data from AccuWeather for Singapore (need API key) and stores in ArrayList<String[]>
	 * Length of 12 hours; 24 hours is paid plan. 
	 * 
	 * @return ArrayList[Temperature, dateTime]
	 * @throws IOException
	 */
	public ArrayList<String[]> callAccuAPI() {

		JSONArray arr = new JSONArray(AgentCaller.getRequestBody(AccuWeatherURL));
		DecimalFormat doubSF = new DecimalFormat("##.#");
		ArrayList<String[]> accuArray = new ArrayList<String[]>();
		for (int i = 0; i< arr.length(); i++) {
			JSONObject object = arr.getJSONObject(i);
			JSONObject temp= object.getJSONObject("Temperature");
			double temper = temp.getFloat("Value");
			String temperature = doubSF.format(temper);
			
			String dateInXSD= object.getString("DateTime");
			String[] args = {temperature, dateInXSD};
			accuArray.add(args);
		}        
		return accuArray;
	}
	/** Utilize a static method when there is only one right answer and it isn't changing. 
	 * 
	 * @return WhereBuilder for nextForecastDay temperatures. 
	 */
	private static WhereBuilder whereQueryBuilderForSensor() {
		WhereBuilder whereB = new WhereBuilder().addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
    			.addPrefix("j4", "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#")
    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
    			.addPrefix("j6", "http://www.w3.org/2006/time#").addWhere("?entity", "j4:observes", "?prop")
    			.addWhere("?prop", "j2:hasValue", "?vprop")
    			.addWhere("?vprop", "j6:hasTime", "?proptime")
    			.addWhere("?proptime", "j6:inXSDDateTime", "?proptimeval");
		return whereB;
	}
	
	/** calls AccuAPI and updates forecast temperature IRI 
	 * In the future, callAccuAPI should be replaced with an agent to call appropriate weather data
	 * @param iriTemperature
	 */
	public void nextForecastDayTemperature(String iriTemperature) {
		//First download AccuAPI and push data from temperature for first 12 hours
		ArrayList<String[]> accuArray = callAccuAPI();
		WhereBuilder whereB = whereQueryBuilderForSensor();   
		SelectBuilder sensorTemp = new SelectBuilder()
    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
    			.addVar("?vprop").addVar("?proptime")
    			.addWhere("?entity","a", "j5:T-Sensor").addWhere(whereB).addOrderBy("?proptimeval").setLimit(12);
    	Query q= sensorTemp.build(); 
    	String sensorInfo = q.toString();
    	String convertedIRI = DESAgentNew.tempIRItoFile(iriTemperature);
    	
    	JSONObject requestParams = new JSONObject().put(JPSConstants.QUERY_SPARQL_QUERY, sensorInfo)
				.put(JPSConstants.TARGETIRI, convertedIRI );
		String resultf = AgentCaller.executeGetWithJsonParameter("jps/kb", requestParams.toString());
		String[] keysf = {"vprop","proptime"};
		List<String[]>  resultListfromquery = JenaResultSetFormatter.convertToListofStringArraysWithKeys(resultf, keysf);
    	int[] indices = {0,1};
		updateOWLFileWithResultList(resultListfromquery,accuArray, convertedIRI, indices); 
		
	}
	/** calls solcast API and updates forecast irradiation API
	 * 
	 * @param iriTemperature
	 * @param iriIrradiation
	 */
	public void nextForecastDaySolcast(String iriTemperature, String iriIrradiation) {
		ArrayList<String[]> solArray = callSolarAPI();
		WhereBuilder whereB = whereQueryBuilderForSensor();
		SelectBuilder sensorIrrad = new SelectBuilder()
    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
    			.addVar("?vprop").addVar("?propval").addVar("?proptime").addVar("?proptimeval")
    			.addWhere("?entity","a", "j5:Q-Sensor").addWhere(whereB).addOrderBy("?proptimeval");
		SelectBuilder sensorTemp = new SelectBuilder()
    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
    			.addVar("?vprop").addVar("?proptime")
    			.addWhere("?entity","a", "j5:T-Sensor").addWhere(whereB).addOrderBy("?proptimeval",Order.DESCENDING).setLimit(12);
    	Query q= sensorTemp.build(); 
    	String sensorInfo = q.toString();
    	String convertedIRITemp = DESAgentNew.tempIRItoFile(iriTemperature);
    	q= sensorIrrad.build(); 
    	String sensorInfo2 = q.toString();
    	String convertedIRIIrrad = DESAgentNew.tempIRItoFile(iriIrradiation);
    	JSONObject requestParams = new JSONObject().put(JPSConstants.QUERY_SPARQL_QUERY, sensorInfo)
				.put(JPSConstants.TARGETIRI, convertedIRITemp);
		String resultf = AgentCaller.executeGetWithJsonParameter("jps/kb", requestParams.toString());
		String[] keysf = {"vprop","proptime"};
		List<String[]>  resultListfromquery = JenaResultSetFormatter.convertToListofStringArraysWithKeys(resultf, keysf);
    	Collections.reverse(resultListfromquery);
    	int[] indices = {0,2};
    	ArrayList<String []> tempArray = new ArrayList<String []>(solArray.subList(12, 24));
		updateOWLFileWithResultList(resultListfromquery,tempArray, convertedIRITemp, indices); 
		int[] indices2 = {1,2};
		requestParams = new JSONObject().put(JPSConstants.QUERY_SPARQL_QUERY, sensorInfo2)
				.put(JPSConstants.TARGETIRI, convertedIRIIrrad);
		resultf = AgentCaller.executeGetWithJsonParameter("jps/kb", requestParams.toString());
		resultListfromquery = JenaResultSetFormatter.convertToListofStringArraysWithKeys(resultf, keysf);
		updateOWLFileWithResultList(resultListfromquery,solArray, convertedIRIIrrad, indices2); 
    	
	}
	/** SubMethod for readWriteToOWL for each type of sensor
	 * @param sensorIRI
	 * @param sparqlQuery
	 */
	private static void updateOWLFileWithResultList(List<String[]>  resultListfromquery,
			ArrayList<String[]> accuArray,  String sensorIRI, int[] indices) {
		
		UpdateBuilder builder = new UpdateBuilder();
		JSONObject requestParams = new JSONObject();
		int sizeOfUpdate = resultListfromquery.size();
		String p = "<http://www.w3.org/2006/time#inXSDDateTime>";
		String d = "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>";
		for (int i = 0; i < sizeOfUpdate; i ++ ) {//We stopped at element 46
			Var v = Var.alloc(RandomStringUtils.random(5, true, false));
			Var m = Var.alloc(RandomStringUtils.random(4, true, false)); //random string generate to prevent collusion
			builder.addDelete("<"+resultListfromquery.get(i)[0]+">" ,d, v)
					.addInsert("<"+resultListfromquery.get(i)[0]+">" ,d,  accuArray.get(i)[indices[0]])
					.addWhere("<"+resultListfromquery.get(i)[0]+">" ,d,v)					
					.addDelete("<"+resultListfromquery.get(i)[1]+">" ,p, m)
					.addInsert("<"+resultListfromquery.get(i)[1]+">" ,p, accuArray.get(i)[indices[1]])
					.addWhere("<"+resultListfromquery.get(i)[1]+">" ,p,m);
			if (i %3 == 0) {
				requestParams = new JSONObject().put(JPSConstants.QUERY_SPARQL_UPDATE, builder.build().toString())
						.put(JPSConstants.TARGETIRI ,sensorIRI);
				AgentCaller.executeGetWithJsonParameter("jps/kb", requestParams.toString());
				builder = new UpdateBuilder();
				
			}
		}
		
		//finally
		requestParams = new JSONObject().put(JPSConstants.QUERY_SPARQL_UPDATE, builder.build().toString())
				.put(JPSConstants.TARGETIRI ,sensorIRI);
		AgentCaller.executeGetWithJsonParameter("jps/kb", requestParams.toString());
	}
	

	
	
}
