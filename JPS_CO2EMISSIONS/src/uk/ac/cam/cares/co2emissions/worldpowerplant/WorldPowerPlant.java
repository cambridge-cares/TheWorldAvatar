package uk.ac.cam.cares.co2emissions.worldpowerplant;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.PythonException;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

import com.google.gson.Gson;

import com.rabbitmq.client.BuiltinExchangeType;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
/**
 * Servlet implementation class WorldPowerPlant
 */
@WebServlet("/WorldPowerPlant")
public class WorldPowerPlant extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private static final String EXCHANGE_NAME = "jps.agents";
	private static final String AGENT_IRI = "agent_iri";
	private static final String SCENARIO_ID_1 = "1";
	private static final String SCENARIO_ID_2 = "2";
	private static final String SCENARIO_ID_3 = "3";
	private static final String SCENARIO_ID_4 = "4";
	private static final String SCENARIO_ID_5 = "5";
	
	private static final String PATH = "/JPS_CO2EMISSIONS/SurrogateModel";
	private static final String KEY = "query";
	private static final Gson g = new Gson();
       
    public WorldPowerPlant() {
        super();
    }
    
    protected void publishMessage(String powerplantIRI, String idScenario, Channel channel) throws UnsupportedEncodingException, IOException {
		Map<String, String> queryParamsJsonObj = new HashMap<String, String>();
		queryParamsJsonObj.put("plant", powerplantIRI);
		String queryParamPowerplantIRIString = g.toJson(queryParamsJsonObj);
//		System.out.println(queryParamPowerplantIRIString);
		
		queryParamsJsonObj.put("agent iri", AGENT_IRI);
		queryParamsJsonObj.put("scenario id", idScenario);
		String queryParamsString = g.toJson(queryParamsJsonObj);
		
		channel.basicPublish(EXCHANGE_NAME, "", null, queryParamsString.getBytes("UTF-8"));
		AgentCaller.executeGet(PATH, KEY, queryParamPowerplantIRIString);			
    }
    
    protected void publishMessages(String[] arrayPowerplantIRI, int numPowerplants, int numSegments, Channel channel) throws UnsupportedEncodingException, IOException {
    	int sizeSegment = numPowerplants / numSegments;
    	int remainder = numPowerplants % numSegments;
    	int[] segmentInitialValue = new int[numSegments];
    	Arrays.fill(segmentInitialValue, 0);
    	
    	int additional = 0;
		int previousSegment = 0;
		for (int i = 0; i < numSegments; i++) {
			int increment = 0;
			if(i == 0) {
				continue;
			}
			if(additional < remainder) {
				increment = 1;
				additional++;
			}
			segmentInitialValue[i] = previousSegment = previousSegment + sizeSegment + increment;
		}
		
		int maxLoop = (remainder > 0) ? sizeSegment + 1 : sizeSegment;
		
		for (int i = 0; i < maxLoop; i++) {
			
			if (i < segmentInitialValue[1]) {
//				System.out.println(i);
				System.out.println(arrayPowerplantIRI[i]);
				publishMessage(arrayPowerplantIRI[i], SCENARIO_ID_1, channel);
			}
			
			if (segmentInitialValue[1]+i < segmentInitialValue[2]) {
//				System.out.println(segmentInitialValue[1] + i);
				System.out.println(arrayPowerplantIRI[segmentInitialValue[1]+i]);
				publishMessage(arrayPowerplantIRI[segmentInitialValue[1]+i], SCENARIO_ID_2, channel);
			}
			
			if (segmentInitialValue[2]+i < segmentInitialValue[3]) {
//				System.out.println(segmentInitialValue[2] + i);
				System.out.println(arrayPowerplantIRI[segmentInitialValue[2]+i]);
				publishMessage(arrayPowerplantIRI[segmentInitialValue[2]+i], SCENARIO_ID_3, channel);
			}
			
			if (segmentInitialValue[3]+i < segmentInitialValue[4]) {
//				System.out.println(segmentInitialValue[3] + i);
				System.out.println(arrayPowerplantIRI[segmentInitialValue[3]+i]);
				publishMessage(arrayPowerplantIRI[segmentInitialValue[3]+i], SCENARIO_ID_4, channel);
			}
			
			if (segmentInitialValue[4]+i < numPowerplants) {
//				System.out.println(segmentInitialValue[4] + i);
				System.out.println(arrayPowerplantIRI[segmentInitialValue[4]+i]);
				publishMessage(arrayPowerplantIRI[segmentInitialValue[4]+i], SCENARIO_ID_5, channel);
			}
			
		}
    }

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		HttpSession session = request.getSession();
		session.setMaxInactiveInterval(24*60*60);
		
		String chosenModel = request.getParameter("model");
		System.out.println(chosenModel);
		
		ConnectionFactory factory = new ConnectionFactory();
		factory.setHost("www.theworldavatar.com");
		factory.setPort(83);
		
		try {
			Connection connection = factory.newConnection();
			Channel channel = connection.createChannel();
			
			channel.exchangeDeclare(EXCHANGE_NAME, BuiltinExchangeType.FANOUT);
			
			request.setCharacterEncoding("UTF-8");
			
			String stringArrayOfPowerplantIRI = PythonHelper.callPython("world_powerplants_sparql.py", 
					"", this);
			
			Gson g = new Gson();
			String[] arrayOfPowerplantIRI = g.fromJson(stringArrayOfPowerplantIRI, String[].class);
			
			publishMessages(arrayOfPowerplantIRI, arrayOfPowerplantIRI.length, 5, channel);
			
//			for(int i = 0; i < arrayOfPowerplantIRI.length; i++) {
//				
//				String plant = arrayOfPowerplantIRI[i];
//				Map<String, String> queryParamsJsonObj = new HashMap<String, String>();
//				queryParamsJsonObj.put("plant", plant);
//				
//				String queryParamsString = g.toJson(queryParamsJsonObj);
//				channel.basicPublish(EXCHANGE_NAME, "", null, queryParamsString.getBytes("UTF-8"));
////				TimeUnit.SECONDS.sleep(1);
//				AgentCaller.executeGet(PATH, KEY, queryParamsString);				
//			}
			
			channel.close();
			connection.close();			

		} catch (TimeoutException e) {
			e.printStackTrace();
		} catch (PythonException e) {
			e.printStackTrace();
		} 
//		catch (InterruptedException e) {
//			e.printStackTrace();
//		}
				
	}

}
