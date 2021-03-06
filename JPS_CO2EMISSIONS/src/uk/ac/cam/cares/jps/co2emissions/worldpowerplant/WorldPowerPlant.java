package uk.ac.cam.cares.jps.co2emissions.worldpowerplant;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeoutException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.json.JSONException;
import org.json.JSONObject;

import com.google.gson.Gson;
import com.rabbitmq.client.BuiltinExchangeType;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.exception.PythonException;
import uk.ac.cam.cares.jps.base.util.PythonHelper;
/**
 * Servlet implementation class WorldPowerPlant (= loop agent)
 */
@WebServlet("/WorldPowerPlant")


public class WorldPowerPlant extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private static final String EXCHANGE_NAME = "jps.agents";

	private static final Gson g = new Gson();
       
    public WorldPowerPlant() {
        super();
    }
    
    protected void publishMessage(String powerplantIRI, String idScenario, Channel channel, String chosenModel) throws UnsupportedEncodingException, IOException, URISyntaxException {
		Map<String, String> queryParamsJsonObj = new HashMap<String, String>();
		queryParamsJsonObj.put("plant", powerplantIRI);
		
		String agentIRI = null;
		
		if (chosenModel.equals("factor")) {
			// EIP --> one parameter
			JSONObject dataSet = new JSONObject();
			try {
				dataSet.put("plant", powerplantIRI);
			} catch (JSONException e) {
				e.printStackTrace();
				throw new JPSRuntimeException(e.getMessage(), e);
			}

			String json = dataSet.toString();
			String resultjson = AgentCaller.executeGet("/JPS_CO2EMISSIONS/FactorModel", "query", json);
			System.out.println(resultjson);

			agentIRI = "http://www.theworldavatar.com/kb/agents/Service__FactorModel.owl#Service";

		} 
		
		else {
			URIBuilder builder = new URIBuilder().setScheme("http").setHost("localhost:8000")
					.setPath("/run-surrogate-model").setParameter("powerplantIRI", powerplantIRI);

			URI uri = builder.build();
			HttpGet getRequest = new HttpGet(uri);
			HttpClient httpClient = HttpClientBuilder.create().build();
			HttpResponse httpResponse = httpClient.execute(getRequest);

			// Parse response into string
			BufferedReader rd = new BufferedReader(new InputStreamReader(httpResponse.getEntity().getContent()));

			StringBuilder total = new StringBuilder();
			String line = null;

			while ((line = rd.readLine()) != null) {
				total.append(line);
			}
			rd.close();
			String body = total.toString();

			System.out.println(body);

			agentIRI = "http://www.theworldavatar.com/kb/agents/Service__SurrogateModel.owl#Service";
		}
		
		queryParamsJsonObj.put("agent iri", agentIRI);
		queryParamsJsonObj.put("scenario id", idScenario);
		String queryParamsString = g.toJson(queryParamsJsonObj);
		channel.basicPublish(EXCHANGE_NAME, "", null, queryParamsString.getBytes("UTF-8"));
    }
    
    protected void publishMessages(String[] arrayPowerplantIRI, int numPowerplants, int numSegments, Channel channel, String chosenModel) throws UnsupportedEncodingException, IOException, URISyntaxException {
    	int sizeSegment = numPowerplants / numSegments;
    	int remainder = numPowerplants % numSegments;
    	int[] segmentInitialValue = new int[numSegments];
    	Arrays.fill(segmentInitialValue, 0);
    	// all elements have initial value of 0
    	
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
		
		//meaning : if remainder>0, then maxloop = sizeSegment+1 ; else maxloop=sizeSegment
		
		for (int i = 0; i < maxLoop; i++) {
			for (int a=1;a<numSegments;a++)	{
				if (segmentInitialValue[a-1] + i < segmentInitialValue[a]) {
					System.out.println(String.format("idx: %d", segmentInitialValue[a-1] + i));
					publishMessage(arrayPowerplantIRI[segmentInitialValue[a-1]+i], String.valueOf(a), channel, chosenModel);
				}
			}

				if (segmentInitialValue[numSegments-1] + i < numPowerplants) {
					System.out.println(String.format("idx: %d", segmentInitialValue[numSegments-1] + i));
					publishMessage(arrayPowerplantIRI[segmentInitialValue[numSegments-1] + i], String.valueOf(numSegments), channel, chosenModel);
				}
		}
    }

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		HttpSession session = request.getSession();
		session.setMaxInactiveInterval(24*60*60);
		
		String chosenModel = request.getParameter("model");
		System.out.println(chosenModel);
		
		String agentnumber = request.getParameter("agent");
		System.out.println(agentnumber);
		
		String nodenumber = request.getParameter("node");
		System.out.println(nodenumber);
		int numpowerplants=0;

			
		
		ConnectionFactory factory = new ConnectionFactory();
		factory.setHost("www.theworldavatar.com");
		//factory.setPort(83);//change because it is used for bms server port
		factory.setPort(5672);
		
		try {
			Connection connection = factory.newConnection();
			Channel channel = connection.createChannel();			
			channel.exchangeDeclare(EXCHANGE_NAME, BuiltinExchangeType.FANOUT);
			
			request.setCharacterEncoding("UTF-8");
			
			System.out.println("START OF TEST");			
			long startTime = System.currentTimeMillis();
						
			String stringArrayOfPowerplantIRI = PythonHelper.callPython("world_powerplants_sparql.py", 
					"", this);
			
			Gson g = new Gson();
			String[] arrayOfPowerplantIRI = g.fromJson(stringArrayOfPowerplantIRI, String[].class);
		
			if (nodenumber.toLowerCase().contentEquals("all"))
			{
		numpowerplants=arrayOfPowerplantIRI.length;
			}
			else
			{
				numpowerplants=Integer.parseInt(nodenumber);
			}
			
			publishMessages(arrayOfPowerplantIRI, numpowerplants, Integer.parseInt(agentnumber), channel, chosenModel);
			//publishMessages(arrayOfPowerplantIRI, arrayOfPowerplantIRI.length, 5, null, chosenModel);
			//publishMessages(arrayOfPowerplantIRI, 10, 5, channel, chosenModel);
			
			long stopTime = System.currentTimeMillis();
			long elapsedTime = stopTime - startTime;
			System.out.println("the time elapsed is= "+elapsedTime);
			System.out.println("done");
			
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
 catch (URISyntaxException e) {
			e.printStackTrace();
		}
				
	}

}
