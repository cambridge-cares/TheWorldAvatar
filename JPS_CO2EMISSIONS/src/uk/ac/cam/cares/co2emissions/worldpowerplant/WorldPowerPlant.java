package uk.ac.cam.cares.co2emissions.worldpowerplant;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

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
       
    public WorldPowerPlant() {
        super();
    }

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
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
			
			
			String path = "/JPS_CO2EMISSIONS/SurrogateModel";
			String key = "query";
			
			for(int i = 0; i < arrayOfPowerplantIRI.length; i++) {
				
				String plant = arrayOfPowerplantIRI[i];
				Map<String, String> queryParamsJsonObj = new HashMap<String, String>();
				queryParamsJsonObj.put("plant", plant);
				
				String queryParamsString = g.toJson(queryParamsJsonObj);
				channel.basicPublish(EXCHANGE_NAME, "", null, queryParamsString.getBytes("UTF-8"));
//				TimeUnit.SECONDS.sleep(1);
				AgentCaller.executeGet(path, key, queryParamsString);				
			}
			
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
