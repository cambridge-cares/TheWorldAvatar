package uk.ac.cam.cares.jps.agents.discovery;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.ac.cam.cares.jps.agents.ontology.ServiceReader;
import uk.ac.cam.cares.jps.composition.servicemodel.MessagePart;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

public class ServiceDiscoveryTest {

	public ArrayList<Service> services;
	public Map<String,Service> httpToServiceMap;
	
	public ServiceDiscoveryTest() throws Exception {
		
		this.services = new ArrayList<Service>();
		this.httpToServiceMap = new HashMap<String,Service>();
		this.loadServices();
		this.generateHttpToServiceMap();
	}	
	
	public ArrayList<Service> getAllServiceCandidates(List<MessagePart> inputs, ArrayList<Service> servicePool){
		
		  
		ArrayList<Service> result = new ArrayList<Service>();
		ArrayList<URI> inputTypesList = new ArrayList<URI>();
		for (MessagePart messagePart_inputs : inputs) {
			inputTypesList.add(messagePart_inputs.getType());
		}

		for (Service currentService : this.services) {
			boolean flag = true;
			for (MessagePart messagePart : currentService.getAllInputs()) {
				URI type = messagePart.getType();
				if (!inputTypesList.contains(type)) {
					flag = false;
				}
			}
			if (flag && !(servicePool.contains(currentService))) {
				result.add(currentService);
			}
		}
		return result;
	}
	
	public void loadServices() throws Exception {
		ServiceReader serviceReader = new ServiceReader();
		String CityToWeatherInString = readTheServicePool("Service_CityToWeather.owl");
		String WeatherToDirectionInString = readTheServicePool("Service_WeatherToWindDirection.owl");
		Service cityToWeatherAgent = serviceReader.parse(new ByteArrayInputStream(CityToWeatherInString.getBytes(Charset.forName("UTF-8"))), "http://www.theworldavatar.com").get(0);
		Service weatherToDirection = serviceReader.parse(new ByteArrayInputStream(WeatherToDirectionInString.getBytes(Charset.forName("UTF-8"))), "http://www.theworldavatar.com").get(0);
		this.services.add(cityToWeatherAgent);
		this.services.add(weatherToDirection);
	}
	
	public void generateHttpToServiceMap() {
		for(Service s : this.services) {this.httpToServiceMap.put(s.getOperations().get(0).getHttpUrl(),s);}
	}
	
	public Service getServiceFromHttpUrl(String url) {
		return this.httpToServiceMap.get(url);
	}
	
	public static String readTheServicePool(String filename) throws Exception {

 		String directory = "C:\\Users\\nasac\\Documents\\GIT\\JPS_COMPOSITION\\testres\\serviceowlfiles";
 		String wholeContent = "";
		try (BufferedReader br = new BufferedReader(new FileReader(directory + "\\" + filename))) {
			String sCurrentLine;
			while ((sCurrentLine = br.readLine()) != null) {
				wholeContent = wholeContent + sCurrentLine;
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		if (wholeContent == null || wholeContent == "") {
			return null;
		} else {
			return wholeContent;
		}
	}
	
}
