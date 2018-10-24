package uk.ac.cam.cares.jps.agents.discovery;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.agents.ontology.ServiceReader;
import uk.ac.cam.cares.jps.base.config.KeyValueServer;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.composition.servicemodel.MessagePart;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

public class ServiceDiscovery {
	
	public static final String KEY_DIR_KB_AGENTS = "absdir.jpsdata.knowledgebase.agents";
	private static ServiceDiscovery instance = null;
	Logger logger = LoggerFactory.getLogger(ServiceDiscovery.class);

	
	public ArrayList<Service> services;
	public Map<String,Service> httpToServiceMap;
	
	public static synchronized ServiceDiscovery getInstance() {
		if (instance == null) {
			instance = new ServiceDiscovery();
		}
		return instance;
	}
	
	private ServiceDiscovery() {
		init();
	}
	
	private synchronized void init() {	
		this.services = new ArrayList<Service>();
		this.httpToServiceMap = new HashMap<String,Service>();
		String directory = KeyValueServer.get(KEY_DIR_KB_AGENTS);
		this.loadServices(directory);
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
	
	private void loadServices(String directory) {
		 this.services = readTheServicePool(directory);
	}
	
	public void generateHttpToServiceMap() {
		for(Service s : this.services) {this.httpToServiceMap.put(s.getOperations().get(0).getHttpUrl(),s);}
	}
	
	public Service getServiceFromHttpUrl(String url) {
		return this.httpToServiceMap.get(url);
	}
	
	private ArrayList<Service> readTheServicePool(String directory) {
		
		logger.info("loading from directory=" + directory);
		
		ServiceReader reader = new ServiceReader();
 		ArrayList<Service> servicesLoaded = new ArrayList<Service>();
 		File[] files = new File(directory).listFiles();
 		
 		for(File file : files) {
 			
 			if(file.getName().endsWith("owl")) {
 	 	 		String wholeContent = "";
 	 			try (BufferedReader br = new BufferedReader(new FileReader(file.getAbsoluteFile()))) {
 	 				String sCurrentLine;
 	 				while ((sCurrentLine = br.readLine()) != null) {
 	 					wholeContent = wholeContent + sCurrentLine;
 	 				}
 	 			} catch (IOException e) {
 	 				e.printStackTrace();
 	 			}
 	 			List<Service> services;
				try {
					services = reader.parse(wholeContent, "http://www.theworldavatar.com");
				} catch (URISyntaxException e) {
					throw new JPSRuntimeException(e.getMessage(), e);
				}
 	 			servicesLoaded.addAll(services);
 			}
 		}
		return servicesLoaded;	 
	}
	
}
