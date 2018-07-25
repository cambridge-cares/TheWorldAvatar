package uk.ac.cam.cares.jps.composition.WebServer;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.json.JSONException;
import org.json.JSONObject;


import uk.ac.cam.cares.jps.composition.*;
import uk.ac.cam.cares.jps.composition.ServiceModel.MessagePart;
import uk.ac.cam.cares.jps.composition.ServiceModel.Service;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;
import uk.ac.cam.cares.jps.composition.util.ServicePoolTool;

public class ServiceDiscovery {
	
	private ArrayList<Service> service_pool;  
	public ServiceDiscovery() throws JSONException, IOException, URISyntaxException {
		this.service_pool = new ArrayList<Service>();
		loadServicePool();
	}
	
	public void loadServicePool() throws JSONException, IOException, URISyntaxException {
		JSONObject service_pool_in_JSON = ServicePoolTool.readTheServicePool();
		for (String key : JSONObject.getNames(service_pool_in_JSON))
		{
			JSONObject service_in_JSON = (JSONObject) service_pool_in_JSON.get(key);
			Service service_in_Java = FormatTranslator.convertJSONTOJavaClass(service_in_JSON.toString());
			this.service_pool.add(service_in_Java);
		}
	}
	
	public ArrayList<Service> getAllServiceCandidates(List<MessagePart> inputs, ArrayList<Service> servicePool){
		ArrayList<Service> result = new ArrayList<Service>();
		ArrayList<URI> inputs_modelReferences= new ArrayList<URI>();
		for (MessagePart messagePart_inputs: inputs) {
			inputs_modelReferences.add(messagePart_inputs.getModelReference());
		}
		
		for(Service currentService: this.service_pool) {
			boolean flag = true;
				for (MessagePart messagePart : currentService.getAllInputs()){
				URI modelReference = messagePart.getModelReference();
				if(!inputs_modelReferences.contains(modelReference)) {flag = false;}
			}
			if(flag && !(servicePool.contains(currentService))) {
				result.add(currentService);
			}
		} 
		return result;
	}
	
	
}
