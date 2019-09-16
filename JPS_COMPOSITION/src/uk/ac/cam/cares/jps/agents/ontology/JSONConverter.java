package uk.ac.cam.cares.jps.agents.ontology;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.json.JSONException;
import org.json.JSONStringer;
import org.json.JSONWriter;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.composition.servicemodel.MessageContent;
import uk.ac.cam.cares.jps.composition.servicemodel.MessagePart;
import uk.ac.cam.cares.jps.composition.servicemodel.Operation;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

public class JSONConverter {
    
    public static String convertToSimplifiedJson(Service service) {
    	
		try {
			JSONWriter writer = new JSONStringer();
			
			String id = service.getUri().toString();
			int i = id.lastIndexOf("/");
			String name = id.substring(i+1);
			i = name.lastIndexOf("__");
			if (i>0) {
				name = name.substring(i+2);
			}
			i = name.lastIndexOf(".");
			if (i>0) {
				name = name.substring(0,i);
			}
			
			String type = service.isComposed()? "composed" : "agent";
			
			addEntry(writer, id, name, type);
			
			for (Operation current : service.getOperations()) {
				String httpUrl = current.getHttpUrl();
				String operationid = current.getUri().toASCIIString();
				addOperation(writer, operationid, httpUrl);
			
				addParameterSection(writer, current, true);
				addParameterSection(writer, current, false);
					
				writer.endObject().endObject();
			}
			
			writer.endArray().endObject();
			
			return writer.toString();
			
		} catch (JSONException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}	 	
    	
    }
    
	private static void addEntry(JSONWriter writer, String id, String name, String type) throws JSONException {
		writer.object();
		writer.key("id").value(id).key("name").value(name).key("type").value(type);
		writer.key("service").array();
	}
	
	private static void addOperation(JSONWriter writer, String id, String httpUrl) throws JSONException {
		writer.object().key("hasOperation").object();
		if (id != null) {
			writer.key("id").value(id);
		}
		writer.key("hasHttpUrl").value(httpUrl);
	}
	
	private static void addParameterSection(JSONWriter writer, Operation operation, boolean input) throws JSONException {
		List<String> params = new ArrayList<String>();
		
		List<MessageContent> messagecontents = null;
		if (input) {
			params.add("hasInput");
			messagecontents = operation.getInputs();
		} else {
			params.add("hasOutput");
			messagecontents = operation.getOutputs();
		}
		
		if (messagecontents.size() > 0) {
			Iterator<MessagePart> it = messagecontents.get(0).getMandatoryParts().iterator();
			while (it.hasNext()) {
				MessagePart current = it.next();
				params.add(current.getName());
				params.add(current.getType().toString());
				params.add(Boolean.toString(current.isArray()));
			}
			String[] array = params.toArray(new String[0]);
			addParameterSection(writer, array);
		}
	}
	
	private static void addParameterSection(JSONWriter writer, String... s) throws JSONException {
		
		writer.key(s[0]).array();
		for (int i=1; i<s.length; i=i+3) {
			writer.object();
			writer.key("hasName").value(s[i]).key("hasType").value(s[i+1]).key("isArray").value(s[i+2]);
			writer.endObject();
		}
		writer.endArray();
	}
}
