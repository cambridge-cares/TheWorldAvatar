package uk.ac.cam.cares.jps.agents.ontology;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.composition.servicemodel.MessageContent;
import uk.ac.cam.cares.jps.composition.servicemodel.MessagePart;
import uk.ac.cam.cares.jps.composition.servicemodel.Operation;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

public class ServiceBuilder {
	
	private Service service = null;
	private List<MessagePart> currentBranch = new ArrayList<>();
	private MessagePart currentPart = null;
	
	public ServiceBuilder() {
		service = new Service();
	}
	
	private static URI uri(String uri) {
		if (uri == null) {
			return null;
		}
		
		try {
			return new URI(uri);
		} catch (URISyntaxException e) {
			throw new RuntimeException(e.getMessage() + ", uri = " + uri, e);
		}
	}
	
	private Operation getCurrentOperation() {
		int size = service.getOperations().size();
		return service.getOperations().get(size-1);
	}
	
	public ServiceBuilder operation(String uri, String httpUrl) {	
		
		List<Operation> operations = service.getOperations();
		if (operations == null) {
			operations = new ArrayList<Operation>();
			service.setOperations(operations);
		}
		
		Operation operation = new Operation(uri(uri), httpUrl);
		service.getOperations().add(operation);
		return this;
	}	
	
	public ServiceBuilder input(String type, String name)  {
		addMessagePart(type, false, name, true, true);
		return this;
	}
	
	public ServiceBuilder input(String type, boolean array, String name, boolean mandatory)  {
		addMessagePart(type, array, name, mandatory, true);
		return this;
	}
	
	public ServiceBuilder output(String type, String name)  {
		addMessagePart(type, false, name, true, false);
		return this;
	}
	
	public ServiceBuilder output(String type, boolean array, String name, boolean mandatory)  {
		addMessagePart(type, array, name, mandatory, false);
		return this;
	}
	
	private void addMessagePart(String type, boolean array, String name, boolean mandatory, boolean input) {
		
		MessagePart parentPart = null;
		List<MessagePart> parts = null;
		
		if (currentBranch.isEmpty()) {		
			parentPart = getContent(input);
		} else if (!mandatory) {
			throw new JPSRuntimeException("nested parameters must be mandatory");
		} else {
			parentPart = currentBranch.get(currentBranch.size() - 1);
		}
		
		if (mandatory) {
			parts = parentPart.getMandatoryParts();
			if (parts == null) {
				parts = new ArrayList<MessagePart>();
				parentPart.setMandatoryParts(parts);
			}
		} else {
			parts = parentPart.getOptionalParts();
			if (parts == null) {
				parts = new ArrayList<MessagePart>();
				parentPart.setOptionalParts(parts);
			}
		}
			
		currentPart = new MessagePart();
		currentPart.setModelReference(uri(type));
		currentPart.setArray(array);
		currentPart.setName(name);
		parts.add(currentPart);
	}
		
	private MessageContent getContent(boolean input) {
		
		List<MessageContent> list = null;
		if (input) {
			list = getCurrentOperation().getInputs();
		} else {
			list = getCurrentOperation().getOutputs();
		}
		
		MessageContent content = null;
		if (list.isEmpty()) {
			content = new MessageContent();
			list.add(content);
		} else {
			content = list.get(0);
		}
		
		return content;
	}
	
	public ServiceBuilder down() {
		currentBranch.add(currentPart);
		return this;
	}

	public ServiceBuilder up() {
		currentBranch.remove(currentBranch.size() - 1);
		return this;
	}
	
	public Service build() {
		return service;
	}
}
