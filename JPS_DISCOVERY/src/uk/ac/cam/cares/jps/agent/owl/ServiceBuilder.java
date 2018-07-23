package uk.ac.cam.cares.jps.agent.owl;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.composition.ServiceModel.MessageContent;
import uk.ac.cam.cares.jps.composition.ServiceModel.MessagePart;
import uk.ac.cam.cares.jps.composition.ServiceModel.Operation;
import uk.ac.cam.cares.jps.composition.ServiceModel.Service;

public class ServiceBuilder {
	
	private Service service = null;
	
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
	
	private Operation getLastOperation() {
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
	
	public ServiceBuilder input(String modelReference, boolean mandatory)  {
		addMessagePart(modelReference, mandatory, true);
		return this;
	}
	
	public ServiceBuilder output(String modelReference, boolean mandatory)  {
		addMessagePart(modelReference, mandatory, false);
		return this;
	}
	
	private void addMessagePart(String modelReference, boolean mandatory, boolean input) {
		
		MessageContent content = getContent(input);
		List<MessagePart> parts = null;
		if (mandatory) {
			parts = content.getMandatoryParts();
			if (parts == null) {
				parts = new ArrayList<MessagePart>();
				content.setMandatoryParts(parts);
			}
		} else {
			parts = content.getOptionalParts();
			if (parts == null) {
				parts = new ArrayList<MessagePart>();
				content.setOptionalParts(parts);
			}
		}
		
		MessagePart newPart = new MessagePart();
		newPart.setModelReference(uri(modelReference));
		parts.add(newPart);
	}
	
	private MessageContent getContent(boolean input) {
		
		List<MessageContent> list = null;
		if (input) {
			list = getLastOperation().getInputs();
		} else {
			list = getLastOperation().getOutputs();
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
	
	public Service build() {
		return service;
	}
}
