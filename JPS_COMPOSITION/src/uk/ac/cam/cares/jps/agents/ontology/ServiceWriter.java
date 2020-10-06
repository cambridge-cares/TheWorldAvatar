package uk.ac.cam.cares.jps.agents.ontology;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.UUID;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.vocabulary.RDF;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.composition.servicemodel.MessageContent;
import uk.ac.cam.cares.jps.composition.servicemodel.MessagePart;
import uk.ac.cam.cares.jps.composition.servicemodel.Operation;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

public class ServiceWriter {

	private String servicePath = null;
	
	public void writeAsOwlFile(Service service, String serviceName, String filePath) throws FileNotFoundException {
		Model model = generateModel(service, serviceName);
		String fileName = servicePath.substring(servicePath.lastIndexOf('/'));
		FileOutputStream fos = new FileOutputStream(filePath + "/" + fileName);
		model.write(fos, "RDF/XML");
	}
	
	public String generateSerializedModel(Service service, String serviceName) { 
		Model model = generateModel(service, serviceName);
		StringWriter out = new StringWriter();
		model.write(out, "RDF/XML");
		return out.toString();
	}
	
	public Model generateModel(Service service, String serviceName) { 

		Model model = ModelFactory.createDefaultModel();	
		model.setNsPrefix("msm", MSM.MSM);
		
		// create the IRI of the Service
		if ((service.getUri() != null)) {
			throw new RuntimeException("service.getUri() must be null but was " + service.getUri());
		}
	
		String name = serviceName;
		if (name == null) {
			name = UUID.randomUUID().toString();
		}
		servicePath = JPSConstants.URI_KB_AGENTS + "/" + service.getClass().getSimpleName() + "_" + name + ".owl";
		
		try {
			URI uri = new URI(this.servicePath + "#Service");
			service.setUri(uri);
		} catch (URISyntaxException e) {
			throw new RuntimeException(e.getMessage(), e);
		}
		
		// create the instance of service in ontology
		Resource current = model.createResource(service.getUri().toASCIIString());
		current.addProperty(RDF.type, MSM.Service.Node());
		current.addProperty(MSM.isComposed.Property(), new Boolean(service.isComposed()).toString());

		for (Operation op : service.getOperations()) {
			op.setUri(getOrCreateUri(op, op.getUri()));
			current.addProperty(MSM.hasOperation.Property(), model.createResource(op.getUri().toASCIIString()));
			addOperationToModel(model, op);

		}

		return model;
	}

	private void addOperationToModel(Model model, Operation operation) {
		operation.setUri(getOrCreateUri(operation, operation.getUri()));
		Resource current = model.createResource(operation.getUri().toASCIIString());
		current.addProperty(RDF.type, MSM.Operation.Node());
		if (operation.getHttpUrl() != null) {
			current.addProperty(MSM.hasHttpUrl.Property(), operation.getHttpUrl());
		}
		for (MessageContent input : operation.getInputs()) {
			input.setUri(getOrCreateUri(input, input.getUri()));
			current.addProperty(MSM.hasInput.Property(), model.createResource(input.getUri().toASCIIString()));
			addMessageContent(model, input);
		}

		for (MessageContent output : operation.getOutputs()) {
			output.setUri(getOrCreateUri(output, output.getUri()));
			current.addProperty(MSM.hasOutput.Property(), model.createResource(output.getUri().toASCIIString()));
			addMessageContent(model, output);
		}
	}

	private void addMessageContent(Model model, MessageContent messageContent) {
		messageContent.setUri(getOrCreateUri(messageContent, messageContent.getUri()));
		Resource current = model.createResource(messageContent.getUri().toASCIIString());
		current.addProperty(RDF.type, MSM.MessageContent.Node());

		List<MessagePart> list = messageContent.getMandatoryParts();
		if ((list != null) && (!list.isEmpty())) {
			for (MessagePart part : list) {
				part.setUri(getOrCreateUri(part, part.getUri()));
				current.addProperty(MSM.hasMandatoryPart.Property(), model.createResource(part.getUri().toASCIIString()));
				addMessagePart(model, part);
			}
		}

		list = messageContent.getOptionalParts();
		if ((list != null) && (!list.isEmpty())) {
			for (MessagePart part : list) {
				part.setUri(getOrCreateUri(part, part.getUri()));
				current.addProperty(MSM.hasOptionalPart.Property(), model.createResource(part.getUri().toASCIIString()));
				addMessagePart(model, part);
			}
		}
	}

	private void addMessagePart(Model model, MessagePart messagePart) {
		messagePart.setUri(getOrCreateUri(messagePart, messagePart.getUri()));
		Resource current = model.createResource(messagePart.getUri().toASCIIString());
		current.addProperty(RDF.type, MSM.MessagePart.Node());
		current.addProperty(MSM.hasType.Property(), messagePart.getType().toASCIIString());
		current.addProperty(MSM.isArray.Property(), new Boolean(messagePart.isArray()).toString());
		current.addProperty(MSM.hasName.Property(), messagePart.getName());
		
		// TODO-AE maybe move both properties to somewhere else
		if (messagePart.getValue() != null) {
			current.addLiteral(MSM.hasObjectValue.Property(), messagePart.getValue());
		}
		if (messagePart.getDatatypeValue() != null) {
			current.addLiteral(MSM.hasDataValue.Property(), messagePart.getDatatypeValue());
		}
		
		// write nested parameters
		if (messagePart.getMandatoryParts() != null) {
			for (MessagePart nestedPart : messagePart.getMandatoryParts()) {
				nestedPart.setUri(getOrCreateUri(nestedPart, nestedPart.getUri()));
				current.addProperty(MSM.hasMandatoryPart.Property(), model.createResource(nestedPart.getUri().toASCIIString()));
				addMessagePart(model, nestedPart);
			}
		}
		
		if (messagePart.getOptionalParts() != null) {
			throw new JPSRuntimeException("nested parameters must be mandatory");
		}
	}
	
	private URI getOrCreateUri(Object resource, URI uri) {
		if (uri == null) {
			UUID uuid = UUID.randomUUID();
			String name = servicePath + "#" + resource.getClass().getSimpleName() + "_" + uuid;
			try {
				return new URI(name);
			} catch (URISyntaxException e) {
				throw new RuntimeException(e.getMessage(), e);
			}
		}

		return uri;
	}
}
