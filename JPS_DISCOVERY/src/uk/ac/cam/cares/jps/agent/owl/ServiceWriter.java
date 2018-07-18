package uk.ac.cam.cares.jps.agent.owl;

import java.io.FileNotFoundException;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.UUID;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.vocabulary.RDF;

import uk.ac.cam.cares.jps.composition.ServiceModel.MessageContent;
import uk.ac.cam.cares.jps.composition.ServiceModel.MessagePart;
import uk.ac.cam.cares.jps.composition.ServiceModel.Operation;
import uk.ac.cam.cares.jps.composition.ServiceModel.Service;

public class ServiceWriter {

	public String generateModel(Service service) throws FileNotFoundException {

		Model model = ModelFactory.createDefaultModel();
		
		model.setNsPrefix("msm", MSM.MSM);
		model.setNsPrefix("sawsdl", MSM.SAWSDL);
		model.setNsPrefix("ontoagent", MSM.ONTOAGENT);
		
		// create the instance of service in ontology
		Resource current = model.createResource(getOrCreateUri(service));
		current.addProperty(RDF.type, MSM.Service.Node());

		for (Operation op : service.getOperations()) {
			current.addProperty(MSM.hasOperation.Property(), model.createResource(getOrCreateUri(op)));
			addOperationToModel(model, op);

		}
		// Write the result to file
		// FileOutputStream fos = new
		// FileOutputStream("/home/zhouxiaochi/Documents/test.rdf");
		// model.write(fos);
		String syntax = "RDF/XML"; // also try "N-TRIPLE" and "TURTLE"
		StringWriter out = new StringWriter();
		model.write(out, syntax);
		String result = out.toString();
		return result;
	}

	public void addOperationToModel(Model model, Operation operation) {
		Resource current = model.createResource(getOrCreateUri(operation));
		current.addProperty(RDF.type, MSM.Operation.Node());
		current.addProperty(MSM.hasHttpUrl.Property(), operation.getHttpUrl());
		for (MessageContent input : operation.getInputs()) {
			current.addProperty(MSM.hasInput.Property(), model.createResource(getOrCreateUri(input)));
			addMessageContent(model, input);
		}

		for (MessageContent output : operation.getOutputs()) {
			current.addProperty(MSM.hasOutput.Property(), model.createResource(getOrCreateUri(output)));
			addMessageContent(model, output);
		}
	}

	public void addMessageContent(Model model, MessageContent messageContent) {
		Resource current = model.createResource(getOrCreateUri(messageContent));
		current.addProperty(RDF.type, MSM.MessageContent.Node());

		List<MessagePart> list = messageContent.getMandatoryParts();
		if ((list != null) && (!list.isEmpty())) {
			for (MessagePart part : list) {
				current.addProperty(MSM.hasMandatoryPart.Property(), model.createResource(getOrCreateUri(part)));
				addMessagePart(model, part);
			}
		}

		list = messageContent.getOptionalParts();
		if ((list != null) && (!list.isEmpty())) {
			for (MessagePart part : list) {
				current.addProperty(MSM.hasOptionalPart.Property(), model.createResource(getOrCreateUri(part)));
				addMessagePart(model, part);
			}
		}
	}

	public void addMessagePart(Model model, MessagePart messagePart) {
		Resource current = model.createResource(getOrCreateUri(messagePart));
		current.addProperty(RDF.type, MSM.MessagePart.Node());
		current.addProperty(MSM.modelReference.Property(), messagePart.getModelReference().toASCIIString());

		// TODO-AE maybe move both properties to somewhere else
		if (messagePart.getValue() != null) {
			current.addLiteral(MSM.hasValue.Property(), messagePart.getValue());
		}
		if (messagePart.getDatatypeValue() != null) {
			current.addLiteral(MSM.hasDatatypeValue.Property(), messagePart.getDatatypeValue());
		}
	}

	private String getOrCreateUri(uk.ac.cam.cares.jps.composition.ServiceModel.Resource resource) {
		if (resource.getUri() == null) {
			UUID uuid = UUID.randomUUID();
			// TODO-AE hardcoded URL and Xiaochis URL in GUI must have the same path 
			String name = "http://www.theworldavatar.com/kb/agent/" + resource.getClass().getSimpleName() + "_" + uuid;
			try {
				URI uri = new URI(name);
				resource.setUri(uri);
			} catch (URISyntaxException e) {
				throw new RuntimeException(e.getMessage(), e);
			}
		}

		return resource.getUri().toASCIIString();
	}
}
