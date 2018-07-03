package uk.ac.cam.cares.jps.composition.Ontology;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.StringWriter;

import com.hp.hpl.jena.datatypes.RDFDatatype;
import com.hp.hpl.jena.datatypes.TypeMapper;
import com.hp.hpl.jena.datatypes.xsd.XSDDatatype;
import com.hp.hpl.jena.rdf.model.*;
import com.hp.hpl.jena.vocabulary.DCTerms;
import com.hp.hpl.jena.vocabulary.OWL2;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.vocabulary.RDFS;

import uk.ac.cam.cares.jps.composition.ServiceModel.MessageContent;
import uk.ac.cam.cares.jps.composition.ServiceModel.MessagePart;
import uk.ac.cam.cares.jps.composition.ServiceModel.Operation;
import uk.ac.cam.cares.jps.composition.ServiceModel.Service;
import uk.ac.cam.cares.jps.composition.Vocabulary.MSM;

public class ServiceWriter {

	public String generateModel(Service service) throws FileNotFoundException {

		Model model = ModelFactory.createDefaultModel();
		Resource current = model.createResource(service.getUri().toASCIIString()); // create the instance of service
																					// in ontology.
		current.addProperty(RDF.type, MSM.Service.Node());

		for (Operation op : service.getOperations()) {
			current.addProperty(MSM.hasOperation.Property(), model.createResource(op.getUri().toASCIIString()));
			addOperationToModel(model, op);

		}
		// Write the result to file
		// FileOutputStream fos = new FileOutputStream("/home/zhouxiaochi/Documents/test.rdf");
		// model.write(fos);
		String syntax = "RDF/XML"; // also try "N-TRIPLE" and "TURTLE"
		StringWriter out = new StringWriter();
		model.write(out, syntax);
		String result = out.toString();
		return result;
	}

	public void addOperationToModel(Model model, Operation operation) {
		Resource current = model.createResource(operation.getUri().toASCIIString());
		current.addProperty(RDF.type, MSM.Operation.Node());
		for (MessageContent input : operation.getInputs()) {
			current.addProperty(MSM.hasInput.Property(), model.createResource(input.uri.toASCIIString()));
			addMessageContent(model, input);
		}

		for (MessageContent output : operation.getOutputs()) {
			current.addProperty(MSM.hasOutput.Property(), model.createResource(output.uri.toASCIIString()));
			addMessageContent(model, output);
		}
	}

	public void addMessageContent(Model model, MessageContent messageContent) {
		Resource current = model.createResource(messageContent.uri.toASCIIString());
		current.addProperty(RDF.type, MSM.MessageContent.Node());
		for (MessagePart part : messageContent.getMandatoryParts()) {
			current.addProperty(MSM.hasMandatoryPart.Property(), model.createResource(part.uri.toASCIIString()));
			addMessagePart(model, part);
		}

		for (MessagePart part : messageContent.getOptionalParts()) {
			current.addProperty(MSM.hasOptionalPart.Property(), model.createResource(part.uri.toASCIIString()));
			addMessagePart(model, part);
		}
	}

	public void addMessagePart(Model model, MessagePart messagePart) {
		Resource current = model.createResource(messagePart.uri.toASCIIString());
		current.addProperty(RDF.type, MSM.MessagePart.Node());
		current.addProperty(MSM.modelReference.Property(),
				model.createResource(messagePart.getModelReference().toASCIIString()));

		current.addLiteral(MSM.hasValue.Property(), messagePart.getValue());
		current.addLiteral(MSM.hasDatatypeValue.Property(), messagePart.getDatatypeValue());

	}

}
