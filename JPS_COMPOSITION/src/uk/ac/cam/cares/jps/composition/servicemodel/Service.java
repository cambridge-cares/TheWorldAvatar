package uk.ac.cam.cares.jps.composition.ServiceModel;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import uk.ac.cam.cares.jps.composition.EngineModel.Edge;

/*
 * Author ZHOU XIAOCHI 2018-06-18
 * This class 
 */
@JsonIgnoreProperties(value = { "allInputs", "allOutputs" })

public class Service {

	public URI uri;
	public String httpUrl;
	public List<Operation> operations;
	public boolean composed = false;

	private List<Edge> outputEdges;
	private List<Edge> inputEdges;

	public boolean moveToNextLayer = false;
	
	public Service() {

	}

	public Service(URI uri) {
		this.setUri(uri);
		this.operations = new ArrayList<Operation>();
		this.setOutputEdges(new ArrayList<Edge>());
		this.setInputEdges(new ArrayList<Edge>());
	}

	public void setHttpUrl(String httpUrl) {
		this.httpUrl = httpUrl;
	}

	public String getHttpUrl() {
		return this.httpUrl;
	}

	public List<Operation> getOperations() {
		return operations;
	}

	public void setOperations(List<Operation> operations) {
		this.operations = operations;
	}

	public boolean addInputEdge(Edge edge) {
		if (edge != null) {
			return this.inputEdges.add(edge);
		}
		return false;
	}

	public boolean addOutputEdge(Edge edge) {
		if (edge != null) {
			return this.outputEdges.add(edge);
		}
		return false;
	}

	public boolean addOperation(Operation op) {
		if (op != null) {
			return this.operations.add(op);
		}
		return false;
	}

	public boolean removeOperation(Operation op) {
		if (op != null) {
			return this.operations.remove(op);
		}
		return false;
	}

	public List<MessagePart> getAllOutputs() { // ignored in serialization
		List<MessagePart> result = new ArrayList<MessagePart>();
		for (Operation currentOperation : this.getOperations()) {
			if (currentOperation.getOutputs() != null) {
				for (MessageContent currentMessageContent : currentOperation.getOutputs()) {
					result.addAll(currentMessageContent.getMandatoryParts());
				}
			}
		}
		return result;
	}

	public List<MessagePart> getAllInputs() { // ignored in serialization
		List<MessagePart> result = new ArrayList<MessagePart>();
		for (Operation currentOperation : this.getOperations()) {
			if (currentOperation.getInputs() != null) {
				for (MessageContent currentMessageContent : currentOperation.getInputs()) {
					result.addAll(currentMessageContent.getMandatoryParts());
				}
			}
		}
		return result;
	}
	 

	
	

	public URI getUri() {
		return uri;
	}

	public void setUri(URI uri) {
		this.uri = uri;
	}

	public List<Edge> getOutputEdges() {
		return outputEdges;
	}

	public void setOutputEdges(List<Edge> outputEdges) {
		this.outputEdges = outputEdges;
	}

	public List<Edge> getInputEdges() {
		return inputEdges;
	}

	public void setInputEdges(List<Edge> inputEdges) {
		this.inputEdges = inputEdges;
	}

	public boolean isComposed() {
		return composed;
	}

	public void setComposed(boolean composed) {
		this.composed = composed;
	}
}
