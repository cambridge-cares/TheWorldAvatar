package uk.ac.cam.cares.jps.composition.ServiceModel;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/*
 * Author ZHOU XIAOCHI 2018-06-18
 * This class 
 */
@JsonIgnoreProperties(value = {"allInputs","allOutputs"})

public class Service{

	public URI uri;
	public String httpUrl;
    public List<Operation> operations;

	public Service() {
		
	}
	
	
	public Service(URI uri) {
		this.setUri(uri);
        this.operations = new ArrayList<Operation>();

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
    	for (Operation currentOperation : this.getOperations()){
    		if(currentOperation.getOutputs()!= null) {
        		for (MessageContent currentMessageContent: currentOperation.getOutputs()) {
        			result.addAll(currentMessageContent.getMandatoryParts());
        		}    			
    		}
    	}
    	return result;
    }
	
    public List<MessagePart> getAllInputs() { // ignored in serialization 
    	List<MessagePart> result = new ArrayList<MessagePart>();
    	for (Operation currentOperation : this.getOperations()){
    		if (currentOperation.getInputs()!=null) {
    	   		for (MessageContent currentMessageContent: currentOperation.getInputs()) {
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
    
}
