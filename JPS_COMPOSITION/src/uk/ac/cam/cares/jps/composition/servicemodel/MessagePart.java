package uk.ac.cam.cares.jps.composition.servicemodel;

/*
 * Author: ZHOU XIAOCHI 2018-06-18
 */

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import uk.ac.cam.cares.jps.composition.enginemodel.Edge;

public class MessagePart{

	public  URI uri;
	private List<MessagePart> mandatoryParts;
	private List<MessagePart> optionalParts;
	// TODO-AE URGENT rename to objectValue with type URI
	private URI objectValue;
	// TODO-AE URGENT rename to dataValue
	private String datatypeValue;
	private URI modelReference;
	private URI type; 
	public String name;
	private boolean array = false;
	// TODO-AE URGENT remove output and input edges from here and other MSM classes
	public ArrayList<Edge> outputEdges;
	public ArrayList<Edge> inputEdges;


	
	/* A messagePart represents an input parameter for a semantic web service
	 * It is currently an extension of class Resource, which requires an IRI(URI) to construct
	 * A messagePart can be merely a 
	 */
	
	public MessagePart() {
        this.outputEdges = new ArrayList<Edge>();
        this.inputEdges = new ArrayList<Edge>();
	}
	
	public MessagePart(URI uri) {
		this.uri = uri;
		this.mandatoryParts = new ArrayList<MessagePart>();
        this.optionalParts = new ArrayList<MessagePart>();
        this.objectValue = null;
        this.datatypeValue = null;
        this.modelReference = null;
       
        this.outputEdges = new ArrayList<Edge>();
        this.inputEdges = new ArrayList<Edge>();
	}
	
    public List<MessagePart> getMandatoryParts() {
        return mandatoryParts;
    }
	
    public List<MessagePart> getOptionalParts() {
        return optionalParts;
    }

    @Deprecated
    public void setModelReference(URI uri) {
    	this.modelReference = uri;
    }
    
    @Deprecated
    public URI getModelReference() {
    	return this.modelReference;
    }
    
    public void setValue(URI value) {
    	this.objectValue = value;
    }
    
    public URI getValue() {
    	return this.objectValue;
    }
    
    public void setDatatypeValue(String dataTypeValue) {
    	this.datatypeValue = dataTypeValue;
    }
    
    public String getDatatypeValue()
    {
    	return this.datatypeValue;
    }
    
    public void setMandatoryParts(List<MessagePart> mandatoryParts) {
        this.mandatoryParts = mandatoryParts;
    }

    
    public void setOptionalParts(List<MessagePart> optionalParts) {
        this.optionalParts = optionalParts;
    }


    public boolean addOptionalPart(MessagePart part) {
        if (part != null) {
            return this.optionalParts.add(part);
        }
        return false;
    }

    public boolean removeOptionalPart(MessagePart part) {
        if (part != null) {
            return this.optionalParts.remove(part);
        }
        return false;
    }

    public boolean addMandatoryPart(MessagePart part) {
        if (part != null) {
            return this.mandatoryParts.add(part);
        }
        return false;
    }

    public boolean removeMandatoryPart(MessagePart part) {
        if (part != null) {
            return this.mandatoryParts.remove(part);
        }
        return false;
    }
    
    public String toString() {
    	
		ObjectMapper mapper = new ObjectMapper();
		mapper.setSerializationInclusion(Include.NON_NULL);
		mapper.setSerializationInclusion(Include.NON_EMPTY);
    	try {
			return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(this);
		} catch (JsonProcessingException e) {
			e.printStackTrace();
		}
		return "Can not serialize this MessagePart Object to JSON";
    }
	
	public URI getUri() {
		return uri;
	}

	public void setUri(URI uri) {
		this.uri = uri;
	}

	private URI getType() {
		return type;
	}

	private void setType(URI type) {
		this.type = type;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public boolean isArray() {
		return array;
	}

	public void setArray(boolean array) {
		this.array = array;
	}
}
