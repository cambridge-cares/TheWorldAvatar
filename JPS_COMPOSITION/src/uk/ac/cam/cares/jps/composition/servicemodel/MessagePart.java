package uk.ac.cam.cares.jps.composition.ServiceModel;

/*
 * Author: ZHOU XIAOCHI 2018-06-18
 */

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

public class MessagePart{

	public  URI uri;
	private List<MessagePart> mandatoryParts;
	private List<MessagePart> optionalParts;
	private String value;
	private String datatypeValue;
	private URI modelReference; 
	
	


	
	/* A messagePart represents an input parameter for a semantic web service
	 * It is currently an extension of class Resource, which requires an IRI(URI) to construct
	 * A messagePart can be merely a 
	 */
	
	public MessagePart()
	{
	}
	
	public MessagePart(URI uri) 
	{
		this.uri = uri;
		this.mandatoryParts = new ArrayList<MessagePart>();
        this.optionalParts = new ArrayList<MessagePart>();
        this.value = null;
        this.datatypeValue = null;
        this.modelReference = null;
	}
	
    public List<MessagePart> getMandatoryParts() {
        return mandatoryParts;
    }
	
    public List<MessagePart> getOptionalParts() {
        return optionalParts;
    }

    public void setModelReference(URI uri)
    {
    	this.modelReference = uri;
    }
    
    public URI getModelReference() {
    	return this.modelReference;
    }
    
    public void setValue(String value) {
    	this.value = value;
    }
    
    public String getValue() {
    	return this.value;
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
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return "Can not serialize this MessagePart Object to JSON";
    }
	
	
}
