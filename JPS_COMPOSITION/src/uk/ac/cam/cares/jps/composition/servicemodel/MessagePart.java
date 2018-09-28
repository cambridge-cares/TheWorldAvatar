package uk.ac.cam.cares.jps.composition.servicemodel;

/*
 * Author: ZHOU XIAOCHI 2018-06-18
 */

import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
		this.mandatoryParts = new ArrayList<MessagePart>();

	}
	
	public MessagePart(URI uri) {
		this.uri = uri;
		this.mandatoryParts = new ArrayList<MessagePart>();
        this.optionalParts = new ArrayList<MessagePart>();
        this.objectValue = null;
        this.datatypeValue = null;
       
        this.outputEdges = new ArrayList<Edge>();
        this.inputEdges = new ArrayList<Edge>();
	}
	
    public List<MessagePart> getMandatoryParts() {
        return mandatoryParts;
    }
	
    public List<MessagePart> getOptionalParts() {
        return optionalParts;
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
    
	public Map<String,String> getTypeNamesUnderThisMessagePart(){
		return getNamesUnderAMessagePart(this);
	}
	
	public static Map<String,String> getTypeNamesUnderMessagePart(MessagePart mp){
		return getNamesUnderAMessagePart(mp);
	}
	
	public Map<String, Map<String,String>> getMapOfUpstreamNamesAndDownstreamNames(Service upstreamService) {
		// Get downstream type-name mapping of this messagePart type
		// Get upstream type-name mapping of this messagePart type 
		String downstreamMPType = this.getType().toASCIIString();
		MessagePart upstreamMP = null;
		for(MessagePart mp : upstreamService.getAllOutputs()) {
			String upstreamMPType = mp.getType().toASCIIString();
			if(downstreamMPType.equalsIgnoreCase(upstreamMPType)) {
				upstreamMP = mp;
				break;
			}
		}
		Map<String,String> upstreamTypeToNameMap = getTypeNamesUnderMessagePart(upstreamMP);
		Map<String,String> downstreamTypeToNameMap = getTypeNamesUnderThisMessagePart();
		Map<String,String> upToDownNameToNameMap = new HashMap<String,String>();
		
		// Firstly, make the root key mapping 
		upToDownNameToNameMap.put(upstreamMP.getName(), this.getName());
		
		for (Map.Entry<String, String> upstreamEntry : upstreamTypeToNameMap.entrySet())
		{
			for (Map.Entry<String, String> downstreamEntry : downstreamTypeToNameMap.entrySet())
			{
				if(upstreamEntry.getKey().equalsIgnoreCase(downstreamEntry.getKey())){
					// The type matches, put the upstream name as key and downstream name as value...
					upToDownNameToNameMap.put(upstreamEntry.getValue(), downstreamEntry.getValue());
					break;
				}
			}
		}
		
		Map<String, Map<String,String>> result = new HashMap<String, Map<String,String>>();
		result.put(upstreamMP.getName(), upToDownNameToNameMap);
		return result;
		
	}
	
	
	public static Map<String,String> getNamesUnderAMessagePart(MessagePart messagePart){
		Map<String,String> names = new HashMap<String,String>();
		if(messagePart.getMandatoryParts().size()!= 0) {
			// The messagePart still has children 
			for(MessagePart childPart: messagePart.getMandatoryParts()) {
				names.putAll(getNamesUnderAMessagePart(childPart));
			}
			return names;
		}
		else {
			// The messagePart has no child, therefore, return its names
			names.put(messagePart.getType().toASCIIString(),messagePart.getName());
			return names;
		}
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

	public URI getType() {
		return type;
	}

	public void setType(URI type) {
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
