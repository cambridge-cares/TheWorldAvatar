package uk.ac.cam.cares.jps.composition.servicemodel;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

public class Operation extends Resource{

	private List<MessageContent> inputs;
	private List<MessageContent> outputs;
	private String httpUrl;
	
	
	public Operation() {
		super();
	}
	
	public Operation(URI uri, String httpUrl) {

		super(uri);
		this.inputs = new ArrayList<MessageContent>();
		this.outputs= new ArrayList<MessageContent>();
		this.httpUrl = httpUrl;
	}
	
    
	public String getHttpUrl() {
		return this.httpUrl;
	}
	
	public void setHttpurl(String httpUrl) {
		this.httpUrl = httpUrl;
	}
	
	public List<MessageContent> getInputs() {
        return inputs;
    }

    public void setInputs(List<MessageContent> inputs) {
        this.inputs = inputs;
    }

    public List<MessageContent> getOutputs() {
        return outputs;
    }

    public void setOutputs(List<MessageContent> outputs) {
        this.outputs = outputs;
    }
	
    public boolean addInput(MessageContent mc) {
        if (mc != null) {
            return this.inputs.add(mc);
        }
        return false;
    }

    public boolean removeInput(MessageContent mc) {
        if (mc != null) {
            return this.inputs.remove(mc);
        }
        return false;
    }

    public boolean addOutput(MessageContent mc) {
        if (mc != null) {
            return this.outputs.add(mc);
        }
        return false;
    }

    public boolean removeOutput(MessageContent mc) {
        if (mc != null) {
            return this.outputs.remove(mc);
        }
        return false;
    }
    
}
