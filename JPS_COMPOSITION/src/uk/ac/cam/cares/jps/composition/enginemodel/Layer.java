package uk.ac.cam.cares.jps.composition.enginemodel;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import uk.ac.cam.cares.jps.composition.servicemodel.*;
@JsonIgnoreProperties(value = {"outputs","allInputs","allOutputs"})

public class Layer {
	// A layer is an object that consists a set of services, a set of ordered layers makes up a graph.
	/* A layer should contain the following functions:
	 * 
	 */
	private List<Service> services; 
	private List<Resource> outputs; // ignored in serialization 
	private int index; 
	
	public Layer() {
		
	}
	
	public Layer(int index) {
		this.setIndex(index); 
		this.services = new ArrayList<Service>();		
		this.outputs = new ArrayList<Resource>(); // Their outputs will soon be 
	}
	
	public void setServices(ArrayList<Service> services) {
		this.services = services;
	}
	
	public List<Service> getServices(){
		return this.services;
	}
	
	public boolean addService(Service service) {
		if (service != null)
		{
			return this.services.add(service);
		}
		else
		{
			return false;
		}
	}
	
	public boolean removeService(Service service) {
		if (service != null) {
			return this.services.remove(service);
		}
		else
		{
			return false;
		}
	}
	
	
	public List<MessagePart> getAllOutputs() { // ignored in serialization
		
		List<MessagePart> result = new ArrayList<MessagePart>();
		for (int i = 0; i < this.services.size(); i++)
		{
			Service currentService = this.services.get(i); // iterate through all the current services in this layer...
			result.addAll(currentService.getAllOutputs());
		}
		return result;
	}

	
	public List<MessagePart> getAllInputs() { // ignored in serialization
		
		List<MessagePart> result = new ArrayList<MessagePart>();
		for (int i = 0; i < this.services.size(); i++)
		{
			Service currentService = this.services.get(i); // iterate through all the current services in this layer...
			result.addAll(currentService.getAllInputs());
		}
		return result;
	}
	
	public List<Resource> getOutputs() { // ignored in serialization 
		return outputs;
	}

	public void setOutputs(List<Resource> outputs) { // ignored in serialization
		this.outputs = outputs;
	}

	public int getIndex() {
		return index;
	}

	public void setIndex(int index) {
		this.index = index;
	}
}
