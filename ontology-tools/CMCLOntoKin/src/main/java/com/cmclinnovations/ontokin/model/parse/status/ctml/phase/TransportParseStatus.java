package com.cmclinnovations.ontokin.model.parse.status.ctml.phase;

/**
 * This class contains the getters and setters to the flags that maintain
 * whether or not the transport tag and its attribute have already been parsed.
 * 
 * @author msff2
 *
 */
public class TransportParseStatus {
	boolean transport = false;
	boolean transportModel = false;
	public boolean isTransport() {
		return transport;
	}
	public void setTransport(boolean transport) {
		this.transport = transport;
	}
	public boolean isTransportModel() {
		return transportModel;
	}
	public void setTransportModel(boolean transportModel) {
		this.transportModel = transportModel;
	}
}
