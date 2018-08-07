package uk.ac.cam.cares.jps.composition.servicemodel;

import java.net.URI;

public class MessageContent extends MessagePart {
	 
	 // Consider MessageContent as a wrapper of MessagePart, messageContent is not essentially different from MessagePart
	
	public MessageContent()
	{
		super();
	}
	
	public MessageContent(URI uri) {
	        super(uri);
	    }
	 
	 
}
