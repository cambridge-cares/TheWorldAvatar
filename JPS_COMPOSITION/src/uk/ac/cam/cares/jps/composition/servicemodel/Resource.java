package uk.ac.cam.cares.jps.composition.servicemodel;

import java.net.URI;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

/* This class represent the either an entity or a concept in Ontology
 * All object under such class is constructed by an URI
 */
public class Resource {
	
	public URI uri;
	
	public Resource()
	{
		
	}
	
	public Resource(URI uri)
	{
		this.uri = uri; 
	}
	
	
	public URI getUri() 
	{
		return uri;
	}
	
	public void setUri(URI uri) 
	{
		this.uri = uri;
	}
}
