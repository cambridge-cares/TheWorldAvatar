package uk.ac.cam.cares.jps.base.rename;

public enum RenameType{
	
	BLAZEGRAPH("blazegraph"),
	FUSEKI("fuseki"),
	RDF4J("rdf4j"),
	OWL_FILE("owl-file");
	
    public String type = null;

	private RenameType(String type) {
        this.type = type;
    }
}