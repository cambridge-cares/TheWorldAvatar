package uk.ac.cam.cares.jps.base.discovery;

public enum MediaType {
    TEXT_CSV("text/csv"),
    APPLICATION_JSON("application/json"),
    APLICATION_SPARQL("application/sparql-results+json"),
	APPLICATION_RDF_XML("application/rdf+xml");

    public String type = null;

    private MediaType(String type) {
        this.type = type;
    }
}
