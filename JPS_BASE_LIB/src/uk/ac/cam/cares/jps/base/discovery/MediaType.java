package uk.ac.cam.cares.jps.base.discovery;

/**
 * This enum defines media types that are relevant for JPS. They are usually used for
 * HTTP headers "Accept" and "ContentType".<br> 
 * <br>
 * For a complete list of media types, see https://www.iana.org/assignments/media-types/media-types.xhtml
 * 
 * @author Andreas
 *
 */
public enum MediaType {
    TEXT_CSV("text/csv"),
    // IANA only supports "text/turtle" but not "application/x-turtle"
    // see https://www.iana.org/assignments/media-types/media-types.xhtml
    TEXT_TURTLE("text/turtle"),
    APPLICATION_JSON("application/json"),
    APPLICATION_LD_JSON("application/ld+json"),
    APPLICATION_SPARQL("application/sparql-results+json"),
    APPLICATION_SPARQL_RESULTS_XML("application/sparql-results+xml"),
	APPLICATION_RDF_XML("application/rdf+xml"),
	APPLICATION_X_WWW_FOR_URLENCODED("application/x-www-form-urlencoded");

    public String type = null;

    private MediaType(String type) {
        this.type = type;
    }
}
