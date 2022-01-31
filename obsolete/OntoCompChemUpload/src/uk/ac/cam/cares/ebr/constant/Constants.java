package uk.ac.cam.cares.ebr.constant;

public enum Constants {

	/**
	 * Localhost parameters
	 */
	XSLT_FILE_PATH_LOCAL_HOST("C:/apache-tomcat-8.5.35/conf/Catalina/xslt/gxmltoowl.xsl"),
	XSD_FILE_PATH_LOCAL_HOST("C:/apache-tomcat-8.5.35/conf/Catalina/xml_schema/schema.xsd"),
	DATA_FOLDER_PATH_LOCAL_HOST("C:/apache-tomcat-8.5.35/webapps/ROOT/data/ontocompchem/"),
	KB_FOLDER_PATH_LOCAL_HOST("C:/apache-tomcat-8.5.35/webapps/ROOT/kb/ontocompchem/"),
	APACHE_FOLDER_PATH_LOCAL_HOST("C:/apache-tomcat-8.5.35/"),
	
	/**
	 * Claudius server parameters
	 */
	XSLT_FILE_PATH_CLAUDIUS("C:/TOMCAT/conf/Catalina/xslt/gxmltoowl.xsl"),
	XSD_FILE_PATH_CLAUDIUS("C:/TOMCAT/conf/Catalina/xml_schema/schema.xsd"),
	DATA_FOLDER_PATH_CLAUDIUS("C:/TOMCAT/webapps/ROOT/data/ontocompchem/"),
	KB_FOLDER_PATH_CLAUDIUS("C:/TOMCAT/webapps/ROOT/kb/ontocompchem/"),
	APACHE_FOLDER_PATH_CLAUDIUS("C:/TOMCAT/"),
	
	/**
	 * ontocompchem uri
	 */
	ONTOCOMPCHEM_KB_TBOX_URI("http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#"),
	ONTOCOMPCHEM_KB_ABOX_URI("http://www.theworldavatar.com/kb/ontocompchem/"),
	
	/**
	 * rdf4j server uri
	 */
	ONTOCOMPCHEM_KB_LOCAL_RDF4J_SERVER_URL_CLAUDIUS("http://localhost/rdf4j-server/repositories/ontocompchem"),// http://www.theworldavatar.com/rdf4j-server/repositories/ontocompchem
	ONTOCOMPCHEM_KB_LOCAL_RDF4J_SERVER_URL_LOCAL_HOST("http://localhost:8080/rdf4j-server/repositories/ontocompchem"),
	
	ONTOCOMPCHEM_NS("http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#");
	
	private String constants;

	private Constants(String cons) {
	    this.constants = cons;
	}
	    
	@Override
	public String toString() {
	    return constants;
	}
}
