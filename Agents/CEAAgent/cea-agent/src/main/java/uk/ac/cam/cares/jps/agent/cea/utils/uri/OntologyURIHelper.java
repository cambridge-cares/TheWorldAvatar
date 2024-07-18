package uk.ac.cam.cares.jps.agent.cea.utils.uri;

import uk.ac.cam.cares.jps.agent.cea.utils.FileReader;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

public class OntologyURIHelper {
    private static final String PROPERTIES_PATH = "/resources/CEAAgentConfig.properties";
    public static final String ocgml = "ocgml";
    public static final String unitOntology = "unitOntology";
    public static final String ontoUBEMMP = "ontoUBEMMP";
    public static final String rdf = "rdf";
    public static final String owl = "owl";
    public static final String bot = "bot";
    public static final String ontobuiltenv = "ontobuiltenv";
    public static final String ontobuiltstructure = "ontobuiltstructure";
    public static final String ontotimeseries = "ontotimeseries";
    public static final String ontoems = "ontoems";
    public static final String geo = "geo";
    public static final String geof = "geof";
    public static final String epsg = "epsg";
    public static final String bldg = "bldg";
    public static final String grp = "grp";
    public static final String gml = "gml";

    public static final String geoliteral = "geoliteral";
    public static final String geoservice = "geoservice";
    public static Map<String, String> ontologyUriMap = new HashMap<>();

    /**
     * Constructs an OntologyURIHelper object with the given ontology URIs from the properties file
     * @param propertiesFileName name of the properties file
     */
    
    static {
        try (InputStream input = FileReader.getStream(PROPERTIES_PATH)) {
            Properties config = new Properties();
            config.load(input);
            ontologyUriMap.put(ocgml, config.getProperty("uri.ontology.ontocitygml"));
            ontologyUriMap.put(unitOntology, config.getProperty("uri.ontology.om"));
            ontologyUriMap.put(ontoUBEMMP, config.getProperty("uri.ontology.ontoubemmp"));
            ontologyUriMap.put(rdf, config.getProperty("uri.ontology.rdf"));
            ontologyUriMap.put(owl, config.getProperty("uri.ontology.owl"));
            ontologyUriMap.put(bot, config.getProperty("uri.ontology.bot"));
            ontologyUriMap.put(ontobuiltenv, config.getProperty("uri.ontology.ontobuiltenv"));
            ontologyUriMap.put(ontobuiltstructure, config.getProperty("uri.ontology.ontobuiltstructure"));
            ontologyUriMap.put(ontotimeseries, config.getProperty("uri.ontology.ontotimeseries"));
            ontologyUriMap.put(ontoems, config.getProperty("uri.ontology.ontoems"));
            ontologyUriMap.put(geo, config.getProperty("uri.ontology.geo"));
            ontologyUriMap.put(geof, config.getProperty("uri.ontology.geofunction"));
            ontologyUriMap.put(epsg, config.getProperty("uri.opengis.epsg"));
            ontologyUriMap.put(bldg, config.getProperty("uri.ontology.bldg"));
            ontologyUriMap.put(grp, config.getProperty("uri.ontology.grp"));
            ontologyUriMap.put(gml, config.getProperty("uri.ontology.gml"));
            ontologyUriMap.put(geoliteral, config.getProperty("uri.ontology.geoliteral"));
            ontologyUriMap.put(geoservice, config.getProperty("uri.service.geo"));
        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
            throw new JPSRuntimeException("config.properties file not found");
        }
        catch (IOException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Returns the ontology URI
     * @param ontologyName name of the ontology
     * @return ontology URI
     */
    public static String getOntologyUri(String ontologyName) {
        return ontologyUriMap.get(ontologyName);
    }
}
