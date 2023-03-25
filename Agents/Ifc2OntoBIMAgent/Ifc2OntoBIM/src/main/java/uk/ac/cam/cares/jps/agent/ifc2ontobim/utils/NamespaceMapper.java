package uk.ac.cam.cares.jps.agent.ifc2ontobim.utils;

import org.apache.jena.arq.querybuilder.AbstractQueryBuilder;

import java.util.HashMap;
import java.util.Map;

/**
 * Provides methods to generate the required namespaces and maps them to their prefixes.
 *
 *  @author qhouyee
 */
public class NamespaceMapper {
    public static final String RDFS_PREFIX = "rdfs";
    public static final String RDFS_NAMESPACE= "http://www.w3.org/2000/01/rdf-schema#";
    public static final String RDF_PREFIX = "rdf";
    public static final String RDF_NAMESPACE= "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    public static final String BIM_PREFIX = "bim";
    public static final String BIM_NAMESPACE = "http://www.theworldavatar.com/kg/ontobim/";
    public static final String BOT_PREFIX = "bot";
    public static final String BOT_NAMESPACE = "https://w3id.org/bot#";
    public static final String OM_PREFIX = "om";
    public static final String OM_NAMESPACE = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    public static final String SKOS_PREFIX = "skos";
    public static final String SKOS_NAMESPACE = "http://www.w3.org/2004/02/skos/core#";
    public static final String IFC_PREFIX = "ifc";
    public static final String IFC_NAMESPACE= "http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#";
    public static final String LIST_PREFIX = "list";
    public static final String LIST_NAMESPACE= "https://w3id.org/list#";
    public static final String EXPRESS_PREFIX = "express";
    public static final String EXPRESS_NAMESPACE= "https://w3id.org/express#";
    public static final String BUILDING_STRUCTURE_PREFIX = "ontobuildingstructure";
    public static final String BUILDING_STRUCTURE_NAMESPACE= "http://www.theworldavatar.com/kg/ontobuildingstructure/";
    private static final String ONTO_BMS_PREFIX = "ontobms";
    private static final String ONTO_BMS_NAMESPACE = "https://www.theworldavatar.com/kg/ontobms/";
    private static final String ONTO_DEVICE_PREFIX = "ontodevice";
    private static final String ONTO_DEVICE_NAMESPACE = "https://www.theworldavatar.com/kg/ontodevice/";
    private static final String ONTO_EMS_PREFIX = "ontoems";
    private static final String ONTO_EMS_NAMESPACE = "https://www.theworldavatar.com/kg/ontoems/";
    private static final String ONTO_LAB_PREFIX = "ontolab";
    private static final String ONTO_LAB_NAMESPACE = "https://www.theworldavatar.com/kg/ontolab/";
    private static final String SAREF_PREFIX = "saref";
    private static final String SAREF_NAMESPACE = "https://saref.etsi.org/core/";
    private static String baseNamespace;
    private static final String basePrefix = "inst";

    /**
     * Get the base namespace.
     */
    public static String getBaseNameSpace() { return baseNamespace; }

    /**
     * Sets the base namespace.
     */
    public static void setBaseNameSpace(String baseIri) {
        baseNamespace = baseIri;
    }

    /**
     * Add the minimum template prefixes for sub-query builders.The superclass of AbstractQueryBuilder allows the
     * namespaces to be generalisable for any sparql query types.
     *
     * @param builder Abstract Query Builder object to add the prefix statements.
     */
    public static void addSubqueryBuilderNamespaces(AbstractQueryBuilder builder) {
        Map<String, String> nsMapping = genNamespaceMapping();
        builder.addPrefixes(nsMapping);
    }

    /**
     * Generates a map of namespaces and their prefixes for additional and duplicate namespaces for templates and sub-queries.
     *
     * @return A map containing the required namespace mappings.
     */
    private static Map<String, String> genNamespaceMapping() {
        Map<String, String> nsMapping = new HashMap<>();
        // Additional namespaces present in OntoBIM
        nsMapping.put(RDFS_PREFIX, RDFS_NAMESPACE);
        nsMapping.put(OM_PREFIX, OM_NAMESPACE);
        nsMapping.put(BIM_PREFIX, BIM_NAMESPACE);
        nsMapping.put(BOT_PREFIX, BOT_NAMESPACE);
        nsMapping.put(SKOS_PREFIX, SKOS_NAMESPACE);
        nsMapping.put(BUILDING_STRUCTURE_PREFIX, BUILDING_STRUCTURE_NAMESPACE);
        nsMapping.put(ONTO_BMS_PREFIX, ONTO_BMS_NAMESPACE);
        nsMapping.put(ONTO_DEVICE_PREFIX, ONTO_DEVICE_NAMESPACE);
        nsMapping.put(ONTO_EMS_PREFIX, ONTO_EMS_NAMESPACE);
        nsMapping.put(ONTO_LAB_PREFIX, ONTO_LAB_NAMESPACE);
        nsMapping.put(SAREF_PREFIX, SAREF_NAMESPACE);
        // Duplicate IfcOwl namespaces for sub-queries - require bim as well
        nsMapping.put(IFC_PREFIX, IFC_NAMESPACE);
        nsMapping.put(LIST_PREFIX, LIST_NAMESPACE);
        nsMapping.put(EXPRESS_PREFIX, EXPRESS_NAMESPACE);
        nsMapping.put(RDF_PREFIX, RDF_NAMESPACE);
        nsMapping.put(basePrefix, baseNamespace);
        return nsMapping;
    }

    /**
     * Retrieves the element's ontoBIM class from its name.
     *
     * @param name The name of the element.
     */
    public static String retrieveClass(String name) {
        String elementName = name.toLowerCase().replaceAll("[\\W\\s\\d]", "");
        if (elementName.contains("electricalmeter") || elementName.contains("electricitymeter")) {
            return ONTO_DEVICE_NAMESPACE + "ElectricityMeter";
        } else if (elementName.contains("watermeter")) {
            return ONTO_DEVICE_NAMESPACE + "WaterMeter";
        } else if (elementName.contains("oilmeter")) {
            return ONTO_DEVICE_NAMESPACE + "OilMeter";
        } else if (elementName.contains("pollutionmeter")) {
            return ONTO_DEVICE_NAMESPACE + "PollutionMeter";
        } else if (elementName.contains("magnetometer")) {
            return ONTO_DEVICE_NAMESPACE + "Magnetometer";
        } else if (elementName.contains("accelerometer")) {
            return ONTO_DEVICE_NAMESPACE + "Accelerometer";
        } else if (elementName.contains("gravitysensor")) {
            return ONTO_DEVICE_NAMESPACE + "Gravity Sensor";
        } else if (elementName.contains("carbondioxidesensor") || elementName.contains("co2sensor")) {
            return ONTO_DEVICE_NAMESPACE + "CO2Sensor";
        } else if (elementName.contains("illuminancesensor")) {
            return ONTO_DEVICE_NAMESPACE + "IlluminanceSensor";
        } else if (elementName.contains("temperaturesensor")) {
            return SAREF_NAMESPACE + "TemperatureSensor";
        } else if (elementName.contains("humiditysensor")) {
            return ONTO_DEVICE_NAMESPACE + "HumiditySensor";
        } else if (elementName.contains("rfidsensor")) {
            return ONTO_DEVICE_NAMESPACE + "RFIDSensor";
        } else if (elementName.contains("smartsensor")) {
            return ONTO_DEVICE_NAMESPACE + "SmartSensor";
        } else if (elementName.contains("pressuresensor")) {
            return ONTO_DEVICE_NAMESPACE + "PressureSensor";
        } else if (elementName.contains("statussensor")) {
            return ONTO_DEVICE_NAMESPACE + "StatusSensor";
        } else if (elementName.contains("flowsensor")) {
            return ONTO_DEVICE_NAMESPACE + "FlowSensor";
        } else if (elementName.contains("feedbacksensor")) {
            return ONTO_DEVICE_NAMESPACE + "FeedbackSensor";
        } else if (elementName.contains("proximitysensor")) {
            return ONTO_DEVICE_NAMESPACE + "ProximitySensor";
        } else if (elementName.contains("fridge")) {
            return ONTO_DEVICE_NAMESPACE + "Fridge";
        } else if (elementName.contains("mobilephone")) {
            return ONTO_DEVICE_NAMESPACE + "MobilePhone";
        } else if (elementName.contains("smartphone")) {
            return ONTO_DEVICE_NAMESPACE + "SmartPhone";
        } else if (elementName.contains("microcontroller")) {
            return ONTO_DEVICE_NAMESPACE + "MicroController";
        } else if (elementName.contains("microphone")) {
            return ONTO_DEVICE_NAMESPACE + "Microphone";
        } else if (elementName.contains("camera")) {
            return ONTO_DEVICE_NAMESPACE + "Camera";
        } else if (elementName.contains("gpsdevice")) {
            return ONTO_DEVICE_NAMESPACE + "GPSDevice";
        } else if (elementName.contains("weatherstation")) {
            return ONTO_EMS_NAMESPACE + "ReportingStation";
        } else if (elementName.contains("solarpanel")) {
            return ONTO_BMS_NAMESPACE + "SolarPanel";
        } else if (elementName.contains("walkinfumehood")) {
            return ONTO_BMS_NAMESPACE + "WalkInFumeHood";
        } else if (elementName.contains("fumehood")) {
            return ONTO_BMS_NAMESPACE + "FumeHood";
        } else if (elementName.contains("chemicalcontainer") || elementName.contains("reagentbottle")
                || elementName.contains("explosiveprecursorbottle")) {
            return ONTO_LAB_NAMESPACE + "ChemicalContainer";
        } else {
            return BOT_NAMESPACE + "Element";
        }
    }
}
