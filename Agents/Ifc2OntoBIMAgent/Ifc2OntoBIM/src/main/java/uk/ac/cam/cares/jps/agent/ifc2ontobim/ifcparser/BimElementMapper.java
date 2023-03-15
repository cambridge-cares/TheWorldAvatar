package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.NamespaceMapper;

import java.util.Map;

/**
 * A mapper to determine the class for an element.
 *
 * @author qhouyee
 */
public class BimElementMapper {
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
            return NamespaceMapper.BOT_NAMESPACE + "Element";
        }
    }

    /**
     * Add the prefix and namespace mappings used in this class.
     *
     * @param mapping A hashmap linking the prefix and namespace.
     */
    public static void addPrefixMapping(Map<String, String> mapping) {
        mapping.put(ONTO_BMS_PREFIX, ONTO_BMS_NAMESPACE);
        mapping.put(ONTO_DEVICE_PREFIX, ONTO_DEVICE_NAMESPACE);
        mapping.put(ONTO_EMS_PREFIX, ONTO_EMS_NAMESPACE);
        mapping.put(ONTO_LAB_PREFIX, ONTO_LAB_NAMESPACE);
        mapping.put(SAREF_PREFIX, SAREF_NAMESPACE);
    }
}
