package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class BimElementMapperTest {
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
    private static final String TEST_ELECTRICITY_METER = "Electricity meter";
    private static final String TEST_ELECTRICITY_METER_CLASS = ONTO_DEVICE_NAMESPACE + "ElectricityMeter";
    private static final String TEST_OIL_METER = "Oil meter";
    private static final String TEST_OIL_METER_CLASS = ONTO_DEVICE_NAMESPACE + "OilMeter";
    private static final String TEST_TEMP_SENSOR = "Temperature Sensor";
    private static final String TEST_TEMP_SENSOR_CLASS = SAREF_NAMESPACE + "TemperatureSensor";
    private static final String TEST_RFID_SENSOR = "RFID Sensor";
    private static final String TEST_RFID_SENSOR_CLASS = ONTO_DEVICE_NAMESPACE + "RFIDSensor";
    private static final String TEST_WEATHER_STATION = "Weather Station";
    private static final String TEST_WEATHER_STATION_CLASS = ONTO_EMS_NAMESPACE + "ReportingStation";
    private static final String TEST_FUME_HOOD = "Fume hood";
    private static final String TEST_FUME_HOOD_CLASS = ONTO_BMS_NAMESPACE + "FumeHood";
    private static final String TEST_WALK_IN_FUME_HOOD = "Walk in Fumehood";
    private static final String TEST_WALK_IN_FUME_HOOD_CLASS = ONTO_BMS_NAMESPACE + "WalkInFumeHood";
    private static final String TEST_BOTTLES = "Explosive Precursor Bottle";
    private static final String TEST_BOTTLES_CLASS = ONTO_LAB_NAMESPACE + "ChemicalContainer";

    @Test
    void testRetrieveClass() {
        assertEquals(TEST_ELECTRICITY_METER_CLASS, BimElementMapper.retrieveClass(TEST_ELECTRICITY_METER));
        assertEquals(TEST_OIL_METER_CLASS, BimElementMapper.retrieveClass(TEST_OIL_METER));
        assertEquals(TEST_TEMP_SENSOR_CLASS, BimElementMapper.retrieveClass(TEST_TEMP_SENSOR));
        assertEquals(TEST_RFID_SENSOR_CLASS, BimElementMapper.retrieveClass(TEST_RFID_SENSOR));
        assertEquals(TEST_WEATHER_STATION_CLASS, BimElementMapper.retrieveClass(TEST_WEATHER_STATION));
        assertEquals(TEST_FUME_HOOD_CLASS, BimElementMapper.retrieveClass(TEST_FUME_HOOD));
        assertEquals(TEST_WALK_IN_FUME_HOOD_CLASS, BimElementMapper.retrieveClass(TEST_WALK_IN_FUME_HOOD));
        assertEquals(TEST_BOTTLES_CLASS, BimElementMapper.retrieveClass(TEST_BOTTLES));
    }

    @Test
    void testAddPrefixMapping() {
        Map<String, String> sampleMapping = new HashMap<>();
        BimElementMapper.addPrefixMapping(sampleMapping);
        assertEquals(ONTO_BMS_NAMESPACE, sampleMapping.get(ONTO_BMS_PREFIX));
        assertEquals(ONTO_DEVICE_NAMESPACE, sampleMapping.get(ONTO_DEVICE_PREFIX));
        assertEquals(ONTO_EMS_NAMESPACE, sampleMapping.get(ONTO_EMS_PREFIX));
        assertEquals(ONTO_LAB_NAMESPACE, sampleMapping.get(ONTO_LAB_PREFIX));
        assertEquals(SAREF_NAMESPACE, sampleMapping.get(SAREF_PREFIX));
    }
}