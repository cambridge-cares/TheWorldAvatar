package uk.ac.cam.cares.jps.agent.ifc2ontobim.utils;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.junit.jupiter.api.Test;


import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class NamespaceMapperTest {
    private static final String ONTO_BMS_NAMESPACE = "https://www.theworldavatar.com/kg/ontobms/";
    private static final String ONTO_DEVICE_NAMESPACE = "https://www.theworldavatar.com/kg/ontodevice/";
    private static final String ONTO_EMS_NAMESPACE = "https://www.theworldavatar.com/kg/ontoems/";
    private static final String ONTO_LAB_NAMESPACE = "https://www.theworldavatar.com/kg/ontolab/";
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
        assertEquals(TEST_ELECTRICITY_METER_CLASS, NamespaceMapper.retrieveClass(TEST_ELECTRICITY_METER));
        assertEquals(TEST_OIL_METER_CLASS, NamespaceMapper.retrieveClass(TEST_OIL_METER));
        assertEquals(TEST_TEMP_SENSOR_CLASS, NamespaceMapper.retrieveClass(TEST_TEMP_SENSOR));
        assertEquals(TEST_RFID_SENSOR_CLASS, NamespaceMapper.retrieveClass(TEST_RFID_SENSOR));
        assertEquals(TEST_WEATHER_STATION_CLASS, NamespaceMapper.retrieveClass(TEST_WEATHER_STATION));
        assertEquals(TEST_FUME_HOOD_CLASS, NamespaceMapper.retrieveClass(TEST_FUME_HOOD));
        assertEquals(TEST_WALK_IN_FUME_HOOD_CLASS, NamespaceMapper.retrieveClass(TEST_WALK_IN_FUME_HOOD));
        assertEquals(TEST_BOTTLES_CLASS, NamespaceMapper.retrieveClass(TEST_BOTTLES));
    }

    @Test
    void testAddSubqueryBuilderNamespaces() {
        ConstructBuilder builder = new ConstructBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(builder);

        String namespaces = builder.buildString();
        List<String> expected = genExpectedResultList();
        expected.forEach(line -> assertTrue(namespaces.contains(line)));
    }


    private static List<String> genExpectedResultList() {
        List<String> results = new ArrayList<>();
        results.add("rdf:");
        results.add("bot:");
        results.add("bim:");
        results.add("ifc:");
        results.add("rdfs:");
        results.add("express:");
        results.add("list:");
        results.add("om:");
        results.add("ontobuildingstructure:");
        results.add("ontobms:");
        results.add("ontodevice:");
        results.add("ontoems:");
        results.add("ontolab:");
        results.add("saref:");
        results.add("<http://www.w3.org/1999/02/22-rdf-syntax-ns#>");
        results.add("<https://w3id.org/bot#>");
        results.add("<https://www.theworldavatar.com/kg/ontobim/>");
        results.add("<http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#>");
        results.add("<https://www.w3.org/2000/01/rdf-schema#>");
        results.add("<https://w3id.org/express#>");
        results.add("<https://w3id.org/list#>");
        results.add("<https://www.ontology-of-units-of-measure.org/resource/om-2/>");
        results.add("<https://www.theworldavatar.com/kg/ontobuildingstructure/>");
        results.add("<https://www.theworldavatar.com/kg/ontobms/>");
        results.add("<https://www.theworldavatar.com/kg/ontodevice/>");
        results.add("<https://www.theworldavatar.com/kg/ontoems/>");
        results.add("<https://www.theworldavatar.com/kg/ontolab/>");
        results.add("<https://saref.etsi.org/core/>");
        return results;
    }
}