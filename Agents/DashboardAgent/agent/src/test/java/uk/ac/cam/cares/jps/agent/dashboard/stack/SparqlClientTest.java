package uk.ac.cam.cares.jps.agent.dashboard.stack;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.IntegrationTestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.datamodel.OrganisationTest;
import uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.utils.SparqlQueryTest;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.Map;
import java.util.Queue;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class SparqlClientTest {
    private static final String SAMPLE_ORGANISATION_NAME = "Org 1";
    private static final String SAMPLE_BUILDING_INSTANCE = TestUtils.genInstance("Building");
    private static final String SAMPLE_OFFICE_INSTANCE = TestUtils.genInstance("Office");
    public static final String SAMPLE_OFFICE_NAME = "Admin Office";
    private static final String SAMPLE_OFFICE_DIRECTOR_ROOM_INSTANCE = TestUtils.genInstance("Room");
    private static final String SAMPLE_OFFICE_DIRECTOR_ROOM_NAME = "Director's room";
    private static final String SAMPLE_OFFICE_STAFF_ROOM_INSTANCE = TestUtils.genInstance("Room");
    public static final String SAMPLE_OFFICE_STAFF_ROOM_NAME = "Staff room";
    private static final String SAMPLE_OFFICE_STORAGE_ROOM_INSTANCE = TestUtils.genInstance("Room");
    private static final String SAMPLE_OFFICE_STORAGE_ROOM_NAME = "Staff room";
    private static final String SAMPLE_OFFICE_SYSTEM_INSTANCE = TestUtils.genInstance("HVAC");
    public static final String SAMPLE_OFFICE_SYSTEM_NAME = "HVAC system";
    private static final String SAMPLE_OFFICE_SUB_SYSTEM_INSTANCE = TestUtils.genInstance("BTU");
    private static final String SAMPLE_OFFICE_SUB_SYSTEM_NAME = "BTU unit";
    public static final String TEMPERATURE = "Temperature";
    public static final String TEMPERATURE_UNIT = "degree";
    public static final String RELATIVE_HUMIDITY = "Relative Humidity";
    private static final double TEMPERATURE_MIN_THRESHOLD = 21.1;
    private static final double TEMPERATURE_MAX_THRESHOLD = 25.6;
    public static final double RELATIVE_HUMIDITY_MIN_THRESHOLD = 50.1;
    public static final double RELATIVE_HUMIDITY_MAX_THRESHOLD = 75.7;
    private static final String SAMPLE_LAB_INSTANCE = TestUtils.genInstance("Laboratory");
    public static final String SAMPLE_LAB_NAME = "Generic Laboratory";
    private static final String SAMPLE_LAB_BIO_ROOM_INSTANCE = TestUtils.genInstance("Room");
    private static final String SAMPLE_LAB_PILOT_ROOM_INSTANCE = TestUtils.genInstance("Room");
    private static final String SAMPLE_LAB_FRIDGE_NAME = "Chemical Cooling Unit";
    private static final String SAMPLE_LAB_FRIDGE_TYPE = "Fridge";
    private static final String SAMPLE_LAB_FRIDGE_INSTANCE = TestUtils.genInstance(SAMPLE_LAB_FRIDGE_TYPE);
    public static final String SAMPLE_LAB_SMART_SENSOR_NAME = "Sample Sensor Kit";
    public static final String SAMPLE_LAB_SMART_SENSOR_TYPE = "SmartSensor";
    private static final String SAMPLE_LAB_SMART_SENSOR_INSTANCE = TestUtils.genInstance(SAMPLE_LAB_SMART_SENSOR_TYPE);
    private static final String SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_ONE_INSTANCE = TestUtils.genInstance("TemperatureSensor");
    private static final String SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_TWO_INSTANCE = TestUtils.genInstance("StatusSensor");
    private static final String SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_THREE_INSTANCE = TestUtils.genInstance("ElectricitySensor");
    private static final String SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_FOUR_INSTANCE = TestUtils.genInstance("HumiditySensor");
    private static final String SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_FIVE_INSTANCE = TestUtils.genInstance("StatusSensor");
    private static final String SAMPLE_LAB_MAKE_UP_AIR_UNIT_NAME = "MAU-03";
    private static final String SAMPLE_LAB_MAKE_UP_AIR_UNIT_TYPE = "MakeUpAirUnit";
    private static final String SAMPLE_LAB_MAKE_UP_AIR_UNIT_INSTANCE = TestUtils.genInstance(SAMPLE_LAB_MAKE_UP_AIR_UNIT_TYPE);
    private static final String SAMPLE_LAB_MAKE_UP_AIR_UNIT_DUCT_INSTANCE = TestUtils.genInstance("Duct");
    private static final String SAMPLE_LAB_MAKE_UP_AIR_UNIT_DUCT_NAME = "Duct-5";
    private static final String SAMPLE_LAB_MAKE_UP_AIR_UNIT_TEMPERATURE_SENSOR_INSTANCE = TestUtils.genInstance("TemperatureSensor");
    private static final String STATE = "State";
    private static final String HUMIDITY_STATE = "Humidity State";
    public static final String ELECTRICITY_CONSUMPTION = "ElectricityConsumption";
    public static final String ELECTRICITY_CONSUMPTION_UNIT = "kwh";

    @BeforeAll
    static void setupNamespaces() {
        IntegrationTestUtils.createNamespace(IntegrationTestUtils.SPATIAL_ZONE_NAMESPACE);
        IntegrationTestUtils.createNamespace(IntegrationTestUtils.GENERAL_NAMESPACE);
    }

    @AfterEach
    void clearNamespace() {
        IntegrationTestUtils.updateEndpoint(IntegrationTestUtils.GENERAL_SPARQL_ENDPOINT, IntegrationTestUtils.SPARQL_DELETE);
        IntegrationTestUtils.updateEndpoint(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT, IntegrationTestUtils.SPARQL_DELETE);
    }


    @Test
    void testGetAllOrganisations_NoMeasures() {
        // Create a new Sparql client - note there should be no username and password for the test container
        SparqlClient client = new SparqlClient(IntegrationTestUtils.SPARQL_ENDPOINT, "", "");
        // Execute method
        String[] results = client.getAllOrganisations();
        // Verify if no organisation is retrieved if there are no related triples
        assertEquals(0, results.length);
    }

    @Test
    void testGetAllOrganisations_AllMeasures() {
        // Insert these triples into the blazegraph
        insertFacilityTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        insertAssetTriples(IntegrationTestUtils.GENERAL_SPARQL_ENDPOINT, true);
        // Create a new Sparql client - note there should be no username and password for the test container
        SparqlClient client = new SparqlClient(IntegrationTestUtils.SPARQL_ENDPOINT, "", "");
        // Execute method
        String[] results = client.getAllOrganisations();
        // Verify if result is as expected
        assertEquals(1, results.length); // There should only be one organisation
        assertEquals(SAMPLE_ORGANISATION_NAME, results[0]);
    }

    @Test
    void testGetAllOrganisations_OnlyRoomMeasures() {
        // Insert these triples into the blazegraph
        insertFacilityTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        // Create a new Sparql client - note there should be no username and password for the test container
        SparqlClient client = new SparqlClient(IntegrationTestUtils.SPARQL_ENDPOINT, "", "");
        // Execute method
        String[] results = client.getAllOrganisations();
        // Verify if result is as expected
        assertEquals(1, results.length); // There should only be one organisation
        assertEquals(SAMPLE_ORGANISATION_NAME, results[0]);
    }

    @Test
    void testGetAllOrganisations_OnlyRoomAndSystemMeasures() {
        // Insert these triples into the blazegraph
        insertFacilityTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        insertSystemTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        // Create a new Sparql client - note there should be no username and password for the test container
        SparqlClient client = new SparqlClient(IntegrationTestUtils.SPARQL_ENDPOINT, "", "");
        // Execute method
        String[] results = client.getAllOrganisations();
        // Verify if result is as expected
        assertEquals(1, results.length); // There should only be one organisation
        assertEquals(SAMPLE_ORGANISATION_NAME, results[0]);
    }

    @Test
    void testGetAllSpatialZoneMetaData_AssetMeasures() {
        // Insert these triples into the blazegraph
        insertFacilityTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        insertAssetTriples(IntegrationTestUtils.GENERAL_SPARQL_ENDPOINT, true);
        // Create a new Sparql client - note there should be no username and password for the test container
        SparqlClient client = new SparqlClient(IntegrationTestUtils.SPARQL_ENDPOINT, "", "");
        // Execute method
        Map<String, Queue<String[]>> results = client.getAllSpatialZoneMetaData(SAMPLE_ORGANISATION_NAME);
        // Verify if result is as expected
        assertEquals(7, results.size()); // Three assets, two rooms, one threshold and one facility should be returned
        OrganisationTest.verifyFacilityQueueResults(results, new String[]{SAMPLE_OFFICE_NAME, SAMPLE_LAB_NAME}, new String[]{SAMPLE_OFFICE_NAME, SAMPLE_OFFICE_DIRECTOR_ROOM_NAME, SAMPLE_OFFICE_STAFF_ROOM_NAME}, new String[]{SAMPLE_LAB_NAME, SAMPLE_LAB_SMART_SENSOR_NAME, SAMPLE_LAB_FRIDGE_NAME, SAMPLE_LAB_MAKE_UP_AIR_UNIT_NAME});
        // For smart sensor
        Queue<String[]> resultQueue = results.get(SAMPLE_LAB_SMART_SENSOR_NAME);
        assertEquals(5, resultQueue.size()); // Five measures should be available
        while (!resultQueue.isEmpty()) {
            String[] metadata = resultQueue.poll();
            if (metadata[0].equals(TEMPERATURE))
                verifyMetadataContents(metadata, TEMPERATURE, TEMPERATURE_UNIT, SAMPLE_LAB_SMART_SENSOR_TYPE);
            if (metadata[0].equals(RELATIVE_HUMIDITY))
                verifyMetadataContents(metadata, RELATIVE_HUMIDITY, null, SAMPLE_LAB_SMART_SENSOR_TYPE);
            if (metadata[0].equals(ELECTRICITY_CONSUMPTION))
                verifyMetadataContents(metadata, ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT, SAMPLE_LAB_SMART_SENSOR_TYPE);
            if (metadata[0].equals(STATE)) verifyMetadataContents(metadata, STATE, null, SAMPLE_LAB_SMART_SENSOR_TYPE);
            if (metadata[0].equals(HUMIDITY_STATE))
                verifyMetadataContents(metadata, HUMIDITY_STATE, null, SAMPLE_LAB_SMART_SENSOR_TYPE);
        }
        // For make up air unit, only one measure should be available
        resultQueue = results.get(SAMPLE_LAB_MAKE_UP_AIR_UNIT_NAME + " " + SAMPLE_LAB_MAKE_UP_AIR_UNIT_DUCT_NAME);
        assertEquals(1, resultQueue.size());
        verifyMetadataContents(resultQueue.poll(), TEMPERATURE, TEMPERATURE_UNIT, SAMPLE_LAB_MAKE_UP_AIR_UNIT_TYPE);
        // For fridge, only one measure should be available
        resultQueue = results.get(SAMPLE_LAB_FRIDGE_NAME);
        assertEquals(1, resultQueue.size());
        verifyMetadataContents(resultQueue.poll(), ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT, SAMPLE_LAB_FRIDGE_TYPE);
    }

    @Test
    void testGetAllSpatialZoneMetaData_RoomAndSystemMeasures() {
        // Insert these triples into the blazegraph
        insertFacilityTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        insertSystemTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        // Create a new Sparql client - note there should be no username and password for the test container
        SparqlClient client = new SparqlClient(IntegrationTestUtils.SPARQL_ENDPOINT, "", "");
        // Execute method
        Map<String, Queue<String[]>> results = client.getAllSpatialZoneMetaData(SAMPLE_ORGANISATION_NAME);
        // Verify if result is as expected
        assertEquals(6, results.size()); // Two system, two rooms, one threshold and one facility should be returned
        OrganisationTest.verifyFacilityQueueResults(results, new String[]{SAMPLE_OFFICE_NAME}, new String[]{SAMPLE_OFFICE_NAME, SAMPLE_OFFICE_DIRECTOR_ROOM_NAME, SAMPLE_OFFICE_STAFF_ROOM_NAME, SAMPLE_OFFICE_SYSTEM_NAME, SAMPLE_OFFICE_SUB_SYSTEM_NAME});
        // For hvac system
        Queue<String[]> resultQueue = results.get(SAMPLE_OFFICE_SYSTEM_NAME);
        assertEquals(1, resultQueue.size()); // only one measure should be available
        String[] metadata = resultQueue.poll();
        verifyMetadataContents(metadata, ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT, StringHelper.SYSTEM_KEY);
        // For hvac subsystem
        resultQueue = results.get(SAMPLE_OFFICE_SUB_SYSTEM_NAME);
        assertEquals(1, resultQueue.size()); // only one measure should be available
        metadata = resultQueue.poll();
        verifyMetadataContents(metadata, ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT, StringHelper.SYSTEM_KEY);
    }

    @Test
    void testGetAllSpatialZoneMetaData_OnlyRoomMeasures() {
        // Insert these triples into the blazegraph
        insertFacilityTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        // Create a new Sparql client - note there should be no username and password for the test container
        SparqlClient client = new SparqlClient(IntegrationTestUtils.SPARQL_ENDPOINT, "", "");
        // Execute method
        Map<String, Queue<String[]>> results = client.getAllSpatialZoneMetaData(SAMPLE_ORGANISATION_NAME);
        // Verify if result is as expected
        OrganisationTest.verifyFacilityQueueResults(results, new String[]{SAMPLE_OFFICE_NAME}, new String[]{SAMPLE_OFFICE_NAME, SAMPLE_OFFICE_DIRECTOR_ROOM_NAME, SAMPLE_OFFICE_STAFF_ROOM_NAME});
        // Should only have two thresholds
        Queue<String[]> resultQueue = results.get(StringHelper.THRESHOLD_KEY);
        while (!resultQueue.isEmpty()) {
            String[] metadata = resultQueue.poll();
            if (metadata[0].equals(RELATIVE_HUMIDITY)) {
                assertEquals(String.valueOf(RELATIVE_HUMIDITY_MIN_THRESHOLD), metadata[1]);
                assertEquals(String.valueOf(RELATIVE_HUMIDITY_MAX_THRESHOLD), metadata[2]);
            } else {
                assertEquals(TEMPERATURE, metadata[0]);
                assertEquals(String.valueOf(TEMPERATURE_MIN_THRESHOLD), metadata[1]);
                assertEquals(String.valueOf(TEMPERATURE_MAX_THRESHOLD), metadata[2]);
            }
        }
        // Director's room should only have one temperature measure
        resultQueue = results.get(SAMPLE_OFFICE_DIRECTOR_ROOM_NAME);
        assertEquals(1, resultQueue.size());
        String[] metadata = resultQueue.poll();
        verifyMetadataContents(metadata, TEMPERATURE, TEMPERATURE_UNIT, StringHelper.ROOM_KEY);
        // Staff room should have both temperature and humidity measure
        resultQueue = results.get(SAMPLE_OFFICE_STAFF_ROOM_NAME);
        assertEquals(2, resultQueue.size());
        metadata = resultQueue.poll();
        verifyMetadataContents(metadata, TEMPERATURE, TEMPERATURE_UNIT, StringHelper.ROOM_KEY);
        metadata = resultQueue.poll();
        verifyMetadataContents(metadata, RELATIVE_HUMIDITY, null, StringHelper.ROOM_KEY);
    }

    public static void insertFacilityTriples(String endpoint) {
        StringBuilder builder = new StringBuilder();
        String organisationRepresentation = TestUtils.genInstance("Organisation");
        String organisationNameRepresentation = TestUtils.genInstance("OrganisationName");
        String directorRoomRepresentation = TestUtils.genInstance("Representation");
        String staffRoomRepresentation = TestUtils.genInstance("Representation");
        String storageRoomRepresentation = TestUtils.genInstance("Representation");
        builder.append(SparqlQueryTest.genExpectedPrefixesString())
                .append("INSERT DATA {<")
                // A building has lab and office facility
                .append(SAMPLE_BUILDING_INSTANCE).append("> rdf:type bot:Building;")
                .append("ontobim:hasFacility <").append(SAMPLE_LAB_INSTANCE).append(">;")
                .append("ontobim:hasFacility <").append(SAMPLE_OFFICE_INSTANCE).append(">.")
                // The organisation managing the facilities
                .append("<").append(organisationRepresentation).append("> <https://www.omg.org/spec/Commons/Designators/hasName> <").append(organisationNameRepresentation).append(">.")
                .append("<").append(organisationNameRepresentation).append("> rdfs:label \"").append(SAMPLE_ORGANISATION_NAME).append("\".")
                // Lab has assets
                .append("<").append(SAMPLE_LAB_INSTANCE).append("> rdfs:label \"").append(SAMPLE_LAB_NAME).append("\";")
                .append("<https://www.theworldavatar.com/kg/ontoassetmanagement/isManagedBy> <").append(organisationRepresentation).append(">;")
                .append("ontobim:hasRoom <").append(SAMPLE_LAB_BIO_ROOM_INSTANCE).append(">;")
                .append("ontobim:hasRoom <").append(SAMPLE_LAB_PILOT_ROOM_INSTANCE).append(">.")
                .append("<").append(SAMPLE_LAB_BIO_ROOM_INSTANCE).append("> rdf:type ontobim:Room.")
                .append("<").append(SAMPLE_LAB_PILOT_ROOM_INSTANCE).append("> rdf:type ontobim:Room.")
                // Asset measures should be placed in another endpoint
                .append("<").append(SAMPLE_LAB_BIO_ROOM_INSTANCE).append("> bot:containsElement <").append(SAMPLE_LAB_SMART_SENSOR_INSTANCE).append(">.")
                .append("<").append(SAMPLE_LAB_SMART_SENSOR_INSTANCE).append("> rdfs:label \"").append(SAMPLE_LAB_SMART_SENSOR_NAME).append("\";")
                .append("rdf:type ontodevice:SmartSensor.")
                .append("<").append(SAMPLE_LAB_PILOT_ROOM_INSTANCE).append("> bot:containsElement <").append(SAMPLE_LAB_MAKE_UP_AIR_UNIT_INSTANCE).append(">;")
                .append("bot:containsElement <").append(SAMPLE_LAB_FRIDGE_INSTANCE).append(">.")
                .append("<").append(SAMPLE_LAB_MAKE_UP_AIR_UNIT_INSTANCE).append("> rdfs:label \"").append(SAMPLE_LAB_MAKE_UP_AIR_UNIT_NAME).append("\";")
                .append("rdf:type ontodevice:MakeUpAirUnit.")
                .append("<").append(SAMPLE_LAB_FRIDGE_INSTANCE).append("> rdfs:label \"").append(SAMPLE_LAB_FRIDGE_NAME).append("\";")
                .append("rdf:type ontodevice:Fridge.")
                // Office has no assets but rooms has measures
                // Thresholds
                .append(genThresholdTriples(SAMPLE_OFFICE_INSTANCE, "ontodevice:hasMinThreshold", "om:Temperature", TEMPERATURE_MIN_THRESHOLD))
                .append(genThresholdTriples(SAMPLE_OFFICE_INSTANCE, "ontodevice:hasMaxThreshold", "om:Temperature", TEMPERATURE_MAX_THRESHOLD))
                .append(genThresholdTriples(SAMPLE_OFFICE_INSTANCE, "ontodevice:hasMinThreshold", "om:RelativeHumidity", RELATIVE_HUMIDITY_MIN_THRESHOLD))
                .append(genThresholdTriples(SAMPLE_OFFICE_INSTANCE, "ontodevice:hasMaxThreshold", "om:RelativeHumidity", RELATIVE_HUMIDITY_MAX_THRESHOLD))
                // Link facility to rooms
                .append("<").append(SAMPLE_OFFICE_INSTANCE).append("> rdfs:label \"").append(SAMPLE_OFFICE_NAME).append("\";")
                .append("<https://www.theworldavatar.com/kg/ontoassetmanagement/isManagedBy> <").append(organisationRepresentation).append(">;")
                .append("ontobim:hasRoom <").append(SAMPLE_OFFICE_DIRECTOR_ROOM_INSTANCE).append(">;")
                .append("ontobim:hasRoom <").append(SAMPLE_OFFICE_STAFF_ROOM_INSTANCE).append(">;")
                .append("ontobim:hasRoom <").append(SAMPLE_OFFICE_STORAGE_ROOM_INSTANCE).append(">.")
                // Director room should only have temperature
                .append("<").append(SAMPLE_OFFICE_DIRECTOR_ROOM_INSTANCE).append("> rdf:type ontobim:Room;")
                .append("ontobim:hasIfcRepresentation <").append(directorRoomRepresentation).append(">.")
                .append("<").append(directorRoomRepresentation).append("> rdfs:label \"").append(SAMPLE_OFFICE_DIRECTOR_ROOM_NAME).append("\".")
                .append(genMeasureTriples(SAMPLE_OFFICE_DIRECTOR_ROOM_INSTANCE, "ontodevice:hasTemperature", TEMPERATURE, TEMPERATURE_UNIT))
                // Staff room should have both temperature and humidity
                .append("<").append(SAMPLE_OFFICE_STAFF_ROOM_INSTANCE).append("> rdf:type ontobim:Room;")
                .append("ontobim:hasIfcRepresentation <").append(staffRoomRepresentation).append(">.")
                .append("<").append(staffRoomRepresentation).append("> rdfs:label \"").append(SAMPLE_OFFICE_STAFF_ROOM_NAME).append("\".")
                .append(genMeasureTriples(SAMPLE_OFFICE_STAFF_ROOM_INSTANCE, "ontodevice:hasTemperature", TEMPERATURE, TEMPERATURE_UNIT))
                .append(genMeasureTriples(SAMPLE_OFFICE_STAFF_ROOM_INSTANCE, "ontodevice:hasRelativeHumidity", RELATIVE_HUMIDITY, "", PostGisClientTest.SAMPLE_ROOM_HUMIDITY_INSTANCE, PostGisClientTest.SAMPLE_ROOM_HUMIDITY_TIME_SERIES_INSTANCE))
                // Storage room has no measures
                .append("<").append(SAMPLE_OFFICE_STORAGE_ROOM_INSTANCE).append("> rdf:type ontobim:Room;")
                .append("ontobim:hasIfcRepresentation <").append(storageRoomRepresentation).append(">.")
                .append("<").append(storageRoomRepresentation).append("> rdfs:label \"").append(SAMPLE_OFFICE_STORAGE_ROOM_NAME).append("\".")
                .append("}");
        IntegrationTestUtils.updateEndpoint(endpoint, builder.toString());
    }

    public static void insertSystemTriples(String endpoint) {
        StringBuilder builder = new StringBuilder();
        builder.append(SparqlQueryTest.genExpectedPrefixesString())
                .append("INSERT DATA {<")
                // Insert a HVAC system
                .append(SAMPLE_OFFICE_INSTANCE).append("> ontotechsystem:containsSystem <").append(SAMPLE_OFFICE_SYSTEM_INSTANCE).append(">.")
                .append("<").append(SAMPLE_OFFICE_SYSTEM_INSTANCE).append("> rdfs:label \"").append(SAMPLE_OFFICE_SYSTEM_NAME).append("\";")
                .append("ontotechsystem:composedOf <").append(SAMPLE_OFFICE_SUB_SYSTEM_INSTANCE).append(">.") // Include its subsystem
                // Generate the measure for the system's energy consumption with actual GIS data
                .append(genMeasureTriples(SAMPLE_OFFICE_SYSTEM_INSTANCE, "ontoubemmp:consumesEnergy", ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT,
                        PostGisClientTest.SAMPLE_SYSTEM_ELEC_CONSUMPTION_INSTANCE, PostGisClientTest.SAMPLE_SYSTEM_ELEC_CONSUMPTION_TIME_SERIES_INSTANCE))
                // Generate the measure for the sub-system's energy consumption, this will not be instantiated in the postgis system
                .append("<").append(SAMPLE_OFFICE_SUB_SYSTEM_INSTANCE).append("> rdfs:label \"").append(SAMPLE_OFFICE_SUB_SYSTEM_NAME).append("\".")
                .append(genMeasureTriples(SAMPLE_OFFICE_SUB_SYSTEM_INSTANCE, "ontoubemmp:consumesEnergy", ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT))
                .append("}");
        IntegrationTestUtils.updateEndpoint(endpoint, builder.toString());
    }


    public static void insertAssetTriples(String endpoint, boolean isComplex) {
        StringBuilder builder = new StringBuilder();
        String intermediateSensorTwo = TestUtils.genInstance("Sensor");
        String intermediateSensorFour = TestUtils.genInstance("Sensor");
        String intermediateMAUSensor = TestUtils.genInstance("Sensor");
        String operatingRangeInstance = TestUtils.genInstance("OperatingRange");
        String operatingPropertyInstance = TestUtils.genInstance("OperatingProperty");
        builder.append(SparqlQueryTest.genExpectedPrefixesString())
                .append("INSERT DATA {")
                // First sub sensor pattern
                .append("<").append(SAMPLE_LAB_SMART_SENSOR_INSTANCE).append("> ontodevice:sendsSignalTo <").append(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_ONE_INSTANCE).append(">.")
                .append(genMeasureTriples(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_ONE_INSTANCE, "ontodevice:measures", TEMPERATURE, TEMPERATURE_UNIT, PostGisClientTest.SAMPLE_TEMPERATURE_INSTANCE, PostGisClientTest.SAMPLE_TEMPERATURE_TIME_SERIES_INSTANCE));
        if (isComplex) {
            // Second sub sensor pattern
            builder.append("<").append(SAMPLE_LAB_SMART_SENSOR_INSTANCE).append("> ontodevice:sendsSignalTo <").append(intermediateSensorTwo).append(">.")
                    .append("<").append(intermediateSensorTwo).append("> saref:consistsOf <").append(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_TWO_INSTANCE).append(">.")
                    .append(genMeasureTriples(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_TWO_INSTANCE, "observes", STATE, ""))
                    // Third sub sensor pattern
                    .append("<").append(SAMPLE_LAB_SMART_SENSOR_INSTANCE).append("> saref:consistsOf <").append(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_THREE_INSTANCE).append(">.")
                    .append(genMeasureTriples(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_THREE_INSTANCE, "ontodevice:measures", ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT))
                    // Fourth sub sensor pattern
                    .append("<").append(SAMPLE_LAB_SMART_SENSOR_INSTANCE).append("> saref:consistsOf <").append(intermediateSensorFour).append(">.")
                    .append("<").append(intermediateSensorFour).append("> ontodevice:sendsSignalTo <").append(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_FOUR_INSTANCE).append(">.")
                    .append(genMeasureTriples(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_FOUR_INSTANCE, "ontodevice:measures", RELATIVE_HUMIDITY, ""))
                    // Fifth sub sensor pattern - directly nested below sensor four
                    .append("<").append(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_FOUR_INSTANCE).append("> saref:consistsOf <").append(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_FIVE_INSTANCE).append(">.")
                    .append(genMeasureTriples(SAMPLE_LAB_SMART_SENSOR_SUB_SENSOR_FIVE_INSTANCE, "observes", HUMIDITY_STATE, ""))
                    // Pattern inclusive of an additional sub element
                    .append("<").append(SAMPLE_LAB_MAKE_UP_AIR_UNIT_INSTANCE).append("> saref:consistsOf <").append(SAMPLE_LAB_MAKE_UP_AIR_UNIT_DUCT_INSTANCE).append(">.")
                    .append(genMeasureTriples(intermediateMAUSensor, "ontodevice:measures", TEMPERATURE, TEMPERATURE_UNIT))
                    .append("<").append(intermediateMAUSensor).append("> ontodevice:isAttachedTo <").append(SAMPLE_LAB_MAKE_UP_AIR_UNIT_DUCT_INSTANCE).append(">.")
                    .append("<").append(SAMPLE_LAB_MAKE_UP_AIR_UNIT_DUCT_INSTANCE).append("> rdfs:label \"").append(SAMPLE_LAB_MAKE_UP_AIR_UNIT_DUCT_NAME).append("\".")
                    // For alternate measure paths
                    .append("<").append(SAMPLE_LAB_FRIDGE_INSTANCE).append("> ontodevice:hasOperatingRange <").append(operatingRangeInstance).append(">.")
                    .append("<").append(operatingRangeInstance).append("> ssn:hasOperatingProperty <").append(operatingPropertyInstance).append(">.")
                    .append(genMeasureTriples(operatingPropertyInstance, "ontodevice:hasQuantity", ELECTRICITY_CONSUMPTION, ELECTRICITY_CONSUMPTION_UNIT));
        }
        builder.append("}");
        IntegrationTestUtils.updateEndpoint(endpoint, builder.toString());
    }

    private static StringBuilder genMeasureTriples(String item, String measureProperty, String measureName, String unit) {
        return genMeasureTriples(item, measureProperty, measureName, unit, TestUtils.genInstance("Measure"), TestUtils.genTimeSeriesInstance());
    }

    private static StringBuilder genMeasureTriples(String item, String measureProperty, String measureName, String unit, String measureInst, String timeSeriesInst) {
        StringBuilder builder = new StringBuilder();
        if (measureProperty.equals("observes")) {
            builder.append("<").append(item).append("> ontodevice:").append(measureProperty).append(" <").append(measureInst).append(">.");
        } else {
            // Quantity instance is only relevant for non-observes property
            String quantityInst = TestUtils.genInstance("Quantity");
            builder.append("<").append(item).append("> ").append(measureProperty).append(" <").append(quantityInst).append(">.")
                    .append("<").append(quantityInst).append("> om:hasValue <").append(measureInst).append(">.");
        }
        builder.append("<").append(measureInst).append("> rdfs:label \"").append(measureName).append("\";")
                .append("ontotimeseries:hasTimeSeries <").append(timeSeriesInst).append(">.");
        if (!unit.equals("")) {
            String unitInst = TestUtils.genInstance("Unit");
            builder.append("<").append(measureInst).append("> om:hasUnit <").append(unitInst).append(">.")
                    .append("<").append(unitInst).append("> om:symbol \"").append(unit).append("\".");
        }
        return builder;
    }

    private static StringBuilder genThresholdTriples(String facility, String measureProperty, String measureType, double thresholdVal) {
        StringBuilder builder = new StringBuilder();
        String thresholdInst = TestUtils.genInstance("Threshold");
        String quantityInst = TestUtils.genInstance("Quantity");
        String measureInst = TestUtils.genInstance("Measure");
        builder.append("<").append(facility).append("> ").append(measureProperty).append(" <").append(thresholdInst).append(">.")
                .append("<").append(thresholdInst).append("> ontodevice:hasQuantity <").append(quantityInst).append(">.")
                .append("<").append(quantityInst).append("> rdf:type ").append(measureType).append(";")
                .append("om:hasValue <").append(measureInst).append(">.")
                .append("<").append(measureInst).append("> om:hasNumericalValue ").append(thresholdVal).append(".");
        return builder;
    }

    private static void verifyMetadataContents(String[] metadata, String measureName, String unit, String itemType) {
        assertEquals(measureName, metadata[0]);
        assertTrue(metadata[1].contains(TestUtils.BASE_URI + "Measure_") || metadata[1].equals(PostGisClientTest.SAMPLE_TEMPERATURE_INSTANCE)
                || metadata[1].equals(PostGisClientTest.SAMPLE_ROOM_HUMIDITY_INSTANCE) || metadata[1].equals(PostGisClientTest.SAMPLE_SYSTEM_ELEC_CONSUMPTION_INSTANCE));
        assertTrue(metadata[2].contains(TestUtils.BASE_URI + "TimeSeries_"));
        assertEquals(unit, metadata[3]);
        assertEquals(itemType, metadata[4]);
    }
}