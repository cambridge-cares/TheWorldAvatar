package uk.ac.cam.cares.jps.agent.smartmeteragent;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.Connection;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.MockStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.util.FileUtil;

public class SmartMeterAgentTest {

    @TempDir
    File tempFolder;

    MockStoreClient mockStore = populateMockStore(new MockStoreClient());
    
    @Test
    public void testGetSqlQueryForData() {
        List<String> devices = new ArrayList<>();
        devices.add("device1");
        devices.add("device2");
        String beforeTime = "2021-01-01 00:00";
        String afterTime = "2001-01-01 00:00";
        SmartMeterAgent smAgent = new SmartMeterAgent();
        String expectedQuery = "SELECT strftime('%Y-%m-%dT%H:%M:00+00:00', MAX(ts)) as time, \"data_source\" as device, " 
        + "(\"ch1Watt\" + \"ch2Watt\" + \"ch3Watt\")/3 as pd, (\"ch1Current\" + \"ch2Current\" + \"ch3Current\")/3 as current, " 
        + "(\"ch1Voltage\" + \"ch2Voltage\" + \"ch3Voltage\")/3 as voltage, (\"ch1Hz\" + \"ch2Hz\" + \"ch3Hz\")/3 as frequency " 
        + "FROM \"Measurement\" " 
        + "WHERE " 
        + "\"ch1Watt\" <> 0 AND \"ch2Watt\" <> 0 AND \"ch3Watt\" <> 0 AND " 
        + "\"ch1Current\" <> 0 AND \"ch2Current\" <> 0 AND \"ch3Current\" <> 0 AND " 
        + "\"ch1Voltage\" <> 0 AND \"ch2Voltage\" <> 0 AND \"ch3Voltage\" <> 0 AND " 
        + "\"ch1Hz\" <> 0 AND \"ch2Hz\" <> 0 AND \"ch3Hz\" <> 0 " 
        + "AND (\"data_source\" = 'device1' OR \"data_source\" = 'device2') "
        + "AND ts <= '2021-01-01 00:00' "
        + "AND ts >= '2001-01-01 00:00' "
        + "GROUP BY device, id " 
        + "ORDER BY ts DESC "
        + "LIMIT 2;";
        String actualQuery = smAgent.getSqlQueryForData(devices, beforeTime, afterTime);
        assertEquals(expectedQuery, actualQuery);
    }

    @Test
    public void testValidateResult() {
        List<String> devices = new ArrayList<String>();
        JSONArray resultArray = new JSONArray();
        JSONObject result1 = new JSONObject()
                            .put("time", "2023-03-28T18:19:00+00:00")
                            .put("Pd", "0.01")
                            .put("device", "Load")
                            .put("current", "0.02")
                            .put("voltage", "0.03")
                            .put("frequency", "0.04");
        JSONObject result2 = new JSONObject()
                            .put("time", "2023-03-28T18:19:00+00:00")
                            .put("Pd", "0.01")
                            .put("device", "Load")
                            .put("current", "0.02")
                            .put("voltage", "0.03")
                            .put("frequency", "0.04");
        JSONObject result3 = new JSONObject()
                            .put("time", "2023-03-28T18:20:00+00:00")
                            .put("Pd", "0.01")
                            .put("device", "PV")
                            .put("current", "0.02")
                            .put("voltage", "0.03")
                            .put("frequency", "0.04");
        devices.add("Load");
        devices.add("PV");
        SmartMeterAgent smAgent = new SmartMeterAgent();
        // When not all devices have readings retrieved
        resultArray.put(result1);
        resultArray.put(result2);
        assertFalse(smAgent.validateResult(devices, resultArray));
        // When readings retrieved have different time
        resultArray.remove(1);
        resultArray.put(result3);
        assertFalse(smAgent.validateResult(devices, resultArray));
    }

    @Test
    public void testReadDataFromCsvFile() throws IOException {
        File mappingFile = new File(tempFolder, "reading.csv");
        FileWriter writer = new FileWriter(mappingFile);
        writer.write(smartMeterReading1);
        writer.write(smartMeterReading2);
        writer.write(smartMeterReading3);
        writer.write(smartMeterReading4);
        writer.write(smartMeterReading5);
        writer.write(smartMeterReading6);
        writer.close();

        String filename = tempFolder.toPath().toString() + "/reading.csv";
        List<String> devices = new ArrayList<>();
        devices.add("device1");
        devices.add("device2");
        OffsetDateTime afterTime = OffsetDateTime.parse("2022-10-26T18:23:00+00:00");
        OffsetDateTime beforeTime = OffsetDateTime.parse("2022-10-26T18:24:00+00:00");
        JSONArray dataIriArray = new JSONArray();
        JSONObject dataIri1 = new JSONObject()
                            .put("device", "device1")
                            .put("BusNumbervalue", "1")
                            .put("PdIri", "http://example.com/device1pd")
                            .put("QdIri", "http://example.com/device1qd");
        JSONObject dataIri2 = new JSONObject()
                            .put("device", "device2")
                            .put("BusNumbervalue", "2")
                            .put("PdIri", "http://example.com/device2pd")
                            .put("QdIri", "http://example.com/device2qd");
        dataIriArray.put(dataIri1);
        dataIriArray.put(dataIri2);
        SmartMeterAgent smAgent = new SmartMeterAgent() {
            @Override
            public void uploadSmartMeterData(JSONArray queryResult, JSONArray dataIRIArray) {
            }
        };
        // check that the number of valid readings selected is correct
        assertEquals(1, smAgent.readDataFromCsvFile(filename, devices, beforeTime, afterTime, dataIriArray));
    }

    @Test
    public void testCheckEmptyReading() {
        SmartMeterAgent smAgent = new SmartMeterAgent();
        String[] validReading = {"1", "2023-03-24T14:41:00+00:00", "0", "0", "POWERMETER", "2023-03-24T14:41:00+00:00", 
        "1.1", "1.2", "1.3", "", "","3.2", "3.4", "3.6", "226.1", "227.1", 
        "228.1", "", "", "", "393.37439", "394.527283", "393.128021", "49.99", "49.99", "49.99", 
        "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "Grid"};
        assertFalse(smAgent.checkEmptyReading(validReading));
        String[] invalidReading = validReading;
        invalidReading[25] = "";
        assertTrue(smAgent.checkEmptyReading(invalidReading));
    }

    @Test
    public void testProcessCsvReadings() {
        String[] reading = {"1", "2023-03-24T14:41:00+00:00", "0", "0", "POWERMETER", "2023-03-24T14:41:00+00:00", 
        "1.1", "1.2", "1.3", "", "","3.2", "3.4", "3.6", "226.1", "227.1", 
        "228.1", "", "", "", "393.37439", "394.527283", "393.128021", "49.99", "49.99", "49.99", 
        "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "Grid"};
        SmartMeterAgent smAgent = new SmartMeterAgent();
        JSONObject actualResult = smAgent.processCsvReadings(reading);

        assertEquals("2023-03-24T14:41:00+00:00", actualResult.getString("time"));
        assertEquals("Grid", actualResult.getString("device"));
        assertEquals(1.2, actualResult.getDouble("pd"), 0.1);
        assertEquals(3.4, actualResult.getDouble("current"), 0.1);
        assertEquals(227.1, actualResult.getDouble("voltage"), 0.1);
        assertEquals(49.99, actualResult.getDouble("frequency"), 0.1);
    }

    @Test
    public void testGetDataIris() {
        SmartMeterAgent smAgent = new SmartMeterAgent() {
            @Override
            public JSONArray callAccessAgentToQuery(String targetResourceID, String sparqlQuery) {
                return mockStore.executeQuery(sparqlQuery);
            }
        };
        // When mapping file is not complete, check that correct exception is thrown
        List<String[]> incompleteMappings = new ArrayList<>();
        String[] mapping1 = {"1", "device1"};
        incompleteMappings.add(mapping1);
        Exception exception = assertThrows(JPSRuntimeException.class, 
                                            ()->{smAgent.getDataIris("", incompleteMappings);});
        String expectedMessage = "Mapping file not complete, bus 2.0 does not have mapping information.";
        String actualMessage = exception.getMessage();
        assertTrue(actualMessage.contains(expectedMessage));

        // When mapping file is complete, check that dataIRIs and devices are put together correctly
        String[] mapping2 = {"2"};
        List<String[]> completeMappings = new ArrayList<>();
        completeMappings.add(mapping1);
        completeMappings.add(mapping2);
        JSONArray actualResult = smAgent.getDataIris("", completeMappings);
        if (actualResult.getJSONObject(0).getString("BusNumbervalue").equals("1.0")) {
            assertEquals("http://example.com/device1pd", actualResult.getJSONObject(0).optString("PdIri"));
            assertEquals("http://example.com/device1qd", actualResult.getJSONObject(0).optString("QdIri"));
            assertEquals("device1", actualResult.getJSONObject(0).getString("device"));
        } else if (actualResult.getJSONObject(0).getString("BusNumbervalue").equals("2.0")) {
            assertEquals("http://example.com/device2pd", actualResult.getJSONObject(0).optString("PdIri"));
            assertEquals("http://example.com/device2qd", actualResult.getJSONObject(0).optString("QdIri"));
            assertEquals("http://example.com/device2current", actualResult.getJSONObject(0).optString("currentIri"));
            assertEquals("http://example.com/device2voltage", actualResult.getJSONObject(0).optString("voltageIri"));
            assertEquals("http://example.com/device2frequency", actualResult.getJSONObject(0).optString("frequencyIri"));
            assertEquals("0", actualResult.getJSONObject(0).getString("device"));
        } else {
            throw new AssertionError("Incorrect BusNumbervalue.");
        }
        if (actualResult.getJSONObject(1).getString("BusNumbervalue").equals("1.0")) {
            assertEquals("http://example.com/device1pd", actualResult.getJSONObject(1).optString("PdIri"));
            assertEquals("http://example.com/device1qd", actualResult.getJSONObject(1).optString("QdIri"));
            assertEquals("device1", actualResult.getJSONObject(1).getString("device"));
        } else if (actualResult.getJSONObject(1).getString("BusNumbervalue").equals("2.0")) {
            assertEquals("http://example.com/device2pd", actualResult.getJSONObject(1).optString("PdIri"));
            assertEquals("http://example.com/device2qd", actualResult.getJSONObject(1).optString("QdIri"));
            assertEquals("http://example.com/device2current", actualResult.getJSONObject(1).optString("currentIri"));
            assertEquals("http://example.com/device2voltage", actualResult.getJSONObject(1).optString("voltageIri"));
            assertEquals("http://example.com/device2frequency", actualResult.getJSONObject(1).optString("frequencyIri"));
            assertEquals("0", actualResult.getJSONObject(1).getString("device"));
        } else {
            throw new AssertionError("Incorrect BusNumbervalue.");
        }
    }

    @Test
    public void testGetDataMapping() throws IOException {
        File mappingFile = new File(tempFolder, "mappings.csv");
        FileWriter writer = new FileWriter(mappingFile);
        writer.write("1,device1\n");
        writer.write("2,");
        writer.close();

        String[] mapping1 = {"1", "device1"};
        String[] mapping2 = {"2"};

        SmartMeterAgent smAgent = new SmartMeterAgent();
        List<String[]> actualResult = smAgent.getDataMappings(tempFolder.toPath().toString());
        assertArrayEquals(mapping1, actualResult.get(0));
        assertArrayEquals(mapping2, actualResult.get(1));
    }
    
    @Test
    public void testFromCsvToArray() throws IOException {
        File inputCsvFile = new File(tempFolder, "testFromCsvToArray.csv");
        FileWriter csvWriter = new FileWriter(inputCsvFile);
        csvWriter.write("bus,busnumber,bustype\n" + "branch,frombus,tobus\n");
        csvWriter.close();
        
        String[] expetedFirstLine = {"bus", "busnumber", "bustype"};
        String[] expectedSecondLine = {"branch", "frombus", "tobus"};

        String inputCsv = tempFolder.toPath().toString() + "/testFromCsvToArray.csv";
        String csv = FileUtil.readFileLocally(inputCsv);
        SmartMeterAgent smAgent = new SmartMeterAgent();
        List<String[]> actualList = smAgent.fromCsvToArray(csv);

        assertArrayEquals(expetedFirstLine, actualList.get(0));
        assertArrayEquals(expectedSecondLine, actualList.get(1));
    }

    @Test
    public void testValidateInput() {
        SmartMeterAgentLauncher agentLauncher = new SmartMeterAgentLauncher();
        JSONObject request = new JSONObject();
        // Empty request
        assertFalse(agentLauncher.validateInput(request));
        request.put("dataRequired", "latest");
        request.put("microgrid", "http://microgridIRI.com");
        // without data source
        assertFalse(agentLauncher.validateInput(request));
        request.put("dataSource", "csv");
        request.remove("dataRequired");
        // without data required
        assertFalse(agentLauncher.validateInput(request));
        request.put("dataRequired", "latest");
        request.remove("microgrid");
        // without microgrid targetResourceID
        assertFalse(agentLauncher.validateInput(request));
        request.put("microgrid", "http://microgridIRI.com");
        // complete request
        assertTrue(agentLauncher.validateInput(request));
    }

    @Test
    public void testProcessRequestParametersInvalidInput() {
        SmartMeterAgentLauncher agentLauncher = new SmartMeterAgentLauncher();
        JSONObject request = new JSONObject();
        Exception exception = assertThrows(JPSRuntimeException.class, ()->{agentLauncher.processRequestParameters(request);});
        String expectedMessage = "Invalid request.";
        String actualMessage = exception.getMessage();
        assertTrue(actualMessage.contains(expectedMessage));

        request.put("dataSource", "csv");
        request.put("dataRequired", "latest");
        request.put("microgrid", "http://microgridIRI.com");
        exception = assertThrows(JPSRuntimeException.class, ()->{agentLauncher.processRequestParameters(request);});
        expectedMessage = "dataRequired should be historical if reading from csv files.";
        actualMessage = exception.getMessage();
        assertTrue(actualMessage.contains(expectedMessage));

        request.remove("dataRequired");
        request.put("dataRequired", "123");
        exception = assertThrows(JPSRuntimeException.class, ()->{agentLauncher.processRequestParameters(request);});
        expectedMessage = "Invalid data source or data required.";
        actualMessage = exception.getMessage();
        assertTrue(actualMessage.contains(expectedMessage));

        request.remove("dataRequired");
        request.put("dataRequired", "historical");
        request.put("dataBefore", "2022-10-26 18:23:58");
        exception = assertThrows(JPSRuntimeException.class, ()->{agentLauncher.processRequestParameters(request);});
        expectedMessage = "Incorrect time format, input time should be yyyy-MM-dd HH:mm .";
        actualMessage = exception.getMessage();
        assertTrue(actualMessage.contains(expectedMessage));
    }

    // Example readings for testing
    // For time: 2022-10-26 18:23:00, invalid reading (not all devices are here)
    protected String smartMeterReading1 = "1,2022-10-26 18:23:58.000000,0,0,POWERMETER,2022-10-26 18:23:58.000000," 
                                        + "0.322116,0.318081,0.31309,,,1.448439,1.426094,1.406155,223.604309,224.251617," 
                                        + "224.19278,,,,387.620453,388.752472,387.646515,49.999737,49.999737,49.999737," 
                                        + "0.0,0.0,0.0,0.0,0.0,0.0,device1\n";
    protected String smartMeterReading2 = "2,2022-10-26 18:23:58.000000,0,0,POWERMETER,2022-10-26 18:23:58.000000," 
                                        + "0.322116,0.318081,0.31309,,,1.448439,1.426094,1.406155,223.604309,224.251617," 
                                        + "224.19278,,,,387.620453,388.752472,387.646515,49.999737,49.999737,49.999737," 
                                        + "0.0,0.0,0.0,0.0,0.0,0.0,ExtraDevice\n";
    // For time: 2022-10-26 18:24:00, valid reading
    protected String smartMeterReading3 = "3,2022-10-26 18:24:58.000000,0,0,POWERMETER,2022-10-26 18:23:58.000000," 
                                        + "0.322116,0.318081,0.31309,,,1.448439,1.426094,1.406155,223.604309,224.251617," 
                                        + "224.19278,,,,387.620453,388.752472,387.646515,49.999737,49.999737,49.999737," 
                                        + "0.0,0.0,0.0,0.0,0.0,0.0,device1\n";
    protected String smartMeterReading4 = "4,2022-10-26 18:24:58.000000,0,0,POWERMETER,2022-10-26 18:23:58.000000," 
                                        + "0.322116,0.318081,0.31309,,,1.448439,1.426094,1.406155,223.604309,224.251617," 
                                        + "224.19278,,,,387.620453,388.752472,387.646515,49.999737,49.999737,49.999737," 
                                        + "0.0,0.0,0.0,0.0,0.0,0.0,device2\n";
    // For time: 2022-10-26 18:25:00, time out of bound
    protected String smartMeterReading5 = "5,2022-10-26 18:25:58.000000,0,0,POWERMETER,2022-10-26 18:23:58.000000," 
                                        + "0.322116,0.318081,0.31309,,,1.448439,1.426094,1.406155,223.604309,224.251617," 
                                        + "224.19278,,,,387.620453,388.752472,387.646515,49.999737,49.999737,49.999737," 
                                        + "0.0,0.0,0.0,0.0,0.0,0.0,device1\n";
    protected String smartMeterReading6 = "6,2022-10-26 18:25:58.000000,0,0,POWERMETER,2022-10-26 18:23:58.000000," 
                                        + "0.322116,0.318081,0.31309,,,1.448439,1.426094,1.406155,223.604309,224.251617," 
                                        + "224.19278,,,,387.620453,388.752472,387.646515,49.999737,49.999737,49.999737," 
                                        + "0.0,0.0,0.0,0.0,0.0,0.0,device2\n";

    public MockStoreClient populateMockStore(MockStoreClient mockStore) {
        // bus 1
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#BusNode>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isModeledBy>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001>");

        // bus number
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#BusNumber_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#BusNumber_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusNumber>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#BusNumber_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#V_BusNumber_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#V_BusNumber_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "\'1.0\'");
        
        // Pd IRI
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasActivePowerAbsorbed>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#Pd_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Pd_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedActivePower>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Pd_EBus-001>", 
                            "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue>", 
                            "<http://example.com/device1pd>");

        // Qd IRI
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasReactivePowerAbsorbed>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#Gd_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Gd_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedReactivePower>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Gd_EBus-001>", 
                            "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue>", 
                            "<http://example.com/device1qd>");

        // bus 2
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Ebus-002>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#BusNode>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Ebus-002>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isModeledBy>", 
                            "<http://localhost:8080/powernetwork/EBus-002.owl#Model_Ebus-002>");

        // bus number
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Model_Ebus-002>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EBus-002.owl#BusNumber_EBus-002>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#BusNumber_EBus-002>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusNumber>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#BusNumber_EBus-002>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EBus-002.owl#V_BusNumber_EBus-002>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#V_BusNumber_EBus-002>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "\'2.0\'");

        // Pd IRI
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Ebus-002>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasActivePowerAbsorbed>", 
                            "<http://localhost:8080/powernetwork/EBus-002.owl#Pd_EBus-002>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Pd_EBus-002>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedActivePower>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Pd_EBus-002>", 
                            "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue>", 
                            "<http://example.com/device2pd>");

        // Qd IRI
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Ebus-002>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasReactivePowerAbsorbed>", 
                            "<http://localhost:8080/powernetwork/EBus-002.owl#Gd_EBus-002>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Gd_EBus-002>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedReactivePower>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Gd_EBus-002>", 
                            "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue>", 
                            "<http://example.com/device2qd>");

        // device 2 has current & voltage & frequency IRIs
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Ebus-002>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasCurrent>",
                            "<http://localhost:8080/powernetwork/EBus-002.owl#Current_EBus-002>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Current_EBus-002>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#Current>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Current_EBus-002>", 
                            "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue>", 
                            "<http://example.com/device2current>");
        
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Ebus-002>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasActualVoltage>", 
                            "<http://localhost:8080/powernetwork/EBus-002.owl#Voltage_EBus-002>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Voltage_EBus-002>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#Voltage>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Voltage_EBus-002>", 
                            "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue>", 
                            "<http://example.com/device2voltage>");

        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Ebus-002>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasFrequency>", 
                            "<http://localhost:8080/powernetwork/EBus-002.owl#Frequency_EBus-002>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Frequency_EBus-002>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#Frequency>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-002.owl#Frequency_EBus-002>", 
                            "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue>", 
                            "<http://example.com/device2frequency>");
        return mockStore;
    }
}
