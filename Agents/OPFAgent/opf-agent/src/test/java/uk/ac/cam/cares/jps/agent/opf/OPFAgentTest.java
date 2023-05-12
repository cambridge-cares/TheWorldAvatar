package test.java.uk.ac.cam.cares.jps.agent.opf;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.BadRequestException;

import org.json.JSONException;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import main.java.uk.ac.cam.cares.jps.agent.opf.OPFAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.MockStoreClient;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

public class OPFAgentTest {

    @Rule
    public TemporaryFolder tempFolder = new TemporaryFolder();

    public MockStoreClient mockStore = populateMockStore(new MockStoreClient());

    @Test
    public void testGenerateInputWithoutSolar() throws IOException {
        String baseUrl = tempFolder.getRoot().getAbsolutePath();
        String[] expectedIRIs = {"http://www.ontology-of-units-of-measure.org/resource/om-2/sample_IRI_Pd", 
                                "http://www.ontology-of-units-of-measure.org/resource/om-2/sample_IRI_Qd"};
        OPFAgent agent = new OPFAgent(){
            @Override
            public JSONArray callAccessAgentToQuery(String targetResourceID, String sparqlQuery) {
                return mockStore.executeQuery(sparqlQuery);
            }

            @Override
            public List<String[]> readTimeSeriesData(List<String[]> dataIRIs, OffsetDateTime time, int numOfBus, String hasSolar) {
                assertArrayEquals(expectedIRIs, dataIRIs.get(0));
                String[] busLoad = {"0", "0"};
                List<String[]> busList = new ArrayList<String[]>();
                busList.add(busLoad);
                return busList;
            }
        };

        agent.generateInputWithAccessAgent("", baseUrl, "1", "2020-01-01T08:00:00+00:00", "false");

        // Check that bus input file is generated
        String fileName = baseUrl + "/bus.txt";
        File file = new File(fileName);
        assertTrue(file.exists());
        // Check bus input information
        String busOutput = FileUtil.readFileLocally(fileName);
        String expectedBus = "1	3	0	0	0	0	1	1.0	0	11	1	1	1\n";
        assertEquals(expectedBus, busOutput);

        // Check branch inputs
        fileName = baseUrl + "/branch.txt";
        file = new File(fileName);
        assertTrue(file.exists());
        String branchOutput = FileUtil.readFileLocally(fileName);
        String expectedBranch = "1	1	0.018436446	0.012435537	0	9900	0	0	0	0	1	-360	360\n";
        assertEquals(expectedBranch, branchOutput);

        // Check generator inputs
        fileName = baseUrl + "/gen.txt";
        file = new File(fileName);
        assertTrue(file.exists());
        String genOutput = FileUtil.readFileLocally(fileName);
        String expectedGen = "1	0	0	10	-10	1	100	1	10	0	0	0	0	0	0	0	0	0	0	0	0\n";
        assertEquals(expectedGen, genOutput);

        // Check genCost inputs
        fileName = baseUrl + "/genCost.txt";
        file = new File(fileName);
        assertTrue(file.exists());
        String genCostOutput = FileUtil.readFileLocally(fileName);
        String expectedGenCost = "2	0	0	3	0	20	0\n";
        assertEquals(expectedGenCost, genCostOutput);

        // Check baseMVA inputs
        fileName = baseUrl + "/baseMVA.txt";
        file = new File(fileName);
        assertTrue(file.exists());
        String baseMVA = FileUtil.readFileLocally(fileName);
        String expectedBaseMVA = "1";
        assertEquals(expectedBaseMVA, baseMVA);
    }

    @Test
    public void testGenerateInputWithSolar() throws IOException {
        String baseUrl = tempFolder.getRoot().getAbsolutePath();
        String[] expectedIRIs = {"http://www.ontology-of-units-of-measure.org/resource/om-2/sample_IRI_Pd", 
                                "http://www.ontology-of-units-of-measure.org/resource/om-2/sample_IRI_Qd", 
                                "http://www.ontology-of-units-of-measure.org/resource/om-2/sample_IRI_solarPd"};
        OPFAgent agent = new OPFAgent(){
            @Override
            public JSONArray callAccessAgentToQuery(String targetResourceID, String sparqlQuery) {
                return mockStore.executeQuery(sparqlQuery);
            }

            @Override
            public List<String[]> readTimeSeriesData(List<String[]> dataIRIs, OffsetDateTime time, int numOfBus, String hasSolar) {
                assertArrayEquals(expectedIRIs, dataIRIs.get(0));
                String[] busLoad = {"0", "0", null};
                List<String[]> busList = new ArrayList<String[]>();
                busList.add(busLoad);
                return busList;
            }
        };

        agent.generateInputWithAccessAgent("", baseUrl, "1", "2020-01-01T08:00:00+00:00", "true");

        // Check that bus input file is generated
        String fileName = baseUrl + "/bus.txt";
        File file = new File(fileName);
        assertTrue(file.exists());
        // Check bus input information
        String busOutput = FileUtil.readFileLocally(fileName);
        String expectedBus = "1	3	0	0	0	0	1	1.0	0	11	1	1	1\n";
        assertEquals(expectedBus, busOutput);

        // Check branch inputs
        fileName = baseUrl + "/branch.txt";
        file = new File(fileName);
        assertTrue(file.exists());
        String branchOutput = FileUtil.readFileLocally(fileName);
        String expectedBranch = "1	1	0.018436446	0.012435537	0	9900	0	0	0	0	1	-360	360\n";
        assertEquals(expectedBranch, branchOutput);

        // Check generator inputs
        fileName = baseUrl + "/gen.txt";
        file = new File(fileName);
        assertTrue(file.exists());
        String genOutput = FileUtil.readFileLocally(fileName);
        String expectedGen = "1	0	0	10	-10	1	100	1	10	0	0	0	0	0	0	0	0	0	0	0	0\n";
        assertEquals(expectedGen, genOutput);

        // Check genCost inputs
        fileName = baseUrl + "/genCost.txt";
        file = new File(fileName);
        assertTrue(file.exists());
        String genCostOutput = FileUtil.readFileLocally(fileName);
        String expectedGenCost = "2	0	0	3	0	20	0\n";
        assertEquals(expectedGenCost, genCostOutput);

        // Check baseMVA inputs
        fileName = baseUrl + "/baseMVA.txt";
        file = new File(fileName);
        assertTrue(file.exists());
        String baseMVA = FileUtil.readFileLocally(fileName);
        String expectedBaseMVA = "1";
        assertEquals(expectedBaseMVA, baseMVA);
    }

    @Test
    public void testProcessBusInput() {
        OPFAgent agent = new OPFAgent(){
            @Override
            public JSONArray callAccessAgentToQuery(String targetResourceID, String sparqlQuery) {
                return mockStore.executeQuery(sparqlQuery);
            }

            @Override
            public List<String[]> readTimeSeriesData(List<String[]> dataIRIs, OffsetDateTime time, int numOfBus, String hasSolar) {
                String[] busLoad = {"0", "0"};
                List<String[]> busList = new ArrayList<String[]>();
                busList.add(busLoad);
                return busList;
            }
        };

        String[] busInfo = {"1", "3", "http://www.ontology-of-units-of-measure.org/resource/om-2/sample_IRI_Pd", 
                            "http://www.ontology-of-units-of-measure.org/resource/om-2/sample_IRI_Qd", "0", "0", 
                            "1", "1.0", "0", "11", "1", "1", "1"};
        List<String[]> busInput = new ArrayList<String[]>();
        busInput.add(busInfo);
        String time = "2020-01-01T08:00:00+00:00";

        List<String[]> expectedOutput = new ArrayList<String[]>();
        String[] outputBusInfo = {"1", "3", "0", "0", "0", "0", "1", "1.0", "0", "11", "1", "1", "1"};
        expectedOutput.add(outputBusInfo);

        List<String[]> actualOutput = agent.processBusInput(busInput, time);
        assertArrayEquals(expectedOutput.get(0), actualOutput.get(0));
    }

    @Test
    public void testProcessBusInputWithSolar() {
        OPFAgent agent = new OPFAgent(){
            @Override
            public JSONArray callAccessAgentToQuery(String targetResourceID, String sparqlQuery) {
                return mockStore.executeQuery(sparqlQuery);
            }

            @Override
            public List<String[]> readTimeSeriesData(List<String[]> dataIRIs, OffsetDateTime time, int numOfBus, String hasSolar) {
                String[] busLoad = {"1", "4", "3"};
                List<String[]> busList = new ArrayList<String[]>();
                busList.add(busLoad);
                return busList;
            }
        };

        String[] busInfo = {"1", "3", "http://www.ontology-of-units-of-measure.org/resource/om-2/sample_IRI_Pd", 
                            "http://www.ontology-of-units-of-measure.org/resource/om-2/sample_IRI_Qd", 
                            "http://www.ontology-of-units-of-measure.org/resource/om-2/sample_IRI_solarPd", 
                            "0", "0", "1", "1.0", "0", "11", "1", "1", "1"};
        List<String[]> busInput = new ArrayList<String[]>();
        busInput.add(busInfo);
        String time = "2020-01-01T08:00:00+00:00";

        List<String[]> expectedOutput = new ArrayList<String[]>();
        String[] outputBusInfo = {"1", "3", "-2.000000000", "4", "0", "0", "1", "1.0", "0", "11", "1", "1", "1"};
        expectedOutput.add(outputBusInfo);

        List<String[]> actualOutput = agent.processBusInputWithSolar(busInput, time);
        assertArrayEquals(expectedOutput.get(0), actualOutput.get(0));
    }

    @Test
    public void testQueryBusOutputIRIs() throws IOException {
        OPFAgent agent = new OPFAgent(){
            @Override
            public JSONArray callAccessAgentToQuery(String targetResourceID, String sparqlQuery) {
                return mockStore.executeQuery(sparqlQuery);
            }
        };   

        File busMapping = tempFolder.newFile("mappingforbus.csv");
        FileWriter mappingWriter = new FileWriter(busMapping);
        mappingWriter.write("1.0,3,bus\n" + "4.0,2,bus\n");
        mappingWriter.close();

        String[] busResult = {"3", "1.0", "0.0", "1.2881944114750332", "1.3084762183180412", "123", "321"};
        String baseUrl = tempFolder.getRoot().getAbsolutePath();

        List<String> actualOutput = agent.queryBusOutputIRIs("", busResult, baseUrl);
        assertEquals("http://www.ontology-of-units-of-measure.org/resource/om-2/sample_IRI_VoltMag", actualOutput.get(0));
        assertEquals("http://www.ontology-of-units-of-measure.org/resource/om-2/sample_IRI_VoltAngle", actualOutput.get(1));
    }

    @Test 
    public void testExtractTripleInArray() throws IOException {
        String baseUrl = tempFolder.getRoot().getAbsolutePath();
        String query = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
		+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
		+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
		+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
		+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
		+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
		+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
		+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
		+ "SELECT ?entity ?formatvalue ?startupcostvalue ?shutdowncostvalue ?gencostnvalue ?gencostn1value ?gencostn2value ?gencostcvalue "

		+ "WHERE {?entity  a  j1:PowerGenerator  ." 
		+ "?entity   j2:isModeledBy ?model ."

		+ "?model   j5:hasModelVariable ?format ." 
		+ "?format  a  j3:CostModel  ."
		+ "?format  j2:hasValue ?vformat ." 
		+ "?vformat  j2:numericalValue ?formatvalue ."

		+ "?model   j5:hasModelVariable ?startup ." 
		+ "?startup  a  j3:StartCost  ."
		+ "?startup  j2:hasValue ?vstartup ." 
		+ "?vstartup   j2:numericalValue ?startupcostvalue ."

		+ "?model   j5:hasModelVariable ?shutdown ." 
		+ "?shutdown  a  j3:StopCost  ."
		+ "?shutdown  j2:hasValue ?vshutdown ." 
		+ "?vshutdown   j2:numericalValue ?shutdowncostvalue ."

		+ "?model   j5:hasModelVariable ?gencostn ." 
		+ "?gencostn  a  j3:genCostn  ."
		+ "?gencostn  j2:hasValue ?vgencostn ." 
		+ "?vgencostn   j2:numericalValue ?gencostnvalue ."

		+ "?model   j5:hasModelVariable ?gencostn1 ." 
		+ "?gencostn1  a  j3:genCostcn-1  ."
		+ "?gencostn1  j2:hasValue ?vgencostn1 ." 
		+ "?vgencostn1   j2:numericalValue ?gencostn1value ."

		+ "?model   j5:hasModelVariable ?gencostn2 ." 
		+ "?gencostn2  a  j3:genCostcn-2  ."
		+ "?gencostn2  j2:hasValue ?vgencostn2 ." 
		+ "?vgencostn2   j2:numericalValue ?gencostn2value ."

		+ "?model   j5:hasModelVariable ?gencostc ." 
		+ "?gencostc  a  j3:genCostc0  ."
		+ "?gencostc  j2:hasValue ?vgencostc ." 
		+ "?vgencostc   j2:numericalValue ?gencostcvalue ." 

		+ "}";

        String genCostKeys[] = new String[] {"entity", "formatvalue", "startupcostvalue", "shutdowncostvalue", 
                                            "gencostnvalue", "gencostn1value", "gencostn2value", "gencostcvalue"};

        OPFAgent agent = new OPFAgent(){@Override
            public JSONArray callAccessAgentToQuery(String targetResourceID, String sparqlQuery) {
                return mockStore.executeQuery(sparqlQuery);
            }
        }; 

        List<String[]> actualOutput = agent.extractTripleInArray("", query, genCostKeys, "generatorCost", baseUrl);
        String[] expectedOutput = {"http://localhost:8080/powernetwork/EGen-001.owl#EGen-001", "2", "0", "0", "3", "0", "20", "0"};
        assertArrayEquals(expectedOutput, actualOutput.get(0));
    }

    @Test
    public void testConvertJSONArraytoList() throws JSONException {
        JSONObject jsonObject1 = new JSONObject()   
                            .put("busNumber", "1")
                            .put("areaNumber", "10");
        JSONObject jsonObject2 = new JSONObject()
                            .put("busNumber", "2")
                            .put("areaNumber", "13");
        JSONArray jsonArray = new JSONArray()
                            .put(jsonObject1)
                            .put(jsonObject2);

        String[] keys = new String[] {"busNumber", "areaNumber"};
        OPFAgent agent = new OPFAgent();
        List<String[]> result = agent.convertJSONArraytoList(jsonArray, keys);

        String[] expectedBus1 = {"1", "10"};
        String[] expectedBus2 = {"2", "13"};

        assertArrayEquals(expectedBus1, result.get(0));
        assertArrayEquals(expectedBus2, result.get(1));
    }

    // This function is added to be used by the unit tests below 
	// Read file content as a list of String arrays (csv compatible) 
	public List<String[]> readResult(String baseUrl,String filename) throws IOException {
        String outputFile = baseUrl + "/" + filename;
        String csv = FileUtil.readFileLocally(outputFile);
        List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
		
		return simulationResult;
	}

    @Test
    public void testGenerateMapping() throws IOException {
        String[] bus1 = {"3", "10"};
        String[] bus2 = {"4", "13"};
        List<String[]> inputList = new ArrayList<String[]>();
        inputList.add(bus1);
        inputList.add(bus2);
        String[] keys = new String[] {"busNumber", "areaNumber"};
        String baseUrl = tempFolder.getRoot().getAbsolutePath();

        OPFAgent agent = new OPFAgent();
        agent.generateMapping(inputList, keys, "bus_testGenerateMapping", baseUrl);

        // Check that the mapping file is generated
        String fileName = baseUrl + "/mappingforbus_testGenerateMapping.csv";
        File file1 = new File(fileName);
        assertTrue(file1.exists());

        // Check that the mapping is correct
        List<String[]> resultList = readResult(baseUrl, "mappingforbus_testGenerateMapping.csv"); 
        assertEquals(2, resultList.size());
        String[] expectedArray1 = {"3", "1", "bus_testGenerateMapping"};
        assertArrayEquals(expectedArray1, resultList.get(0));
        String[] expectedArray2 = {"4", "2", "bus_testGenerateMapping"};
        assertArrayEquals(expectedArray2, resultList.get(1));
    }

    @Test(expected = JPSRuntimeException.class)
    public void testRunPythonScriptFileNotFound() throws Exception {
        String script = "PyPower-PF-OPF-JA-9-Java-2.py";
        String[] fileNames = {"/testPy_baseMVA.txt", "/testPy_bus.txt", "/testPy_gen.txt", "/testPy_branch.txt", 
        "/testPy_outputBusOPF.txt", "/testPy_outputBranchOPF.txt", "/testPy_outputGenOPF.txt", "/testPy_areas.txt", 
        "/testPy_genCost.txt", "/testPy_outputStatus.txt"};
        String baseUrl = tempFolder.getRoot().getAbsolutePath();

        OPFAgent agent = new OPFAgent();
        agent.runPythonScript(script, baseUrl, fileNames);
    }

    // This function is added to be used by the unit tests below 
	// Convert list of string arrays to list of double arrays 
    public List<double[]> convertStringArrayListtoDouble(List<String[]> original) {
        List<double[]> result = new ArrayList<double[]>();
        for (int i = 0; i < original.size(); i++) {
            String[] splitedStrings;
            splitedStrings = original.get(i)[0].split("\t");
            double[] doubleArray = new double[splitedStrings.length];
            for (int j = 0; j < splitedStrings.length; j++) {
                doubleArray[j] = Double.parseDouble(splitedStrings[j]);
            }
            result.add(doubleArray);
        }
        return result;
    }

    // This function is added to be used by the unit tests below 
	// Convert string array to list of double arrays 
    public List<double[]> convertStringArraytoDouble(String[] original) {
        List<double[]> result = new ArrayList<double[]>();
        for (int i = 0; i < original.length; i++) {
            String[] splitedStrings;
            splitedStrings = original[i].split("\t");
            double[] doubleArray = new double[splitedStrings.length];
            for (int j = 0; j < splitedStrings.length; j++) {
                doubleArray[j] = Double.parseDouble(splitedStrings[j]);
            }
            result.add(doubleArray);
        }
        return result;
    }

    // Pypower and Scipy package need to be installed before running this test.
    @Test
    public void testRunPythonScript() throws Exception {
        File baseMVAFile = tempFolder.newFile("testPy_baseMVA.txt");
        File busFile = tempFolder.newFile("testPy_bus.txt");
        File branchFile = tempFolder.newFile("testPy_branch.txt");
        File genFile = tempFolder.newFile("testPy_gen.txt");
        File genCostFile = tempFolder.newFile("testPy_genCost.txt");

        FileWriter writer = new FileWriter(baseMVAFile);
        writer.write("1");
        writer.close();

        writer = new FileWriter(busFile);
        writer.write("1	1	0.14	0.1428286	0	0	1	1.0	0	11	1	1.1	0.9\n"
                + "2	1	0.07	0.0714143	0	0	1	1.0	0	11	1	1.1	0.9\n"
                + "3	1	0.07	0.0714143	0	0	1	1.0	0	11	1	1.1	0.9\n"
                + "4	1	0.07	0.0714143	0	0	1	1.0	0	11	1	1.1	0.9\n"
                + "5	1	0.0441	0.044991	0	0	1	1.0	0	11	1	1.1	0.9\n"
                + "6	1	0.14	0.1428286	0	0	1	1.0	0	11	1	1.1	0.9\n"
                + "7	1	0.07	0.0714143	0	0	1	1.0	0	11	1	1.1	0.9\n"
                + "8	1	0.0441	0.044991	0	0	1	1.0	0	11	1	1.1	0.9\n"
                + "9	3	0	0	0	0	1	1.0	0	11	1	1	1\n"
                + "10	1	0.0441	0.044991	0	0	1	1.0	0	11	1	1.1	0.9\n"
                + "11	1	0.07	0.0714143	0	0	1	1.0	0	11	1	1.1	0.9\n"
                + "12	1	0.14	0.1428286	0	0	1	1.0	0	11	1	1.1	0.9\n"
                + "13	1	0.0441	0.044991	0	0	1	1.0	0	11	1	1.1	0.9\n"
                + "14	1	0.14	0.1428286	0	0	1	1.0	0	11	1	1.1	0.9\n"
                + "15	1	0.14	0.1428286	0	0	1	1.0	0	11	1	1.1	0.9\n");
        writer.close();

        writer = new FileWriter(branchFile);
        writer.write("4	5	0.013939752	0.009402479	0.0	9900	0	0	0	0	1	-360	360\n"
                + "7	8	0.016637769	0.011222314	0.0	9900	0	0	0	0	1	-360	360\n"
                + "12	2	0.018436446	0.012435537	0.0	9900	0	0	0	0	1	-360	360\n"
                + "12	1	0.009892727	0.006672727	0.0	9900	0	0	0	0	1	-360	360\n"
                + "10	14	0.021134463	0.014255372	0.0	9900	0	0	0	0	1	-360	360\n"
                + "14	15	0.008993388	0.006066116	0.0	9900	0	0	0	0	1	-360	360\n"
                + "14	3	0.010342397	0.006976033	0.0	9900	0	0	0	0	1	-360	360\n"
                + "11	6	0.014839091	0.010009091	0.0	9900	0	0	0	0	1	-360	360\n"
                + "6	7	0.020235124	0.01364876	0.0	9900	0	0	0	0	1	-360	360\n"
                + "9	10	0.011182562	0.010937934	0.0	9900	0	0	0	0	1	-360	360\n"
                + "10	11	0.009671405	0.009459835	0.0	9900	0	0	0	0	1	-360	360\n"
                + "11	12	0.006951322	0.006799256	0.0	9900	0	0	1	0	1	-360	360\n"
                + "12	13	0.012590744	0.008492562	0.0	9900	0	0	0	0	1	-360	360\n"
                + "10	4	0.016637769	0.011222314	0.0	9900	0	0	0	0	1	-360	360\n");
        writer.close();

        writer = new FileWriter(genFile);
        writer.write("9	1.288194411474922	1.3084762183180216	10.0	-10.0	1	100	1	10.0	0	0	0	0	0	0	0	0	0	0	0	0\n");
        writer.close();

        writer = new FileWriter(genCostFile);
        writer.write("2.0	0.0	0.0	3	0	20	0\n");
        writer.close();

        String script = "PyPower-PF-OPF-JA-9-Java-2.py";
        String[] fileNames = {"/testPy_baseMVA.txt", "/testPy_bus.txt", "/testPy_gen.txt", "/testPy_branch.txt", 
        "/testPy_outputBusOPF.txt", "/testPy_outputBranchOPF.txt", "/testPy_outputGenOPF.txt", "/testPy_areas.txt", 
        "/testPy_genCost.txt", "/testPy_outputStatus.txt"};
        String baseUrl = tempFolder.getRoot().getAbsolutePath();

        OPFAgent agent = new OPFAgent();
        agent.runPythonScript(script, baseUrl, fileNames);

        // Check that OPF runs successfully with output status "Converged"
        String fileName = baseUrl + "/testPy_outputStatus.txt";
        BufferedReader input = new BufferedReader(new FileReader(fileName));
        String last, line;
        last = "";
        while ((line = input.readLine()) != null) { 
            last = line;
        }
        assertTrue(last.contains("Converged")); 
                    
        // Check that the output files are made
        File branchOutFile = new File(baseUrl +"/testPy_outputBranchOPF.txt");
        assertTrue(branchOutFile.exists());
        File busOutFile = new File(baseUrl +"/testPy_outputBusOPF.txt");
        assertTrue(busOutFile.exists());
        File genOutFile = new File(baseUrl +"/testPy_outputGenOPF.txt");
        assertTrue(genOutFile.exists());

    	// Check that the number of rows are the same for inputs and outputs
		List<String[]> branchIn = readResult(baseUrl, "testPy_branch.txt"); 
		List<String[]> branchOut = readResult(baseUrl, "testPy_outputBranchOPF.txt");
		assertEquals(branchIn.size(), branchOut.size());

		List<String[]> busIn = readResult(baseUrl, "testPy_bus.txt"); 
		List<String[]> busOut = readResult(baseUrl, "testPy_outputBusOPF.txt");
		assertEquals(busIn.size(), busOut.size());

		List<String[]> genIn = readResult(baseUrl, "testPy_gen.txt"); 
		List<String[]> genOut = readResult(baseUrl, "testPy_outputGenOPF.txt");
		assertEquals(genIn.size(), genOut.size());

        // Check output OPF results of branches
        String[] expectedBranch = {"1	5.9180102141863367e-05	3.991747253512257e-05	2.9590051070931683e-05	1.9958736267561283e-05	3.5692047794689664e-05",
                                        "2	7.402129132406954e-05	4.992797856036291e-05	3.701064566203477e-05	2.4963989280181453e-05	4.464290148614567e-05",
                                        "3	0.00020488203840132724	0.00013819464820798155	0.00010244101920066362	6.909732410399078e-05	0.0001235661871759531",
                                        "4	0.00043990307698940634	0.00029671830014213474	0.00021995153849470317	0.00014835915007106737	0.0002653094734380896",
                                        "5	0.005768060851772838	0.003890605271620051	0.002884030425886419	0.0019453026358100256	0.003478769012499685",
                                        "6	0.0003936048439138473	0.0002654897844219517	0.00019680242195692366	0.00013274489221097585	0.00023738660386848838",
                                        "7	0.00011293772372396316	7.617743620198791e-05	5.646886186198158e-05	3.808871810099396e-05	6.81137490273773e-05",
                                        "8	0.00217633516337723	0.0014679562714955408	0.001088167581688615	0.0007339781357477704	0.0013125671752690683",
                                        "9	0.0006016202212425908	0.0004057978597456352	0.0003008101106212954	0.0002028989298728176	0.00036284252561618317",
                                        "10	0.037702621209810605	0.03687784448858067	0.018851310604905303	0.018438922244290334	0.026369788870099053",
                                        "11	0.011289757566546044	0.01104278476286813	0.005644878783273022	0.005521392381434065	0.007896228866212279",
                                        "12	0.0024439142032758876	0.002390451529954929	0.0012219571016379438	0.0011952257649774645	0.001709310910135827",
                                        "13	5.5380952839606534e-05	3.735491529407997e-05	2.7690476419803267e-05	1.8677457647039986e-05	3.340074712506673e-05",
                                        "14	0.0004721922296472142	0.000318497598413639	0.0002360961148236071	0.0001592487992068195	0.000284783348325733"};
        List<double[]> actualBranchOutput = convertStringArrayListtoDouble(branchOut);
        List<double[]> expectedBranchOutput = convertStringArraytoDouble(expectedBranch);
        for (int i = 0; i < branchIn.size(); i++) {
            assertArrayEquals(expectedBranchOutput.get(i), actualBranchOutput.get(i), 0.000001);
        }

        // Check output OPF results of buses
        String[] expectedBus = {"1	0.9484393336941132	0.08693266395217579	0.0	0.0	0.14	0.1428286", 
                                    "2	0.9486078372696072	0.0848537514911073	0.0	0.0	0.07	0.0714143", 
                                    "3	0.9569541625173138	0.2050264292435832	0.0	0.0	0.07	0.0714143", 
                                    "4	0.9679703457839625	0.07196194092738395	0.0	0.0	0.07	0.0714143", 
                                    "5	0.9668970214279292	0.0849716632174654	0.0	0.0	0.0441	0.044991", 
                                    "6	0.9499519353595163	0.13152814134731286	0.0	0.0	0.14	0.1428286", 
                                    "7	0.9458284078648992	0.1824254008701606	0.0	0.0	0.07	0.0714143", 
                                    "8	0.944516981204416	0.1986931782173225	0.0	0.0	0.0441	0.044991", 
                                    "9	1.0	0.0	1.28819441147497	1.308476218318052	0.0	0.0", 
                                    "10	0.9712828107953471	0.031968402501272586	0.0	0.0	0.0441	0.044991", 
                                    "11	0.9566689392304171	0.049348233506373816	0.0	0.0	0.07	0.0714143", 
                                    "12	0.9509046101111183	0.0565157973888409	0.0	0.0	0.14	0.1428286", 
                                    "13	0.9499178289039322	0.06869120866760962	0.0	0.0	0.0441	0.044991", 
                                    "14	0.9582313297055367	0.1893886646766468	0.0	0.0	0.14	0.1428286", 
                                    "15	0.9560079249383326	0.21661168656280289	0.0	0.0	0.14	0.1428286"};
        List<double[]> actualBusOutput = convertStringArrayListtoDouble(busOut);
        List<double[]> expectedBusOutput = convertStringArraytoDouble(expectedBus);
        for (int i = 0; i < busIn.size(); i++) {
            assertArrayEquals(expectedBusOutput.get(i), actualBusOutput.get(i), 0.000001);
        }
            
        // Check output OPF results of generators
        String[] expectedGen = {"1	1.28819441147497	1.308476218318052"};
        List<double[]> actualGenOutput = convertStringArrayListtoDouble(genOut);
        List<double[]> expectedGenOutput = convertStringArraytoDouble(expectedGen);
        assertArrayEquals(expectedGenOutput.get(0), actualGenOutput.get(0), 0.000001);

        input.close();
    }

    @Test(expected = BadRequestException.class)
    public void testValidateInputWithEmptyInput() {
        JSONObject requestParams = new JSONObject();
        OPFAgent agent = new OPFAgent();
        agent.validateInput(requestParams);
    }

    @Test(expected = JPSRuntimeException.class)
    public void testValidateInputWithoutIRI() {
        JSONObject requestParams = new JSONObject();
        requestParams.put("electricalnetwork", "http://localhost:8080/");
        requestParams.put("time", "2020-01-01T08:00:00+00:00");
        requestParams.put("hasSolar", "true");
        OPFAgent agent = new OPFAgent();
        agent.validateInput(requestParams);
    }

    @Test(expected = JPSRuntimeException.class)
    public void testValidateInputWithoutMVA() {
        JSONObject requestParams = new JSONObject();
        requestParams.put("baseMVA", "100");
        requestParams.put("time", "2020-01-01T08:00:00+00:00");
        requestParams.put("hasSolar", "true");
        OPFAgent agent = new OPFAgent();
        agent.validateInput(requestParams);
    }

    @Test(expected = JPSRuntimeException.class)
    public void testValidateInputWithoutSolarParam() {
        JSONObject requestParams = new JSONObject();
        requestParams.put("electricalnetwork", "http://localhost:8080/");
        requestParams.put("baseMVA", "100");
        requestParams.put("time", "2020-01-01T08:00:00+00:00");
        OPFAgent agent = new OPFAgent();
        agent.validateInput(requestParams);
    }

    @Test(expected = JPSRuntimeException.class)
    public void testValidateInputWithoutTime() {
        JSONObject requestParams = new JSONObject();
        requestParams.put("electricalnetwork", "http://localhost:8080/");
        requestParams.put("baseMVA", "100");
        requestParams.put("hasSolar", "true");
        OPFAgent agent = new OPFAgent();
        agent.validateInput(requestParams);
    }

    @Test(expected = JPSRuntimeException.class)
    public void testValidateInputInvalidTime() {
        JSONObject requestParams = new JSONObject();
        requestParams.put("electricalnetwork", "http://localhost:8080/");
        requestParams.put("baseMVA", "100");
        requestParams.put("time", "2020/01/01T08:00:00");
        requestParams.put("hasSolar", "true");
        OPFAgent agent = new OPFAgent();
        agent.validateInput(requestParams);
    }

    @Test
    public void testValidateInput() {
        JSONObject requestParams1 = new JSONObject();
        JSONObject requestParams2 = new JSONObject();
        requestParams1.put("electricalnetwork", "http://localhost:8080/");
        requestParams1.put("baseMVA", "100");
        requestParams1.put("time", "2020-01-01T08:00:00+00:00");
        requestParams1.put("hasSolar", "true");
        requestParams2.put("electricalnetwork", "IRI");
        requestParams2.put("baseMVA", "100");
        requestParams2.put("time", "2020-01-01T08:00:00+00:00");
        requestParams2.put("hasSolar", "false");

        OPFAgent agent = new OPFAgent();
        // Valid IRI input
        assertTrue(agent.validateInput(requestParams1));
        // Invalid IRI input
        assertFalse(agent.validateInput(requestParams2));
    }

    @Test
    public void testFromCsvToArray() throws IOException {
        File inputCsvFile = tempFolder.newFile("testFromCsvToArray.csv");
        FileWriter csvWriter = new FileWriter(inputCsvFile);
        csvWriter.write("bus,busnumber,bustype\n" + "branch,frombus,tobus\n");
        csvWriter.close();
        
        String[] expetedFirstLine = {"bus,busnumber,bustype"};
        String[] expectedSecondLine = {"branch,frombus,tobus"};

        String inputCsv = tempFolder.getRoot().getAbsolutePath() + "/testFromCsvToArray.csv";
        String csv = FileUtil.readFileLocally(inputCsv);
        OPFAgent agent = new OPFAgent();
        List<String[]> actualList = agent.fromCsvToArray(csv);

        assertArrayEquals(expetedFirstLine, actualList.get(0));
        assertArrayEquals(expectedSecondLine, actualList.get(1));
    }

    @Test
    public void testReadResult() throws IOException {
        File inputCsvFile = tempFolder.newFile("testReadResult.csv");
        FileWriter csvWriter = new FileWriter(inputCsvFile);
        csvWriter.write("bus,busnumber,bustype\n" + "branch,frombus,tobus\n");
        csvWriter.close();

        String[] expetedFirstLine = {"bus,busnumber,bustype"};
        String[] expectedSecondLine = {"branch,frombus,tobus"};

        OPFAgent agent = new OPFAgent();
        List<String[]> actualList = agent.readResult(tempFolder.getRoot().getAbsolutePath() + "/testReadResult.csv");

        assertArrayEquals(expetedFirstLine, actualList.get(0));
        assertArrayEquals(expectedSecondLine, actualList.get(1));
    }

    @Test
    public void testCreateNewTSV() throws IOException {
        OPFAgent agent = new OPFAgent();
        File genMappingFile = tempFolder.newFile("testTSV_mappingforgenerator.csv");
        File busMappingFile = tempFolder.newFile("testTSV_mappingforbus.csv");

        FileWriter writer = new FileWriter(genMappingFile);
        writer.write("http://localhost:8080/powernetwork/EGen-001.owl#EGen-001,1,generator" + "\n");
        writer.close();
        writer = new FileWriter(busMappingFile);
        writer.write("1,10,bus\n" + "14,1,bus\n" + "8,2,bus\n");
        writer.close();
        
        String genMapping = tempFolder.getRoot().getAbsolutePath() + "/testTSV_mappingforgenerator.csv";
        String busMapping = tempFolder.getRoot().getAbsolutePath() + "/testTSV_mappingforbus.csv";

        // Check bus result
        String expectedBusOutput = "1	1	0.07	0.0714143	0	0	1	1.0	0	11	1	1.1	0.9" + "\n"
                                + "2	1	0.14	0.1428286	0	0	1	1.0	0	11	1	1.1	0.9" + "\n";
        List<String[]> busList = new ArrayList<String[]>();
        String[] bus1Info = {"14", "1", "0.07", "0.0714143", "0", "0", "1", "1", "0", "11", "1", "1.1", "0.9"};
        String[] bus2Info = {"8", "1", "0.14", "0.1428286", "0", "0", "1", "1", "0", "11", "1", "1.1", "0.9"};
        busList.add(bus1Info);
        busList.add(bus2Info);
        String actualBusOutput = agent.createNewTSV(busList, busMapping, busMapping);
        assertEquals(expectedBusOutput, actualBusOutput);

        // Check non-bus result
        String expectedGenOutput = "10	1.2881944114749324	1.3084762183180156	10.0	-10.0	1	" 
                            + "100	1	10.0	0	0	0	0	0	0	0	0	0	0	0	0" + "\n";
        List<String[]> genList = new ArrayList<String[]>();
        String[] genInfo = {"http://localhost:8080/powernetwork/EGen-001.owl#EGen-001", "1", "1.2881944114749324", 
                            "1.3084762183180156", "10.0", "-10.0", "1", "100", "1", "10.0", "0", "0", "0", 
                            "0", "0", "0", "0", "0", "0", "0", "0", "0"};
        genList.add(genInfo);
        String actualGenOutput = agent.createNewTSV(genList, genMapping, busMapping);
        assertEquals(expectedGenOutput, actualGenOutput);
    }

    @Test
    public void testCreateDummyValueTSV() throws IOException {
        List<String[]> batteryList = new ArrayList<String[]>();
        String[] batteryInfo = {"batteryIRI", "1.28"};
        batteryList.add(batteryInfo);
        String expectedOutput = "0	0	0	1.28	0	0	0	0	0	0	0	" 
                                + "0	0	0	0	0	0	0	0	0	0" + "\n";

        OPFAgent agent = new OPFAgent();
        String actualOutput = agent.createDummyValueTSV(batteryList);
        assertEquals(expectedOutput, actualOutput);
    }

    /**
     * Add triples to mock store (The mock store contains 1 bus, 1 branch and 1 generator)
     * This method is added to test the correctness of SPARQL queries
     * @param mockStore
     * @return
     */
    public MockStoreClient populateMockStore(MockStoreClient mockStore) {
        // Add triples for 1 bus
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#BusNode>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isModeledBy>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001>");

        // BusNumber
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

        // Type
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#BusType_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#BusType_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusType>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#BusType_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#V_BusType_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#V_BusType_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "3");

        // Pd IRI
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasActivePowerAbsorbed>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#Pd_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Pd_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedActivePower>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Pd_EBus-001>", 
                            "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue>", 
                            "<http://www.ontology-of-units-of-measure.org/resource/om-2/sample_IRI_Pd>");

        // Solar PV
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Building-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#Building>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Building-001>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#hasBusNode>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001>");

        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Building-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#contains>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#solarPV_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#solarPV_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PhotovoltaicPanel>");

        // solarPd IRI
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#solarPV_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasGeneratedPower>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#solarPd_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#solarPd_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#GeneratedPower>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#solarPd_EBus-001>", 
                            "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue>", 
                            "<http://www.ontology-of-units-of-measure.org/resource/om-2/sample_IRI_solarPd>");

        // Gd IRI
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasReactivePowerAbsorbed>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#Gd_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Gd_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedReactivePower>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Gd_EBus-001>", 
                            "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue>", 
                            "<http://www.ontology-of-units-of-measure.org/resource/om-2/sample_IRI_Qd>");

        // Gs
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#Gs_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Gs_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Gs>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Gs_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#V_Gs_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#V_Gs_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0");

        // Bs
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#Bs_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Bs_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Bs>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Bs_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#V_Bs_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#V_Bs_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0");

        // Area
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#Area_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Area_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Area>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Area_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#V_Area_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#V_Area_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "1");

        // Vm
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#Vm_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Vm_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Vm>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Vm_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#V_Vm_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#V_Vm_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "1.0");

        // Va
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#Va_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Va_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Va>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Va_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#V_Va_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#V_Va_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0.0");

        // BaseKV
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#BaseKV_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#BaseKV_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#baseKV>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#BaseKV_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#V_BaseKV_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#V_BaseKV_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "11");

        // Zone
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#Zone_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Zone_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Zone>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Zone_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#V_Zone_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#V_Zone_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "1");

        // VmMax
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#VmMax_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#VmMax_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#VmMax>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#VmMax_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#V_VmMax_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#V_VmMax_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "1");

        // VmMin
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#VmMin_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#VmMin_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#VmMin>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#VmMin_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#V_VmMin_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#V_VmMin_EBus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "1");

        // Voltage Magnitude IRI
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasVoltageMagnitude>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#VoltMag_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#VoltMag_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#VoltageMagnitude>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#VoltMag_EBus-001>", 
                            "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue>", 
                            "<http://www.ontology-of-units-of-measure.org/resource/om-2/sample_IRI_VoltMag>");

        // Voltage Angle IRI
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasVoltageAngle>", 
                            "<http://localhost:8080/powernetwork/EBus-001.owl#VoltAngle_EBus-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#VoltAngle_EBus-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#VoltageAngle>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EBus-001.owl#VoltAngle_EBus-001>", 
                            "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue>", 
                            "<http://www.ontology-of-units-of-measure.org/resource/om-2/sample_IRI_VoltAngle>");

        // Add triples for 1 branch
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#ELine-013>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#UndergroundCable>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isModeledBy>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013>");

        // Frombus
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#FromBusNumber_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#FromBusNumber_ELine-013>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusFrom>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#FromBusNumber_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#V_FromBusNumber_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#V_FromBusNumber_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "\'1.0\'");

        // Tobus
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#ToBusNumber_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#ToBusNumber_ELine-013>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusTo>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#ToBusNumber_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#V_ToBusNumber_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#V_ToBusNumber_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "\'1.0\'");

        // R
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#R_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#R_ELine-013>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#R>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#R_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#V_R_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#V_R_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0.018436446");

        // X
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#X_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#X_ELine-013>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#X>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#X_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#V_X_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#V_X_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0.012435537");

        // B
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#B_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#B_ELine-013>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#B>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#B_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#V_B_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#V_B_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0.0");

        // rateA
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#RateA_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#RateA_ELine-013>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#RateA>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#RateA_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#V_RateA_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#V_RateA_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "9900");
                            
        // rateB
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#RateB_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#RateB_ELine-013>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#RateB>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#RateB_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#V_RateB_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#V_RateB_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0"); 

        // rateC
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#RateC_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#RateC_ELine-013>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#RateC>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#RateC_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#V_RateC_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#V_RateC_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0"); 

        // Ratio coefficient
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#RatioCoeff_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#RatioCoeff_ELine-013>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#RatioCoefficient>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#RatioCoeff_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#V_RatioCoeff_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#V_RatioCoeff_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0"); 

        // Angle
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#Angle_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#Angle_ELine-013>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Angle>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#Angle_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#V_Angle_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#V_Angle_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0"); 

        // Status
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#Status_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#Status_ELine-013>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BranchStatus>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#Status_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#V_Status_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#V_Status_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "1"); 

        // AngleMin
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#AngleMin_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#AngleMin_ELine-013>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#AngleMin>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#AngleMin_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#V_AngleMin_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#V_AngleMin_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "-360"); 

        // AngleMax
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#AngleMax_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#AngleMax_ELine-013>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#AngleMax>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#AngleMax_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/ELine-013.owl#V_AngleMax_ELine-013>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/ELine-013.owl#V_AngleMax_ELine-013>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "360"); 

        // Add triples for 1 generator
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PowerGenerator>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isModeledBy>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>");

        // busNumber
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#BusNumber_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#BusNumber_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusNumber>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#BusNumber_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_BusNumber_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_BusNumber_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "\'1.0\'"); 

        // Pg
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#PGen_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#PGen_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Pg>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#PGen_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_PGen_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_PGen_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0.0"); 

        // Qg
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#QGen_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#QGen_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Qg>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#QGen_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_QGen_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_QGen_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0.0"); 

        // Qmax
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#Qmax_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Qmax_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QMax>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Qmax_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_Qmax_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_Qmax_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "10.0"); 

        // Qmin
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#Qmin_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Qmin_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QMin>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Qmin_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_Qmin_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_Qmin_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "-10.0"); 

        // Vg
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#Vg_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Vg_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Vg>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Vg_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_Vg_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_Vg_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "1"); 

        // mBase
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#mBase_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#mBase_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#mBase>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#mBase_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_mBase_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_mBase_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "100"); 

        // Status
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#Status_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Status_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Status>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Status_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_Status_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_Status_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "1"); 

        // Pmax
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#PMax_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#PMax_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#PMax>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#PMax_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_PMax_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_PMax_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "10.0");           

        // Pmin
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#PMin_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#PMin_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#PMin>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#PMin_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_PMin_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_PMin_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0");                   

        // Pc1
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#Pc1_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Pc1_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Pc1>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Pc1_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_Pc1_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_Pc1_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0");     

        // Pc2
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#Pc2_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Pc2_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Pc2>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Pc2_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_Pc2_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_Pc2_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0");      

        // Qc1min
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#QC1Min_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#QC1Min_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QC1Min>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#QC1Min_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_QC1Min_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_QC1Min_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0");                              

        // Qc1max
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#QC1Max_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#QC1Max_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QC1Max>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#QC1Max_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_QC1Max_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_QC1Max_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0");      

        // Qc2min
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#QC2Min_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#QC2Min_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QC2Min>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#QC2Min_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_QC2Min_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_QC2Min_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0");                              

        // Qc2max
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#QC2Max_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#QC2Max_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QC2Max>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#QC2Max_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_QC2Max_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_QC2Max_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0");                               

        // Rampagc
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#Rampagc_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Rampagc_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Rampagc>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Rampagc_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_Rampagc_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_Rampagc_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0"); 

        // Ramp10
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#Ramp10_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Ramp10_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Ramp10>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Ramp10_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_Ramp10_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_Ramp10_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0"); 

        // Ramp30
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#Ramp30_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Ramp30_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Ramp30>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Ramp30_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_Ramp30_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_Ramp30_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0"); 

        // Rampq
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#Rampq_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Rampq_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Rampq>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Rampq_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_Rampq_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_Rampq_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0"); 

        // APF
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#APF_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#APF_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#APF>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#APF_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_APF_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_APF_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0"); 

        // GenCost Info
        // Cost Model
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#Format_CostEq_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Format_CostEq_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#CostModel>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Format_CostEq_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_Format_CostEq_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_Format_CostEq_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "2"); 

        // Startup cost
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#StartupCost_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#StartupCost_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#StartCost>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#StartupCost_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_StartupCost_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_StartupCost_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0"); 

        // Shutdown cost
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#ShutdownCost_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#ShutdownCost_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#StopCost>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#ShutdownCost_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_ShutdownCost_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_ShutdownCost_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0"); 

        // genCostn
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#genCostn_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#genCostn_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#genCostn>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#genCostn_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_genCostn_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_genCostn_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "3"); 

        // genCostcn-1
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#genCostcn-1_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#genCostcn-1_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#genCostcn-1>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#genCostcn-1_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_genCostcn-1_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_genCostcn-1_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0"); 

        // genCostcn-2
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#genCostcn-2_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#genCostcn-2_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#genCostcn-2>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#genCostcn-2_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_genCostcn-2_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_genCostcn-2_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "20"); 

        // genCostc0
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#genCostc0_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#genCostc0_EGen-001>", 
                            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", 
                            "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#genCostc0>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#genCostc0_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue>", 
                            "<http://localhost:8080/powernetwork/EGen-001.owl#V_genCostc0_EGen-001>");
        mockStore.addTriple("<http://localhost:8080/powernetwork/EGen-001.owl#V_genCostc0_EGen-001>", 
                            "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue>", 
                            "0"); 

        return mockStore;
    }
}
