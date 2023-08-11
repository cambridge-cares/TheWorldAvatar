package uk.ac.cam.cares.jps.agent.cea.tasks;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.agent.cea.data.CEAInputData;
import uk.ac.cam.cares.jps.agent.cea.data.CEAOutputData;

import kong.unirest.*;
import java.io.*;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.OffsetDateTime;
import java.util.*;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

public class RunCEATaskTest {
    @Test
    public void testRunCEATask() {
        RunCEATask task;

        try {
            URI testURI = new URI("http://localhost/test");
            ArrayList<CEAInputData> testData = new ArrayList<CEAInputData>();
            testData.add(new CEAInputData("test", "test", (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00), null, null, null, null));
            ArrayList<String> testArray = new ArrayList<>();
            testArray.add("testUri");
            Integer test_thread = 0;
            String test_CRS = "27700";
            task = new RunCEATask(testData, testURI, testArray, test_thread, test_CRS, null);;
            assertNotNull(task);
        } catch (Exception e) {
            fail();
        }
    }

    @Test
    public void testStop() {
        try {
            URI testURI = new URI("http://localhost/test");
            ArrayList<CEAInputData> testData = new ArrayList<CEAInputData>();
            testData.add(new CEAInputData("test", "test", (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00), null, null, null, null));
            ArrayList<String> testArray = new ArrayList<>();
            testArray.add("testUri");
            Integer test_thread = 0;
            String test_CRS = "27700";
            RunCEATask task = new RunCEATask(testData, testURI, testArray, test_thread, test_CRS, null);;

            Field stopField = task.getClass().getDeclaredField("stop");
            stopField.setAccessible(true);
            assertFalse((Boolean) stopField.get(task));
            Method stopMethod = task.getClass().getDeclaredMethod("stop");
            stopMethod.setAccessible(true);
            stopMethod.invoke(task);
            assertTrue((Boolean) stopField.get(task));
        } catch (NoSuchFieldException | IllegalAccessException | NoSuchMethodException | InvocationTargetException | URISyntaxException e) {
            fail();
        }
    }

    @Test
    public void testRunProcess() throws Exception {
        URI testURI = new URI("http://localhost/test");
        ArrayList<CEAInputData> testData = new ArrayList<CEAInputData>();
        testData.add(new CEAInputData("test", "test", (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00), null, null, null, null));
        ArrayList<String> testArray = new ArrayList<>();
        testArray.add("testUri");
        Integer test_thread = 0;
        String test_CRS = "27700";

        Process dummyProcess  = mock(Process.class);

        RunCEATask task = Mockito.spy(new RunCEATask(testData, testURI, testArray, test_thread, test_CRS, null));
        try (MockedConstruction<ProcessBuilder> builder = mockConstruction(ProcessBuilder.class,  (mock, context) -> {
            when(mock.start()).thenReturn(dummyProcess);
        })) {
            try (MockedConstruction<BufferedReader> reader = mockConstruction(BufferedReader.class, (mock, context) -> {
                when(mock.read()).thenReturn(-1);
            })) {
                try (MockedConstruction<InputStreamReader> streamReader = mockConstruction(InputStreamReader.class)) {

                    Method runProcess = task.getClass().getDeclaredMethod("runProcess", ArrayList.class);

                    ArrayList<String> args = new ArrayList<>();
                    args.add("test");
                    Process result = (Process) runProcess.invoke(task, args);

                    assertEquals(dummyProcess, result);

                    verify(builder.constructed().get(0), times(1)).redirectOutput(ProcessBuilder.Redirect.INHERIT);
                    verify(builder.constructed().get(0), times(1)).redirectErrorStream(true);
                    verify(builder.constructed().get(0), times(1)).start();
                    verify(reader.constructed().get(0), times(1)).close();
                    verify(dummyProcess, times(1)).waitFor();
                }
            }
        }

    }

    @Test
    public void testDeleteDirectoryContents() {
        try {
            URI testURI = new URI("http://localhost/test");
            ArrayList<CEAInputData> testData = new ArrayList<CEAInputData>();
            testData.add(new CEAInputData("test", "test", (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00), null, null, null, null));
            ArrayList<String> testArray = new ArrayList<>();
            testArray.add("testUri");
            Integer test_thread = 0;
            String test_CRS = "27700";
            RunCEATask task = new RunCEATask(testData, testURI, testArray, test_thread, test_CRS, null);;

            File myTempDir = new File(System.getProperty("java.io.tmpdir"));
            File newDirectory = new File(myTempDir, "new_directory");
            assertTrue(newDirectory.mkdir() || newDirectory.isDirectory());

            File tempFile = File.createTempFile("text", ".temp", newDirectory);

            Method deleteDirectoryContents = task.getClass().getDeclaredMethod("deleteDirectoryContents", File.class);
            deleteDirectoryContents.setAccessible(true);
            deleteDirectoryContents.invoke(task, newDirectory);

            assertFalse(tempFile.isFile());

        } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException | IOException | URISyntaxException e) {
            System.out.println(e.getMessage());
            fail();
        }
    }

    @Test
    public void testExtractArea() throws Exception {
        String PVTitle = "PV_roofs_top_m2,PV_walls_south_m2,PV_walls_north_m2,Other,PV_walls_east_m2,PV_walls_west_m2";
        String PVValues = "10.0,20.0,30.0,40.0,50.0,60.0";

        Map<String, List<String>> testCEAoutputs = new HashMap<>();
        testCEAoutputs.put("PV", Arrays.asList(PVTitle, PVValues));
        CEAOutputData data = new CEAOutputData();
        String tmpDir = "test";

        URI testURI = new URI("http://localhost/test");
        ArrayList<CEAInputData> testData = new ArrayList<CEAInputData>();
        testData.add(new CEAInputData("test", "test", (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00), null, null, null, null));
        ArrayList<String> testArray = new ArrayList<>();
        testArray.add("testUri");
        Integer test_thread = 0;
        String test_CRS = "27700";
        RunCEATask task = Mockito.spy(new RunCEATask(testData, testURI, testArray, test_thread, test_CRS, null));
        doNothing().when(task).deleteDirectoryContents(any());

        for (Map.Entry<String, List<String>> entry : testCEAoutputs.entrySet()) {
            try (MockedConstruction<FileReader> fReader = mockConstruction(FileReader.class)) {
                try (MockedConstruction<BufferedReader> bReader = mockConstruction(BufferedReader.class, (mock, context) -> {
                    when(mock.readLine()).thenReturn(entry.getValue().get(0), entry.getValue().get(1), null);
                })) {
                    Method extractArea = task.getClass().getDeclaredMethod("extractArea", String.class, CEAOutputData.class);
                    CEAOutputData result = (CEAOutputData) extractArea.invoke(task, tmpDir, data);

                    String[] testValues = entry.getValue().get(1).split(",");

                    assertTrue(result.RoofSolarSuitableArea.contains(testValues[0]));
                    assertTrue(result.SouthWallSolarSuitableArea.contains(testValues[1]));
                    assertTrue(result.NorthWallSolarSuitableArea.contains(testValues[2]));
                    assertTrue(result.EastWallSolarSuitableArea.contains(testValues[4]));
                    assertTrue(result.WestWallSolarSuitableArea.contains(testValues[5]));

                    assertTrue(result.targetUrl.contains(testURI.toString()));
                    assertTrue(result.iris.get(0).contains(testArray.get(0)));
                }
            }
        }
    }

    @Test
    public void testExtractTimeSeriesOutputs() throws Exception {
        String demandTitleRow = "GRID_kWh,QH_sys_kWh,QC_sys_kWh,Other,E_sys_kWh";
        String demandValuesRow1 = "10.0,20.0,30.0,40.0,50.0";
        String demandValuesRow2 = "11.0,21.0,31.0,41.0,51.0";

        String PVTitleRow = "Date,PV_roofs_top_E_kWh,PV_walls_south_E_kWh,Other,PV_walls_north_E_kWh,PV_walls_west_E_kWh,PV_walls_east_E_kWh";
        String PVValuesRow1 = "2005-01-01 00:00:00+00:00,20.0,30.0,40.0,50.0,60.0,70.0";
        String PVValuesRow2 = "2005-01-01 01:00:00+00:00,21.0,31.0,41.0,51.0,61.0,71.0";

        String tmpDir = "test";

        URI testURI = new URI("http://localhost/test");
        ArrayList<CEAInputData> testData = new ArrayList<CEAInputData>();
        testData.add(new CEAInputData("test", "test", (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00), null, null, null, null));
        ArrayList<String> testArray = new ArrayList<>();
        testArray.add("testUri");
        Integer test_thread = 0;
        String test_CRS = "27700";
        RunCEATask task = Mockito.spy(new RunCEATask(testData, testURI, testArray, test_thread, test_CRS, null));

        try (MockedConstruction<FileReader> fReader = mockConstruction(FileReader.class)) {
            try (MockedConstruction<BufferedReader> bReader = mockConstruction(BufferedReader.class,  (mock, context) -> {
                when(mock.readLine()).thenReturn(demandTitleRow, demandValuesRow1,demandValuesRow2, null);
            })) {
                Method extractTimeSeriesOutputs = task.getClass().getDeclaredMethod("extractTimeSeriesOutputs", String.class);
                CEAOutputData result = (CEAOutputData) extractTimeSeriesOutputs.invoke(task, tmpDir);

                assertTrue(result.GridConsumption.get(0).get(0).contains("10.0"));
                assertTrue(result.GridConsumption.get(0).get(1).contains("11.0"));
                assertTrue(result.ElectricityConsumption.get(0).get(0).contains("50.0"));
                assertTrue(result.ElectricityConsumption.get(0).get(1).contains("51.0"));
                assertTrue(result.HeatingConsumption.get(0).get(0).contains("20.0"));
                assertTrue(result.HeatingConsumption.get(0).get(1).contains("21.0"));
                assertTrue(result.CoolingConsumption.get(0).get(0).contains("30.0"));
                assertTrue(result.CoolingConsumption.get(0).get(1).contains("31.0"));
            }
            try (MockedConstruction<BufferedReader> bReader = mockConstruction(BufferedReader.class,  (mock, context) -> {
                when(mock.readLine()).thenReturn(PVTitleRow, PVValuesRow1, PVValuesRow2, null);
            })) {
                Method extractTimeSeriesOutputs = task.getClass().getDeclaredMethod("extractTimeSeriesOutputs", String.class);
                CEAOutputData result = (CEAOutputData) extractTimeSeriesOutputs.invoke(task, tmpDir);

                assertTrue(result.PVRoofSupply.get(0).get(0).contains("20.0"));
                assertTrue(result.PVRoofSupply.get(0).get(1).contains("21.0"));
                assertTrue(result.PVWallSouthSupply.get(0).get(0).contains("30.0"));
                assertTrue(result.PVWallSouthSupply.get(0).get(1).contains("31.0"));
                assertTrue(result.PVWallNorthSupply.get(0).get(0).contains("50.0"));
                assertTrue(result.PVWallNorthSupply.get(0).get(1).contains("51.0"));
                assertTrue(result.PVWallEastSupply.get(0).get(0).contains("70.0"));
                assertTrue(result.PVWallEastSupply.get(0).get(1).contains("71.0"));
                assertTrue(result.PVWallWestSupply.get(0).get(0).contains("60.0"));
                assertTrue(result.PVWallWestSupply.get(0).get(1).contains("61.0"));

                String expectedTime = result.times.get(0);
                String expectedTime2 = result.times.get(1);
                assertTrue(expectedTime.contains("2005-01-01T00:00:00+00:00"));
                assertTrue(expectedTime2.contains("2005-01-01T01:00:00+00:00"));
            }
        }
    }

    @Test
    public void testReturnOutputs() {
        CEAOutputData data = null;
        Method returnOutputs = null;
        RunCEATask task = null;
        URI testURI = null;

        try {
            testURI = new URI("http://localhost/test");
            ArrayList<CEAInputData> testData = new ArrayList<CEAInputData>();
            testData.add(new CEAInputData("test", "test", (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00), null, null, null, null));
            ArrayList<String> testArray = new ArrayList<>();
            testArray.add("testUri");
            Integer test_thread = 0;
            String test_CRS = "27700";
            task = new RunCEATask(testData, testURI, testArray, test_thread, test_CRS, null);

            data = new CEAOutputData();

            returnOutputs = task.getClass().getDeclaredMethod("returnOutputs", CEAOutputData.class);
        } catch (URISyntaxException| NoSuchMethodException e){
            fail();
        }

        try{
            returnOutputs.invoke(task, data);
        } catch (Exception e) {
            assert e instanceof InvocationTargetException;
            assertEquals(((InvocationTargetException) e).getTargetException().getClass(), JPSRuntimeException.class);
        }

        try{
            HttpResponse<?> response = mock(HttpResponse.class);
            try (MockedStatic<Unirest> unirest = mockStatic(Unirest.class, RETURNS_MOCKS)) {
                unirest.when(() -> Unirest.post(anyString())
                                .header(anyString(), anyString())
                                .body(anyString())
                                .socketTimeout(anyInt())
                                .asEmpty())
                        .thenReturn(response);

                returnOutputs.invoke(task, data);
            } catch (Exception e) {
                assert e instanceof InvocationTargetException;
                assertEquals(((InvocationTargetException) e).getTargetException().getClass(), JPSRuntimeException.class);
                assertEquals(((InvocationTargetException) e).getTargetException().getCause().getMessage(), testURI.toString()+" 0");
            }

        } catch (Exception e) {
            fail();
        }
    }

    @Test
    public void testRun() throws NoSuchMethodException, URISyntaxException, InvocationTargetException, IllegalAccessException {
        URI testURI = new URI("http://localhost/test");
        ArrayList<CEAInputData> testData = new ArrayList<CEAInputData>();
        testData.add(new CEAInputData("test", "test", (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00), null, null, null, null));
        ArrayList<String> testArray = new ArrayList<>();
        testArray.add("testUri");
        Integer test_thread = 0;
        String test_CRS = "27700";
        RunCEATask task = Mockito.spy(new RunCEATask(testData, testURI, testArray, test_thread, test_CRS, null));

        Method run = task.getClass().getDeclaredMethod("run");

        Process dummy_process = mock(Process.class);
        CEAOutputData dummy_data = mock(CEAOutputData.class);

        doReturn(dummy_process).when(task).runProcess(any());
        doNothing().when(task).renamePVT(anyString(), anyString());
        doReturn(dummy_data).when(task).extractTimeSeriesOutputs(anyString());
        doReturn(dummy_data).when(task).extractArea(anyString(), any());
        doNothing().when(task).returnOutputs(any());

        run.invoke(task);

        verify(task, times(6)).runProcess(any());
        verify(task, times(1)).extractTimeSeriesOutputs(anyString());
        verify(task, times(1)).extractArea(anyString(), any());
        verify(task, times(1)).returnOutputs(any());
    }

    @Test
    public void testDataToFile(@TempDir Path tempDir) throws URISyntaxException, NoSuchMethodException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, IOException {
        URI testURI = new URI("http://localhost/test");
        ArrayList<CEAInputData> testData = new ArrayList<>();
        List<OffsetDateTime> testTimes = Collections.nCopies(8760, OffsetDateTime.now());
        Map<String, List<Double>> testWeather = new HashMap<>();
        testWeather.put("testWeather", Collections.nCopies(8760, 0.00));
        testData.add(new CEAInputData("test", "test", (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00), null, testTimes, testWeather, Arrays.asList(0.00, 0.00, 0.00, 0.00)));
        ArrayList<String> testArray = new ArrayList<>();
        testArray.add("testUri");
        Integer test_thread = 0;
        String test_CRS = "27700";
        RunCEATask task = new RunCEATask(testData, testURI, testArray, test_thread, test_CRS, null);

        ArrayList<CEAInputData> surroundings = new ArrayList<>();
        surroundings.add(new CEAInputData("test", "test", (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00), null, null, null, null));
        testData.get(0).setSurrounding(surroundings);

        Method dataToFile = task.getClass().getDeclaredMethod("dataToFile", ArrayList.class, String.class, String.class, String.class, String.class, String.class);
        assertNotNull(dataToFile);
        dataToFile.setAccessible(true);

        Field noSurroundings = task.getClass().getDeclaredField("noSurroundings");
        noSurroundings.setAccessible(true);
        Field noWeather = task.getClass().getDeclaredField("noWeather");
        noWeather.setAccessible(true);

        Path testPath = Files.createFile(tempDir.resolve("test.txt"));
        Path testPath2 = Files.createFile(tempDir.resolve("test2.txt"));
        Path testPath3 = Files.createFile(tempDir.resolve("test3.txt"));
        Path testPath4 = Files.createFile(tempDir.resolve("test4.txt"));

        dataToFile.invoke(task, testData, tempDir.toString(), testPath.toString(), testPath2.toString(), testPath3.toString(), testPath4.toString());

        assertFalse((Boolean) noSurroundings.get(task));
        assertFalse((Boolean) noWeather.get(task));
    }

    @Test
    public void testExtractSolarSupply() throws URISyntaxException, NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        URI testURI = new URI("http://localhost/test");
        ArrayList<CEAInputData> testData = new ArrayList<CEAInputData>();
        testData.add(new CEAInputData("test", "test", (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00), null, null, null, null));
        ArrayList<String> testArray = new ArrayList<>();
        testArray.add("testUri");
        Integer test_thread = 0;
        String test_CRS = "27700";
        RunCEATask task = Mockito.spy(new RunCEATask(testData, testURI, testArray, test_thread, test_CRS, null));

        String PVTitleRow = "Date,PV_roofs_top_E_kWh,PV_walls_south_E_kWh,Other,PV_walls_north_E_kWh,PV_walls_west_E_kWh,PV_walls_east_E_kWh";
        String PVValuesRow1 = "2005-01-01 00:00:00+00:00,20.0,30.0,40.0,50.0,60.0,70.0";
        String PVValuesRow2 = "2005-01-01 01:00:00+00:00,21.0,31.0,41.0,51.0,61.0,71.0";

        String generatorType = "PV";
        List<String> supplyTypes = Arrays.asList("E");
        String dataSeparator = ",";
        String solarFile = "test";
        String tmpDir = "test";

        try (MockedConstruction<FileReader> fReader = mockConstruction(FileReader.class)) {
            try (MockedConstruction<BufferedReader> bReader = mockConstruction(BufferedReader.class, (mock, context) -> {
                when(mock.readLine()).thenReturn(PVTitleRow, PVValuesRow1, PVValuesRow2, null);
            })) {
                Method extractSolarSupply = task.getClass().getDeclaredMethod("extractSolarSupply", CEAOutputData.class, String.class, List.class, String.class, String.class, String.class, Boolean.class);
                CEAOutputData result = (CEAOutputData) extractSolarSupply.invoke(task, new CEAOutputData(), generatorType, supplyTypes, dataSeparator, solarFile, tmpDir, false);

                assertTrue(result.PVRoofSupply.get(0).get(0).contains("20.0"));
                assertTrue(result.PVRoofSupply.get(0).get(1).contains("21.0"));
                assertTrue(result.PVWallSouthSupply.get(0).get(0).contains("30.0"));
                assertTrue(result.PVWallSouthSupply.get(0).get(1).contains("31.0"));
                assertTrue(result.PVWallNorthSupply.get(0).get(0).contains("50.0"));
                assertTrue(result.PVWallNorthSupply.get(0).get(1).contains("51.0"));
                assertTrue(result.PVWallEastSupply.get(0).get(0).contains("70.0"));
                assertTrue(result.PVWallEastSupply.get(0).get(1).contains("71.0"));
                assertTrue(result.PVWallWestSupply.get(0).get(0).contains("60.0"));
                assertTrue(result.PVWallWestSupply.get(0).get(1).contains("61.0"));
                verify(task, times(5)).addSolarSupply(any(), anyString(), anyString(), anyString(), any());
            }
        }
    }
}
