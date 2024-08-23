package uk.ac.cam.cares.jps.agent.cea.tasks;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.agent.cea.data.CEAMetaData;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.agent.cea.data.CEABuildingData;
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
    public void testRunProcess() throws Exception {
        URI testURI = new URI("http://localhost/test");
        ArrayList<CEABuildingData> testData = new ArrayList<CEABuildingData>();
        testData.add(new CEABuildingData(new CEAGeometryData(), (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00)));
        CEAMetaData testMetaData = new CEAMetaData(null, null, null, null, null);
        ArrayList<String> testArray = new ArrayList<>();
        testArray.add("testUri");
        Integer test_thread = 0;
        String test_CRS = "27700";

        Process dummyProcess  = mock(Process.class);

        RunCEATask task = new RunCEATask(testData, testMetaData, testURI, testArray, test_thread, test_CRS);
        try (MockedConstruction<ProcessBuilder> builder = mockConstruction(ProcessBuilder.class,  (mock, context) -> {
            when(mock.start()).thenReturn(dummyProcess);
        })) {
            try (MockedConstruction<BufferedReader> reader = mockConstruction(BufferedReader.class, (mock, context) -> {
                when(mock.read()).thenReturn(-1);
            })) {
                try (MockedConstruction<InputStreamReader> streamReader = mockConstruction(InputStreamReader.class)) {
                    ArrayList<String> args = new ArrayList<>();
                    args.add("test");
                    Process result = (Process) task.runProcess(args);

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
            ArrayList<CEABuildingData> testData = new ArrayList<CEABuildingData>();
            testData.add(new CEABuildingData(new CEAGeometryData(), (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00)));
            CEAMetaData testMetaData = new CEAMetaData(null, null, null, null, null);
            ArrayList<String> testArray = new ArrayList<>();
            testArray.add("testUri");
            Integer test_thread = 0;
            String test_CRS = "27700";
            RunCEATask task = new RunCEATask(testData, testMetaData, testURI, testArray, test_thread, test_CRS);

            File myTempDir = new File(System.getProperty("java.io.tmpdir"));
            File newDirectory = new File(myTempDir, "new_directory");
            assertTrue(newDirectory.mkdir() || newDirectory.isDirectory());

            File tempFile = File.createTempFile("text", ".temp", newDirectory);

            task.deleteDirectoryContents(newDirectory);

            assertFalse(tempFile.isFile());
        }
        catch (IOException | URISyntaxException e) {
            System.out.println(e.getMessage());
            fail();
        }
    }

    @Test
    public void testReturnOutputs() {
        CEAOutputData data = null;
        RunCEATask task = null;
        URI testURI = null;

        try {
            testURI = new URI("http://localhost/test");
            ArrayList<CEABuildingData> testData = new ArrayList<CEABuildingData>();
            testData.add(new CEABuildingData(new CEAGeometryData(), (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00)));
            CEAMetaData testMetaData = new CEAMetaData(null, null, null, null, null);
            ArrayList<String> testArray = new ArrayList<>();
            testArray.add("testUri");
            Integer test_thread = 0;
            String test_CRS = "27700";
            task = new RunCEATask(testData, testMetaData, testURI, testArray, test_thread, test_CRS);

            data = new CEAOutputData();

        }
        catch (URISyntaxException e) {
            fail();
        }

        HttpResponse<?> response = mock(HttpResponse.class);
        try (MockedStatic<Unirest> unirest = mockStatic(Unirest.class, RETURNS_MOCKS)) {
            unirest.when(() -> Unirest.post(anyString())
                            .header(anyString(), anyString())
                            .body(anyString())
                            .socketTimeout(anyInt())
                            .asEmpty())
                    .thenReturn(response);

            task.returnOutputs(data);
        }
        catch (Exception e) {
            assertEquals(e.getCause().getMessage(), testURI.toString()+" 0");
        }
    }

    @Test
    public void testRun() throws NoSuchMethodException, URISyntaxException, InvocationTargetException, IllegalAccessException {
        URI testURI = new URI("http://localhost/test");
        ArrayList<CEABuildingData> testData = new ArrayList<CEABuildingData>();
        testData.add(new CEABuildingData(new CEAGeometryData(new ArrayList<>(), "", ""), (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00)));
        CEAMetaData testMetaData = new CEAMetaData(null, null, null, null, null);
        ArrayList<String> testArray = new ArrayList<>();
        testArray.add("testUri");
        Integer test_thread = 0;
        String test_CRS = "27700";
        RunCEATask task = Mockito.spy(new RunCEATask(testData, testMetaData, testURI, testArray, test_thread, test_CRS));


        Process dummy_process = mock(Process.class);

        doReturn(dummy_process).when(task).runProcess(any());
        doNothing().when(task).renamePVT(anyString(), anyString());
        doNothing().when(task).returnOutputs(any());

        try (MockedStatic<CEAOutputHandler> ceaOutputHandlerMock = mockStatic(CEAOutputHandler.class)) {
            ceaOutputHandlerMock.when(() ->
                    CEAOutputHandler.extractCEAOutputs(anyString(), anyList()))
                    .thenReturn(new CEAOutputData());

            task.run();
        }

        verify(task, times(6)).runProcess(any());
        verify(task, times(1)).returnOutputs(any());
    }
}
