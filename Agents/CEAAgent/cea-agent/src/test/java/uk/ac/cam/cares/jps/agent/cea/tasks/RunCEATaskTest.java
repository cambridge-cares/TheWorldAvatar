package uk.ac.cam.cares.jps.agent.cea.tasks;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.agent.cea.data.CEAMetaData;
import uk.ac.cam.cares.jps.agent.cea.data.CEABuildingData;
import uk.ac.cam.cares.jps.agent.cea.data.CEAOutputData;

import java.io.*;
import java.util.*;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

public class RunCEATaskTest {
    @Test
    public void testRunProcess() throws Exception {
        ArrayList<CEABuildingData> testData = new ArrayList<CEABuildingData>();
        testData.add(new CEABuildingData(new CEAGeometryData(), (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00)));
        CEAMetaData testMetaData = new CEAMetaData(null, null, null, null, null);
        ArrayList<String> testArray = new ArrayList<>();
        testArray.add("testUri");
        Integer test_thread = 0;
        String test_CRS = "27700";

        Process dummyProcess  = mock(Process.class);

        RunCEATask task = new RunCEATask(testData, testMetaData, testArray, test_thread, test_CRS,
                "testDatabase", null, null);
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
            ArrayList<CEABuildingData> testData = new ArrayList<CEABuildingData>();
            testData.add(new CEABuildingData(new CEAGeometryData(), (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00)));
            CEAMetaData testMetaData = new CEAMetaData(null, null, null, null, null);
            ArrayList<String> testArray = new ArrayList<>();
            testArray.add("testUri");
            Integer test_thread = 0;
            String test_CRS = "27700";
            RunCEATask task = new RunCEATask(testData, testMetaData, testArray, test_thread, test_CRS,
                    "testDatabase", null, null);

            File myTempDir = new File(System.getProperty("java.io.tmpdir"));
            File newDirectory = new File(myTempDir, "new_directory");
            assertTrue(newDirectory.mkdir() || newDirectory.isDirectory());

            File tempFile = File.createTempFile("text", ".temp", newDirectory);

            task.deleteDirectoryContents(newDirectory);

            assertFalse(tempFile.isFile());
        }
        catch (IOException e) {
            System.out.println(e.getMessage());
            fail();
        }
    }

    @Test
    public void testRun() {
        ArrayList<CEABuildingData> testData = new ArrayList<CEABuildingData>();
        testData.add(new CEABuildingData(new CEAGeometryData(new ArrayList<>(), "", ""), (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00)));
        CEAMetaData testMetaData = new CEAMetaData(new ArrayList<>(), null, null, null, null);
        ArrayList<String> testArray = new ArrayList<>();
        testArray.add("testUri");
        Integer test_thread = 0;
        String test_CRS = "27700";
        CEAOutputUpdater updater = mock(CEAOutputUpdater.class);
        RunCEATask task = Mockito.spy(new RunCEATask(testData, testMetaData, testArray, test_thread, test_CRS,
                "testDatabase", updater, null));


        Process dummy_process = mock(Process.class);

        doReturn(dummy_process).when(task).runProcess(any());
        doNothing().when(task).renamePVT(anyString(), anyString());
        CEAOutputData outputData = new CEAOutputData();

        try (MockedStatic<CEAOutputHandler> ceaOutputHandlerMock = mockStatic(CEAOutputHandler.class)) {
            ceaOutputHandlerMock.when(() ->
                    CEAOutputHandler.extractCEAOutputs(anyString(), anyList()))
                    .thenReturn(outputData);

            task.run();
        }

        verify(task, times(6)).runProcess(any());
        verify(updater, times(1)).updateCEA(outputData);
    }
}
