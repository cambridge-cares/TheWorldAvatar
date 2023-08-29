package uk.ac.cam.cares.jps.agent.osmagent.usage;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import com.opencsv.CSVReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;

public class UsageShareCalculatorTest {
    @Test
    public void testUpdateUsageShare() {
        try (MockedConstruction<RemoteRDBStoreClient> remoteRDBStoreClientMock = mockConstruction(RemoteRDBStoreClient.class)) {
            UsageShareCalculator.updateUsageShare("", "", "points", "polygons", "");

            ArgumentCaptor<String> argumentCaptor = ArgumentCaptor.forClass(String.class);

            verify(remoteRDBStoreClientMock.constructed().get(0), times(3)).executeUpdate(argumentCaptor.capture());

            List<String> allCaptures = argumentCaptor.getAllValues();

            assertEquals(3, allCaptures.size());
            assertTrue(allCaptures.get(1).contains("points"));
            assertTrue(allCaptures.get(2).contains("polygons"));
        }
    }

    @Test
    public void testUpdateLandUse() throws IOException {
        String[] testLine = {"testKey", "testValue", "testComment", "testOntoBuiltEnv"};

        try (MockedConstruction<InputStreamReader> inputStreamReaderMock = mockConstruction(InputStreamReader.class)) {
            try (MockedConstruction<CSVReader> csvReaderMock = mockConstruction(CSVReader.class,
                    (mock, context) -> {
                        when(mock.readNext()).thenReturn(testLine).thenReturn(null);
                    })) {
                try (MockedConstruction<RemoteRDBStoreClient> rdbStoreClientMock = mockConstruction(RemoteRDBStoreClient.class)) {
                    UsageShareCalculator.updateLandUse("", "","", "","","");

                    ArgumentCaptor<String> argumentCaptor = ArgumentCaptor.forClass(String.class);

                    verify(rdbStoreClientMock.constructed().get(0), times(2)).executeUpdate(argumentCaptor.capture());
                    List<String> allCaptures = argumentCaptor.getAllValues();

                    assertEquals(2, allCaptures.size());
                    assertTrue(allCaptures.get(0).contains("testKey"));
                    assertTrue(allCaptures.get(0).contains("testValue"));
                    assertTrue(allCaptures.get(1).contains("testKey"));
                    assertTrue(allCaptures.get(1).contains("testValue"));
                }
            }
        }
    }
}
