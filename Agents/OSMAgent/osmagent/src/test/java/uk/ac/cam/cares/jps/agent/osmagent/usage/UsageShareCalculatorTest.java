package uk.ac.cam.cares.jps.agent.osmagent.usage;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.agent.osmagent.FileReader;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.List;

public class UsageShareCalculatorTest {
    @Test
    public void testUpdateUsageShare() {
        try (MockedConstruction<RemoteRDBStoreClient> remoteRDBStoreClientMock = mockConstruction(RemoteRDBStoreClient.class)) {
            UsageShareCalculator usageShareCalculator = new UsageShareCalculator("", "", "");

            usageShareCalculator.updateUsageShare( "usage.usage");

            ArgumentCaptor<String> argumentCaptor = ArgumentCaptor.forClass(String.class);

            verify(remoteRDBStoreClientMock.constructed().get(0), times(3)).executeUpdate(argumentCaptor.capture());

            List<String> allCaptures = argumentCaptor.getAllValues();

            assertEquals(3, allCaptures.size());
            assertTrue(allCaptures.get(1).contains("usage.usage"));
            assertTrue(allCaptures.get(2).contains("usage.usage"));
        }
    }

    @Test
    public void testUpdateLandUse() {
        String content = "testKey,testValue,testComment,testOntoBuiltEnv\ntest_key,test_value,test_comment,test_ontobuiltenv";

        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());

        try (MockedStatic<FileReader> fileReaderMock = mockStatic(FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedConstruction<RemoteRDBStoreClient> rdbStoreClientMock = mockConstruction(RemoteRDBStoreClient.class)) {
                UsageShareCalculator usageShareCalculator = new UsageShareCalculator("", "", "");

                usageShareCalculator.updateLandUse("usage.usage", "public.landuse", "");

                ArgumentCaptor<String> argumentCaptor = ArgumentCaptor.forClass(String.class);

                verify(rdbStoreClientMock.constructed().get(0), times(1)).executeUpdate(argumentCaptor.capture());
                List<String> allCaptures = argumentCaptor.getAllValues();

                assertEquals(1, allCaptures.size());
                assertTrue(allCaptures.get(0).contains("test_key"));
                assertTrue(allCaptures.get(0).contains("test_value"));
            }
        }
    }
}
