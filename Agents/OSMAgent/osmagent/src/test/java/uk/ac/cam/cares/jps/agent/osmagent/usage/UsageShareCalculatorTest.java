package uk.ac.cam.cares.jps.agent.osmagent.usage;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

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
}
