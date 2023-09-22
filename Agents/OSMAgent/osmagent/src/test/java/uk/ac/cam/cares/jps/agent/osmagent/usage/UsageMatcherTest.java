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
import java.sql.*;
import java.util.List;

public class UsageMatcherTest {
    @Test
    public void testUpdateOntoBuilt() {
        String content = "testKey,testValue,testComment,testOntoBuiltEnv\ntest_key,test_value,test_comment,test_ontobuiltenv";

        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        InputStream mockInputStream2 = new ByteArrayInputStream(content.getBytes());


        try (MockedStatic<FileReader> fileReaderMock = mockStatic(FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream).thenReturn(mockInputStream2);
            try (MockedConstruction<RemoteRDBStoreClient> remoteRDBStoreClientMock = mockConstruction(RemoteRDBStoreClient.class)) {
                UsageMatcher usageMatcher = new UsageMatcher("", "", "");
                usageMatcher.updateOntoBuilt("points", "polygons");

                ArgumentCaptor<String> argumentCaptor = ArgumentCaptor.forClass(String.class);

                verify(remoteRDBStoreClientMock.constructed().get(0), times(2)).executeUpdate(argumentCaptor.capture());

                List<String> allCaptures = argumentCaptor.getAllValues();

                assertEquals(2, allCaptures.size());
                assertTrue(allCaptures.get(0).contains("test_key"));
                assertTrue(allCaptures.get(0).contains("test_value"));
                assertTrue(allCaptures.get(0).contains("test_ontobuiltenv"));
                assertTrue(allCaptures.get(1).contains("test_key"));
                assertTrue(allCaptures.get(1).contains("test_value"));
                assertTrue(allCaptures.get(1).contains("test_ontobuiltenv"));

                boolean point0 = allCaptures.get(0).contains("points");
                boolean polygon0 = allCaptures.get(0).contains("polygons");
                boolean point1 = allCaptures.get(1).contains("points");
                boolean polygon1 = allCaptures.get(1).contains("points");

                // verify each table is updated exactly once
                assertTrue((point0 || point1) && (polygon0 || polygon1) && !(point0 && point1));
            }
        }
    }

    @Test
    public void testCheckAndAddColumns() throws SQLException {
        Connection connectionMock = mock(Connection.class);
        DatabaseMetaData databaseMetaDataMock = mock(DatabaseMetaData.class);
        ResultSet resultSetMock = mock(ResultSet.class);
        Statement statementMock = mock(Statement.class);

        doReturn(statementMock).when(connectionMock).createStatement();
        doReturn(databaseMetaDataMock).when(connectionMock).getMetaData();

        doReturn(resultSetMock).when(databaseMetaDataMock).getColumns(isNull(), anyString(), anyString(), anyString());

        when(resultSetMock.next()).thenReturn(true).thenReturn(false).thenReturn(true).thenReturn(false).thenReturn(true).thenReturn(false).thenReturn(true).thenReturn(false);

        try (MockedConstruction<RemoteRDBStoreClient> remoteRDBStoreClientMock = mockConstruction(RemoteRDBStoreClient.class,
                (mock, context) -> {
                    doReturn(connectionMock).when(mock).getConnection();
                })) {
            UsageMatcher usageMatcher = new UsageMatcher("", "", "");

            usageMatcher.checkAndAddColumns("public.points", "public.polygons");

            verify(connectionMock, times(4)).getMetaData();
            verify(connectionMock, times(2)).createStatement();
            verify(databaseMetaDataMock, times(4)).getColumns(isNull(), anyString(), anyString(), anyString());
            verify(resultSetMock, times(4)).next();
            verify(statementMock, times(2)).execute(anyString());
            verify(remoteRDBStoreClientMock.constructed().get(0), times(1)).getConnection();
        }

    }
}
