package uk.ac.cam.cares.jps.agent.isochroneagent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyDouble;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import java.nio.file.Path;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.ontop.OntopClient;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;
import java.util.Properties;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;


public class IsochroneAgentTest {

    @Mock
    private RemoteStoreClient remoteStoreClient;

    @Mock
    private RemoteRDBStoreClient RemoteRDBStoreClient;

    private IsochroneAgent agent; 

    @Test
    public void testProcessRequestParameters_EmptyIRIInput() {

        //Mock response 
        MockedConstruction<EndpointConfig> endpointConfigMock = mockConstruction(EndpointConfig.class,
                (mock, context) -> {
                    doReturn("test").when(mock).getDbUrl(anyString());
                    doReturn("test").when(mock).getDbUser();
                    doReturn("test").when(mock).getDbPassword();
                });
                
        agent = new IsochroneAgent();
        HttpServletRequest setRequest = mock(HttpServletRequest.class);
        when(setRequest.getRequestURI()).thenReturn("localhost:10105/isochroneagent/update?");

        JSONObject input = new JSONObject();
        input.put("function", "");
        input.put("timethreshold", "");
        input.put("timeinterval", "");

        JSONObject result = agent.processRequestParameters(input, setRequest);
        try {
            assertFalse(agent.validateInput(result));
        } catch (BadRequestException e) {
            fail("Unexpected BadRequestException");
        }
    }

    @Test
    public void testReadConfig(){


        String PROPETIES_PATH ="/resources/configTest.properties";

        try(InputStream input = FileReader.getStream(PROPETIES_PATH)){
        Properties prop = new Properties();
        prop.load(input);
        assertEquals("test",prop.getProperty("db.name"));
        assertEquals(0.0002,prop.getProperty("segmentization_length"));
        assertEquals("", prop.getProperty("kgEndpoint"));

        String populationTables = prop.getProperty("populationTables");
        // Split the string using the comma as the delimiter
        String[] tableNames = populationTables.split("\\s*,\\s*");
        ArrayList<String> populationTableList = new ArrayList<String>(Arrays.asList(tableNames));
        }
        catch (Exception e) {
        }
    
    }


    @Test
    public void testProcessRequestParameters_ValidInput(){
        //Mock response 
        MockedConstruction<EndpointConfig> endpointConfigMock = mockConstruction(EndpointConfig.class,
                (mock, context) -> {
                    doReturn("test").when(mock).getDbUrl(anyString());
                    doReturn("test").when(mock).getDbUser();
                    doReturn("test").when(mock).getDbPassword();
                });

        agent = new IsochroneAgent();
        String content = "db.name=test\nsegmentization_length=0.0002 \nkgEndpoint=\npopulationTables=population, population_test, population_women";

        //Create Mockclasses
        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        MockedStatic<FileReader> fileReaderMock = mockStatic(FileReader.class);
        fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
        MockedConstruction<RouteSegmentization> routeSegmentizationMock = mockConstruction(RouteSegmentization.class,
                (mock, context) -> {
                    //Table does not exists
                    doReturn(false).when(mock).doesTableExist(any(RemoteRDBStoreClient.class));
                });
        MockedConstruction<IsochroneGenerator> isochroneGeneratorMock = mockConstruction(IsochroneGenerator.class);
        MockedConstruction<PopulationMapper> populationMapperMock = mockConstruction(PopulationMapper.class);
        MockedConstruction<GeoServerClient> geoserverClientMock = mockConstruction(GeoServerClient.class);
        MockedConstruction<OntopClient> ontopClientMock = mockConstruction(OntopClient.class);



            try (fileReaderMock;
            endpointConfigMock;
            routeSegmentizationMock;
            isochroneGeneratorMock;
            populationMapperMock;
            geoserverClientMock;ontopClientMock;) {

        JSONObject input = new JSONObject();
        input.put("function", "15MSC");
        input.put("timethreshold", "15");
        input.put("timeinterval", "2");
        agent.processRequestParameters(input);
        verify(endpointConfigMock.constructed().get(0), times(1)).getDbUrl(anyString());
        verify(endpointConfigMock.constructed().get(0), times(1)).getDbUser();
        verify(endpointConfigMock.constructed().get(0), times(1)).getDbPassword();
        verify(routeSegmentizationMock.constructed().get(0), times(1)).segmentize(any(RemoteRDBStoreClient.class), anyDouble());
        verify(isochroneGeneratorMock.constructed().get(0), times(1)).generateIsochrone(any(RemoteRDBStoreClient.class), anyInt(), anyInt(),  any(Map.class));
        verify(isochroneGeneratorMock.constructed().get(0), times(1)).createIsochroneBuilding(any(RemoteRDBStoreClient.class));
        verify(populationMapperMock.constructed().get(0), times(1)).checkAndAddColumns(any(RemoteRDBStoreClient.class), any(ArrayList.class));
        verify(populationMapperMock.constructed().get(0), times(1)).mapPopulation(any(RemoteRDBStoreClient.class), any(ArrayList.class));
        verify(geoserverClientMock.constructed().get(0), times(1)).createWorkspace(anyString());
        verify(geoserverClientMock.constructed().get(0), times(1)).createPostGISDataStore(anyString(), anyString(), anyString(), anyString());
        verify(geoserverClientMock.constructed().get(0), times(1)).createPostGISLayer(anyString(), anyString(), anyString(), any(GeoServerVectorSettings.class));
        verify(ontopClientMock.constructed().get(0), times(1)).updateOBDA(any(Path.class));  
    }

}

}
