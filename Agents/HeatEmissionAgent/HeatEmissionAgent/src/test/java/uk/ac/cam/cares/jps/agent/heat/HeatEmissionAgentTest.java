package uk.ac.cam.cares.jps.agent.heat;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.ws.rs.BadRequestException;

public class HeatEmissionAgentTest {

    // processRequestParameters(JSONObject requestParams)
    @Test
    public void testProcessRequestParameters() throws NoSuchMethodException, Exception {
        HeatEmissionAgent Query = new HeatEmissionAgent();
        Method processRequestParameters = Query.getClass().getDeclaredMethod("processRequestParameters",
                JSONObject.class);

        // Test empty request parameters
        JSONObject requestParams_1 = new JSONObject();
        try {
            processRequestParameters.invoke(Query, requestParams_1);
        } catch (Exception e) {
            assert e instanceof InvocationTargetException;
            assertEquals(((InvocationTargetException) e).getTargetException().getClass(),
                    BadRequestException.class);
        }

        // Test data
        String inputBounds = "{\"job\":{\"lower_bounds\":\"8464.256074442204#23588.08319044689#0\",\"upper_bounds\":\"17619.669922658715#30520.376177137474#105\"}}\r\n";
        JSONObject requestParams_2 = new JSONObject(inputBounds);
        JSONObject expected = new JSONObject();
        JSONObject row = new JSONObject();
        row.put("Coordinate", "13435.86861#26485.17517#60");
        row.put("Heat Emission", "100");
        JSONArray heatresult = new JSONArray()
                .put(new JSONObject().put("Coordinate", "13435.86861#26485.17517#60").put("Heat Emission", "100"));
        expected.put("result", heatresult);
        JSONObject actual = (JSONObject) processRequestParameters.invoke(Query, requestParams_2);
        assertEquals(expected, actual);
    }

    // Test validateInput(JSONObject requestParams)
    @Test
    public void testValidateInput() {
        HeatEmissionAgent Query = new HeatEmissionAgent();
        Method validateInput = null;
        try {
            validateInput = Query.getClass().getDeclaredMethod("validateInput", JSONObject.class);
        } catch (Exception e) {
            fail();
        }

        // Check failure with empty request parameters
        JSONObject requestInput1 = new JSONObject();
        try {
            validateInput.invoke(Query, requestInput1);
        } catch (Exception e) {
            assert e instanceof InvocationTargetException;
            assertEquals(((InvocationTargetException) e).getTargetException().getClass(), BadRequestException.class);
        }

        // Check failure with empty upper limits
        String inputBounds_2 = "{\"job\":{\"lower_bounds\":\"10#20#0\",\"upper_bounds\":\"\"}}\r\n";
        JSONObject requestInput2 = new JSONObject(inputBounds_2);
        try {
            validateInput.invoke(Query, requestInput2);
        } catch (Exception e) {
            assert e instanceof InvocationTargetException;
            assertEquals(((InvocationTargetException) e).getTargetException().getClass(), BadRequestException.class);
        }

        // Check failure with empty lower limits
        String inputBounds_3 = "{\"job\":{\"lower_bounds\":\"\",\"upper_bounds\":\"20#30#40\"}}\r\n";
        JSONObject requestInput3 = new JSONObject(inputBounds_3);
        try {
            validateInput.invoke(Query, requestInput3);
        } catch (Exception e) {
            assert e instanceof InvocationTargetException;
            assertEquals(((InvocationTargetException) e).getTargetException().getClass(), BadRequestException.class);
        }

        // Should pass now
        String inputBounds_4 = "{\"job\":{\"lower_bounds\":\"10#20#0\",\"upper_bounds\":\"20#30#40\"}}\r\n";
        JSONObject requestInput4 = new JSONObject(inputBounds_4);
        try {
            validateInput.invoke(Query, requestInput4);
        } catch (Exception e) {
            fail();
        }
    }

    @Test
    public void testAgent() {
        JSONObject requestInput = new JSONObject();
        requestInput.put("endpoint", "http://localhost:48889/blazegraph/namespace/jibusinessunits/sparql");
        // requestInput.put("endpoint",
        // "http://localhost:3838/blazegraph/namespace/jibusinessunits/sparql");
        requestInput.put("ontology", "ontochemplant");
        JSONObject result = new HeatEmissionAgent().processRequestParameters(requestInput);
        assertTrue(result.getString("success").length() > 0);

    }

    @Test
    public void testCheckBasePolygon() {
        JSONObject requestInput = new JSONObject();
        requestInput.put("endpoint", "http://localhost:48889/blazegraph/namespace/jibusinessunits/sparql");
        String polyString = "17614.702476747625#28402.62858989779#0.0#17614.693799048087#28402.491679600076#0.0#17614.667914805017#28402.356929220015#0.0#17614.625232234797#28402.226463851686#0.0#17614.566424460703#28402.102341011836#0.0#17614.492418922044#28401.986518191694#0.0#17614.404382724613#28401.88082198668#0.0#17614.30370425659#28401.786919288825#0.0#17614.191971275228#28401.70629099986#0.0#17614.070945876294#28401.640208676046#0.0#17613.942536704457#28401.58971447521#0.0#17613.8087688463#28401.55560472111#0.0#17613.671751902977#28401.538417344866#0.0#17613.533646713833#28401.538423401795#0.0#17613.396631278938#28401.55562279614#0.0#17613.262866415644#28401.58974428344#0.0#17613.134461671565#28401.640249747335#0.0#17613.013442068805#28401.706342686488#0.0#17612.901716162036#28401.786980775687#0.0#17612.801045928747#28401.880892304096#0.0#17612.713019004004#28401.986596230832#0.0#17612.639023624994#28402.102425541852#0.0#17612.580226739814#28402.22655353953#0.0#17612.537555611692#28402.357022651315#0.0#17612.511683187684#28402.491775301325#0.0#17612.503017496467#28402.628686359843#0.0#17612.51169519705#28402.765596657584#0.0#17612.537579442527#28402.900347037274#0.0#17612.58026201504#28403.030812405075#0.0#17612.63906978701#28403.15493524426#0.0#17612.71307532757#28403.27075806341#0.0#17612.801111523826#28403.376454267425#0.0#17612.901789993244#28403.470356964255#0.0#17613.013522972906#28403.55098525222#0.0#17613.134548372727#28403.61706757499#0.0#17613.26295754384#28403.667561775103#0.0#17613.39672539969#28403.70167152858#0.0#17613.533742340587#28403.718858904514#0.0#17613.671847531514#28403.71885284758#0.0#17613.80886296538#28403.701653453307#0.0#17613.94262782773#28403.6675319665#0.0#17614.071032572425#28403.617026503216#0.0#17614.19205217034#28403.550933565017#0.0#17614.30377807816#28403.470295476694#0.0#17614.404448311332#28403.376383949217#0.0#17614.492475236228#28403.270680023626#0.0#17614.566470618476#28403.15485071353#0.0#17614.625267502874#28403.03072271673#0.0#17614.667938631817#28402.90025360565#0.0#17614.69381105537#28402.765500956066#0.0#17614.702476747625#28402.62858989779#0.0";
        boolean result = new JurongIsland(requestInput).checkBasePolygon(polyString);
        assertTrue(result);
    }

    @Test
    public void testAgentMainland() {
        JSONObject requestInput = new JSONObject();
        requestInput.put("endpoint", "http://localhost:48889/blazegraph/namespace/sgbusinessunits/sparql");
        requestInput.put("ontology", "ontocompany");
        JSONObject result = new HeatEmissionAgent().processRequestParameters(requestInput);
        assertTrue(result.getString("success").equals("true"));

    }

}
