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
        String inputBounds = "{\"job\":{\"lower_bounds\":\"8464#23588#0\",\"upper_bounds\":\"17619#30520#105\"}}\r\n";
        JSONObject requestInput = new JSONObject(inputBounds);
        JSONObject result = new HeatEmissionAgent().processRequestParameters(requestInput);
        assertTrue(result.getJSONArray("result").length() > 0);

    }

    // Test Boundary(JSONObject inputBounds)
    @Test
    public void testBoundary() throws Exception {
        String inputBounds_new = "{\"job\":{\"lower_bounds\":\"8464.256074442204#23588.08319044689#0\",\"upper_bounds\":\"17619.669922658715#30520.376177137474#105\"}}\r\n";
        JSONObject requestInput = new JSONObject(inputBounds_new);
        HeatEmissionAgent Query = new HeatEmissionAgent();
        Method boundary = Query.getClass().getDeclaredMethod("Boundary", JSONObject.class);
        assertNotNull(boundary);
        boundary.setAccessible(true);
        double[] test_result = (double[]) boundary.invoke(Query, requestInput);
        assertEquals(test_result[0], 8464.256074442204, 1e-6d);
        assertEquals(test_result[1], 23588.08319044689, 1e-6d);
        assertEquals(test_result[2], 17619.669922658715, 1e-6d);
        assertEquals(test_result[3], 30520.376177137474, 1e-6d);
    }

    // Test IRIandCO2Query ()
    @Test
    public void testIRIandCO2Query() {
        String IRI = "http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql/cityfurniture/UUID_4de83001-0c75-4155-b835-e21f1a46ac77/";
        String CO2 = "80";
        JSONArray expected = new JSONArray().put(new JSONObject().put("CO2", CO2).put("IRI", IRI));

        try (MockedStatic<HeatEmissionAgent> heq = Mockito.mockStatic(HeatEmissionAgent.class)) {
            heq.when(() -> HeatEmissionAgent.IRIandCO2Query()).thenReturn(expected);
            JSONArray actual = (JSONArray) HeatEmissionAgent.IRIandCO2Query();
            assertEquals(expected, actual);
        }
    }

    // Test FuelCEIEfficiency (String ChemicalPlant)
    @Test
    public void testFuelCEIEfficiency() {
        String ChemicalPlant = "http://www.theworldavatar.com/kb/ontochemplant/Chemical_plant_of_YTLPowerSerayaPte.Limited";
        String CEI = "60";
        String Effi = "50";
        JSONArray expected = new JSONArray().put(new JSONObject().put("Effi", Effi).put("CEI", CEI));

        try (MockedStatic<HeatEmissionAgent> heq = Mockito.mockStatic(HeatEmissionAgent.class)) {
            heq.when(() -> HeatEmissionAgent.FuelCEIEfficiency(ChemicalPlant)).thenReturn(expected);
            JSONArray actual = (JSONArray) HeatEmissionAgent.FuelCEIEfficiency(ChemicalPlant);
            assertEquals(expected, actual);
        }
    }

    // Test CoordinateQuery (String CityFurnitureIRI)
    @Test
    public void testCoordinateQuery() {
        String CityFurnitureIRI = "http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql/cityfurniture/UUID_4de83001-0c75-4155-b835-e21f1a46ac77/";
        String geometricIRI_1 = "http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql/surfacegeometry/UUID_062b3ce0-dca1-4723-9d58-eb53c095cf6f/";
        String polygonData_1 = "13435.86861#26485.17517#0.0#13435.15841#26483.78133#0.0#13435.15841#26483.78133#37.0#13435.86861#26485.17517#37.0#13435.86861#26485.17517#0.0";
        String geometricIRI_2 = "http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql/surfacegeometry/UUID_930a6813-64a0-4968-b7be-9da134f5811e/";
        String polygonData_2 = "13443.95878#26479.29732#0.0#13444.66898#26480.69116#0.0#13444.66898#26480.69116#37.0#13443.95878#26479.29732#37.0#13443.95878#26479.29732#0.0";
        JSONArray expected = new JSONArray()
                .put(new JSONObject().put(geometricIRI_1, polygonData_1).put(geometricIRI_2, polygonData_2));

        try (MockedStatic<HeatEmissionAgent> heq = Mockito.mockStatic(HeatEmissionAgent.class)) {
            heq.when(() -> HeatEmissionAgent.CoordinateQuery(CityFurnitureIRI)).thenReturn(expected);
            JSONArray actual = (JSONArray) HeatEmissionAgent.CoordinateQuery(CityFurnitureIRI);
            assertEquals(expected, actual);
        }
    }

    // Test HeatEmissionCoordinate (JSONArray coordiSpatialQueryResult)
    @Test
    public void testHeatEmissionCoordinate() throws Exception {
        JSONArray testInput = new JSONArray(
                "[{\"geometricIRI\":\"Sample1\",\"polygonData\":\"1#1#0#2#1#0#1#2#0#2#2#0\"},{\"geometricIRI\":\"Sample2\",\"polygonData\":\"1#1#5#2#1#5#1#2#0#2#2#5\"},{\"geometricIRI\":\"Sample3\",\"polygonData\":\"1.5#1.5#4#2#2#3#3#3#1#5#5#4.5\"}]");
        HeatEmissionAgent Query = new HeatEmissionAgent();
        Method heatEmissionCoordinate = Query.getClass().getDeclaredMethod("HeatEmissionCoordinate", JSONArray.class);
        assertNotNull(heatEmissionCoordinate);
        heatEmissionCoordinate.setAccessible(true);
        String test_result = (String) heatEmissionCoordinate.invoke(Query, testInput);
        assertEquals(test_result, "1.5#1.5#5");
    }

    // Test sparqlUpdate (String Plant_item, String Heat_value)
    @Test
    public void testSparqlUpdate() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        HeatEmissionAgent Query = spy(new HeatEmissionAgent());
        Method sparqlUpdate = Query.getClass().getDeclaredMethod("sparqlUpdate", String.class, String.class);
        assertNotNull(sparqlUpdate);

        String Plant_tiem = "http://www.theworldavatar.com/kb/ontochemplant/Plant_item";
        String Heat_value = "100";
        doNothing().when(Query).sparqlUpdate(anyString(), anyString());
        sparqlUpdate.invoke(Query, Plant_tiem, Heat_value);
        verify(Query, times(1)).sparqlUpdate(anyString(), anyString());
    }

}
