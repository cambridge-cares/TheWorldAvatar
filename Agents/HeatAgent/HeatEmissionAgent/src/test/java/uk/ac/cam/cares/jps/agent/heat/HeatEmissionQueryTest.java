package uk.ac.cam.cares.jps.agent.heat;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.*;

public class HeatEmissionQueryTest {

	@Test
	public void testAgent() {
		String inputBounds = "{\"job\":{\"lower_bounds\":\"8464#23588#0\",\"upper_bounds\":\"17619#30520#105\"}}\r\n";
		JSONObject requestInput = new JSONObject(inputBounds);
		JSONObject result = HeatEmissionQuery.performCrossDomainQ(requestInput);
		assertTrue(result.getJSONArray("result").length() > 0);

	}

	// Test performCrossDomainQ(JSONObject jsonObject)
	@Test
	public void testPerformCrossDomainQ() throws Exception {
		String inputBounds = "{\"job\":{\"lower_bounds\":\"8464.256074442204#23588.08319044689#0\",\"upper_bounds\":\"17619.669922658715#30520.376177137474#105\"}}\r\n";
		JSONObject requestInput = new JSONObject(inputBounds);
		JSONObject expected = new JSONObject();
		JSONObject row = new JSONObject();
		row.put("Coordinate", "13435.86861#26485.17517#60");
		row.put("Heat Emission", "100");
		JSONArray heatresult = new JSONArray()
				.put(new JSONObject().put("Coordinate", "13435.86861#26485.17517#60").put("Heat Emission", "100"));
		expected.put("result", heatresult);

		try (MockedStatic<HeatEmissionQuery> heq = Mockito.mockStatic(HeatEmissionQuery.class)) {
			heq.when(() -> HeatEmissionQuery.performCrossDomainQ(requestInput)).thenReturn(expected);
			JSONObject actual = (JSONObject) HeatEmissionQuery.performCrossDomainQ(requestInput);
			assertEquals(expected, actual);
		}
	}

	// Test Boundary(JSONObject inputBounds)
	@Test
	public void testBoundary() throws Exception {
		String inputBounds_new = "{\"job\":{\"lower_bounds\":\"8464.256074442204#23588.08319044689#0\",\"upper_bounds\":\"17619.669922658715#30520.376177137474#105\"}}\r\n";
		JSONObject requestInput = new JSONObject(inputBounds_new);
		HeatEmissionQuery Query = new HeatEmissionQuery();
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

		try (MockedStatic<HeatEmissionQuery> heq = Mockito.mockStatic(HeatEmissionQuery.class)) {
			heq.when(() -> HeatEmissionQuery.IRIandCO2Query()).thenReturn(expected);
			JSONArray actual = (JSONArray) HeatEmissionQuery.IRIandCO2Query();
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

		try (MockedStatic<HeatEmissionQuery> heq = Mockito.mockStatic(HeatEmissionQuery.class)) {
			heq.when(() -> HeatEmissionQuery.FuelCEIEfficiency(ChemicalPlant)).thenReturn(expected);
			JSONArray actual = (JSONArray) HeatEmissionQuery.FuelCEIEfficiency(ChemicalPlant);
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

		try (MockedStatic<HeatEmissionQuery> heq = Mockito.mockStatic(HeatEmissionQuery.class)) {
			heq.when(() -> HeatEmissionQuery.CoordinateQuery(CityFurnitureIRI)).thenReturn(expected);
			JSONArray actual = (JSONArray) HeatEmissionQuery.CoordinateQuery(CityFurnitureIRI);
			assertEquals(expected, actual);
		}
	}

	// Test HeatEmissionCoordinate (JSONArray coordiSpatialQueryResult)
	@Test
	public void testHeatEmissionCoordinate() throws Exception {
		JSONArray testInput = new JSONArray(
				"[{\"geometricIRI\":\"Sample1\",\"polygonData\":\"1#1#0#2#1#0#1#2#0#2#2#0\"},{\"geometricIRI\":\"Sample2\",\"polygonData\":\"1#1#5#2#1#5#1#2#0#2#2#5\"},{\"geometricIRI\":\"Sample3\",\"polygonData\":\"1.5#1.5#4#2#2#3#3#3#1#5#5#4.5\"}]");
		HeatEmissionQuery Query = new HeatEmissionQuery();
		Method heatEmissionCoordinate = Query.getClass().getDeclaredMethod("HeatEmissionCoordinate", JSONArray.class);
		assertNotNull(heatEmissionCoordinate);
		heatEmissionCoordinate.setAccessible(true);
		String test_result = (String) heatEmissionCoordinate.invoke(Query, testInput);
		assertEquals(test_result, "1.5#1.5#5");
	}

	// Test sparqlUpdate (String Plant_item, String Heat_value)
	@Test
	public void testSparqlUpdate() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
		HeatEmissionQuery Query = spy(new HeatEmissionQuery());
		Method sparqlUpdate = Query.getClass().getDeclaredMethod("sparqlUpdate", String.class, String.class);
		assertNotNull(sparqlUpdate);

		String Plant_tiem = "http://www.theworldavatar.com/kb/ontochemplant/Plant_item";
		String Heat_value = "100";
		doNothing().when(Query).sparqlUpdate(anyString(), anyString());
		sparqlUpdate.invoke(Query, Plant_tiem, Heat_value);
		verify(Query, times(1)).sparqlUpdate(anyString(), anyString());
	}
}