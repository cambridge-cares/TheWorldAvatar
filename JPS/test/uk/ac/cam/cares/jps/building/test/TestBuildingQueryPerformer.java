package uk.ac.cam.cares.jps.building.test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.http.client.utils.URIBuilder;
import org.openimaj.math.geometry.shape.Polygon;

import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.util.MatrixToJsonConverter;
import uk.ac.cam.cares.jps.building.BuildingQueryPerformer;
import uk.ac.cam.cares.jps.building.CRSTransformer;
import uk.ac.cam.cares.jps.building.SimpleBuildingData;
import uk.ac.cam.cares.jps.building.SimpleShapeConverter;
import uk.ac.cam.cares.jps.building.SparqlConstants;

public class TestBuildingQueryPerformer extends TestCase implements SparqlConstants {
	
	// IRI for old KB of THE Hague
	//public static final String BUILDING_IRI_THE_HAGUE_PREFIX = "http://www.theworldavatar.com/Building/";
	public static final String BUILDING_IRI_THE_HAGUE_PREFIX = "http://www.theworldavatar.com/kb/nld/thehague/buildings/";

	// the following building from The Hague doesn't have any building parts and only contains one ground surface
	public static final String BUILDING_IRI_THE_HAGUE_WITHOUT_PARTS = BUILDING_IRI_THE_HAGUE_PREFIX + "10_buildings0.owl#BuildingGUID_83EFA0E4-FC06-46B3-8482-E38C8CF602BC";
	public static final String BUILDING_IRI_THE_HAGUE_WITH_THREE_PARTS= BUILDING_IRI_THE_HAGUE_PREFIX + "10_buildings0.owl#BuildingGUID_E77C9F0F-554A-4986-8332-75EDFF2DCF07";
	public static final String BUILDING_IRI_THE_HAGUE_PLANT = BUILDING_IRI_THE_HAGUE_PREFIX + "10_buildings0.owl#BuildingGUID_E59B86F5-443F-49AE-BD94-BAB9AAAA278A";
	public static final String BUILDING_IRI_BERLIN_PLANT = "http://www.theworldavatar.com/kb/deu/berlin/buildings/3920_5819.owl#BuildingDEB_LOD2_UUID_ccf8cd7c-b41b-4c1e-a60c-0a645c6c5c4b";
	
	public static BuildingQueryPerformer createQueryPerformerForTheHague() {
	
		// TODO-AE URGENT remove this as soon as we don't need the old KB for The Hague anymore
		if (BUILDING_IRI_THE_HAGUE_PREFIX.equals("http://www.theworldavatar.com/Building/")) {
			return new BuildingQueryPerformer("www.theworldavatar.com", 80, "/damecoolquestion/buildingsLite/query");
		}
		
		return new BuildingQueryPerformer();
	}
	
	public String performQueryOnLocalHost(String dataset, String query) {
		URIBuilder builder = new URIBuilder().setScheme("http").setHost("localhost").setPort(3030)
				.setPath("/" + dataset + "/query")
				.setParameter("query", query);
		return createQueryPerformerForTheHague().executeGet(builder);
	}
	
	public void testTheHagueTenBuildings() {
		
		String query = PREFIX_ONTOCAPE_SYS + PREFIX_ONTOCAPE_SPACE_AND_TIME_EXTENDED + PREFIX_CITYGML +
				"SELECT ?building ?name\n" + 
				"WHERE {\n" + 
				"?building citygml:name ?name .\n" + 
				"?building a citygml:BuildingType .\n" +
				"}\n" +
				"LIMIT 10";
		
		String result = createQueryPerformerForTheHague().performQuery(BuildingQueryPerformer.THE_HAGUE_IRI, query);
		
		Map<String, List<String>> map = MatrixToJsonConverter.fromCsv(result);
		assertEquals(10, map.get("building").size());
		assertEquals(10, map.get("name").size());
	}
	
	public void testTheHagueOneBuildingCenterCoordinates() {
				
		String query = PREFIX_ONTOCAPE_SYS + PREFIX_ONTOCAPE_SPACE_AND_TIME_EXTENDED + PREFIX_CITYGML +
				"SELECT ?x ?y\n" + 
				"WHERE {\n" + 
				"<%s> space_and_time_extended:hasGISCoordinateSystem ?coordinates .\n" + 
				CITYGML_HASCOORDINATES_XY +
				"}\n" +
				"LIMIT 100";
		
		query = BuildingQueryPerformer.format(query, BUILDING_IRI_THE_HAGUE_WITHOUT_PARTS);
		
		System.out.println(query);
		
		String result = createQueryPerformerForTheHague().performQuery(BuildingQueryPerformer.THE_HAGUE_IRI, query);
		
		Map<String, List<String>> map = MatrixToJsonConverter.fromCsv(result);
		assertEquals(1, map.get("x").size());
		assertEquals(1, map.get("y").size());
	}
	
	public void testTheHagueOneBuildingMeasuredHeight() {
		
		String query = createQueryPerformerForTheHague().getQueryBdnHeight(BUILDING_IRI_THE_HAGUE_WITHOUT_PARTS);
		String result = createQueryPerformerForTheHague().performQuery(BuildingQueryPerformer.THE_HAGUE_IRI, query);
		
		Map<String, List<String>> map = MatrixToJsonConverter.fromCsv(result);
		assertEquals(1, map.get("h").size());
		assertEquals("9.432", map.get("h").get(0));
	}
	
	public void testTheHagueOneBuildingOneGroundSurface() {
				
		String query = createQueryPerformerForTheHague().getQueryBdnVerticesWithAndWithoutBuildingParts(BUILDING_IRI_THE_HAGUE_WITHOUT_PARTS);		
		String result = createQueryPerformerForTheHague().performQuery(BuildingQueryPerformer.THE_HAGUE_IRI, query);
		
		Map<String, List<String>> map = MatrixToJsonConverter.fromCsv(result);
		assertEquals(5, map.get("x").size());
		assertEquals(5, map.get("y").size());
		assertEquals(5, map.get("z").size());
		assertEquals("79429.187", map.get("x").get(2));
		assertEquals("454738.596", map.get("y").get(2));
		assertEquals("1.841", map.get("z").get(2));
		
		List<Polygon> polygons = SimpleShapeConverter.convertTo2DPolygons(map, "groundsurface", "x", "y");
		assertEquals(1, polygons.size());
		assertEquals(5, polygons.get(0).size());
	}
	
	public void testTheHaguePlantBuilding() {
				
		// old plant IRI from The Hague
		List<String> buildingIRIs = Arrays.asList(BUILDING_IRI_THE_HAGUE_PLANT);
		SimpleBuildingData data = createQueryPerformerForTheHague().performQuerySimpleBuildingData(BuildingQueryPerformer.THE_HAGUE_IRI, buildingIRIs);
		assertEquals(1, data.BldIRI.size());
	
		//<http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings0.owl#V_x_Building_GUID_E59B86F5-443F-49AE-BD94-BAB9AAAA278A>
		//"79813.66794742201"^^xsd:double
		// the next value is the center of the best shrinked box
		// old KB for The Hauge 
		// assertEquals(79831., data.BldX.get(0), 0.1);
		assertEquals(79822., data.BldX.get(0), 1);
		//<http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings0.owl#V_y_Building_GUID_E59B86F5-443F-49AE-BD94-BAB9AAAA278A>
		//"454799.0413769716"^^xsd:double
		// the next value is the center of the best shrinked box
		//assertEquals(454766.375, data.BldY.get(0), 0.1);
		assertEquals(454783.531, data.BldY.get(0), 1);
		// <http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings0.owl#V_EstimatedHeight_Building_GUID_E59B86F5-443F-49AE-BD94-BAB9AAAA278A>
		//"83.607"^^xsd:double
		// the next value was read from the knowledge base
		assertEquals(83.607, data.BldHeight.get(0), 0.1);
	}
	
	public void testselectClosestBuilding() {
		
		double centerx = 10;
		double centery = 10;
		
		Map<String, List<String>> map = new HashMap<String, List<String>>();
		map.put("bdn", Arrays.asList("b1", "b2", "b3", "b4", "b5"));
		map.put("x", Arrays.asList("9", "5", "9.5", "3", "15"));
		map.put("y", Arrays.asList("11", "16", "10.5", "4", "1"));
		
		List<String> buildingIRIs = createQueryPerformerForTheHague().selectClosestBuilding(centerx, centery, 2, map);
		assertEquals(2, buildingIRIs.size());
		assertEquals("b3", buildingIRIs.get(0));
		assertEquals("b1", buildingIRIs.get(1));
	}
	
	public void testTheHaguePerformQueryCLosestBuildingsFromRegionAroundPlant() {
		
		double plantx = 79831;
		double planty = 454766;
		double lowerx = plantx - 400;
		double lowery = planty - 400;
		double upperx = plantx + 400;
		double uppery = planty + 400;
		
		List<String> buildingIRIs = createQueryPerformerForTheHague().performQueryClosestBuildingsFromRegion( 
				BuildingQueryPerformer.THE_HAGUE_IRI, plantx, planty, 25, lowerx, lowery, upperx, uppery);
				
		assertEquals(25, buildingIRIs.size());
	}
	
	public void testTheHagueBuildingWithParts() {
		
		String query = PREFIX_ONTOCAPE_SYS + PREFIX_ONTOCAPE_SPACE_AND_TIME_EXTENDED + PREFIX_CITYGML +
				"SELECT  distinct ?bdn\n" + 
				"WHERE {\n" + 
				"?bdn citygml:consistsOfBuildingPart ?part .\n" +
				"?part citygml:boundedBy ?groundsurface .\n" +
				"?groundsurface a citygml:GroundSurfaceType .\n" + 
				"}\n" +
				"LIMIT 10";
		
		/**
		http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings0.owl#BuildingGUID_E77C9F0F-554A-4986-8332-75EDFF2DCF07
		http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings0.owl#BuildingGUID_E5F7DC6A-59FE-4DC2-A826-0CABEEB70451
		http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings0.owl#BuildingGUID_94405F3C-FB53-4EC8-93A1-5F95FEC74CBD
		http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings0.owl#BuildingGUID_8D251403-40CD-463F-AE65-AC8951A1B2FD
		http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings0.owl#BuildingGUID_24834678-6E28-47A3-8535-7276348DF078
		http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings0.owl#BuildingGUID_885FA69C-CA8B-46CC-8D34-D38FD14B52C8
		http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings0.owl#BuildingGUID_00075B44-10B6-4E22-8AC1-1E212EAB803E
		http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings0.owl#BuildingGUID_55E12A29-6A2E-4032-BEF0-FA55038D04CB
		http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings0.owl#BuildingGUID_0C6D00D2-8473-4801-9787-3ECEE3720B71
		http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings0.owl#BuildingGUID_612BE82E-7A6A-4356-9A18-0434EF6AA180
		*/
		
		String result = createQueryPerformerForTheHague().performQuery(BuildingQueryPerformer.THE_HAGUE_IRI, query);
		
		Map<String, List<String>> map = MatrixToJsonConverter.fromCsv(result);
		assertEquals(10, map.get("bdn").size());
	}
	
	
	/**
	 * The building has three building parts, i.e. 3 ground surfaces, i.e. 3 polygons 
	 * (each with 5 points where the last point coincides with the first one)
	 */
	public void testTheHagueOneBuildingSeveralParts() {
		
		String query = createQueryPerformerForTheHague().getQueryBdnVerticesWithAndWithoutBuildingParts(BUILDING_IRI_THE_HAGUE_WITH_THREE_PARTS);
		
		String result = createQueryPerformerForTheHague().performQuery(BuildingQueryPerformer.THE_HAGUE_IRI, query);
		
		Map<String, List<String>> map = MatrixToJsonConverter.fromCsv(result);
		List<String> distinctGroundSurface = new ArrayList<String>();
		for (Object current : map.get("groundsurface")) {
			if (!distinctGroundSurface.contains(current)) {
				distinctGroundSurface.add((String) current);
			}
		}
		assertEquals(3, distinctGroundSurface.size());
		assertEquals(15, map.get("x").size());
	
		List<Polygon> polygons = SimpleShapeConverter.convertTo2DPolygons(map, "groundsurface", "x", "y");
		assertEquals(3, polygons.size());
		assertEquals(5, polygons.get(0).size());
		assertEquals(5, polygons.get(1).size());
		assertEquals(5, polygons.get(2).size());
	}
	
	public void testTheHagueOneBuildingSimpleBuildingData() {
		
		List<String> buildingIRIs = Arrays.asList(BUILDING_IRI_THE_HAGUE_WITHOUT_PARTS, BUILDING_IRI_THE_HAGUE_WITH_THREE_PARTS);
	
		SimpleBuildingData result = createQueryPerformerForTheHague().performQuerySimpleBuildingData(BuildingQueryPerformer.THE_HAGUE_IRI, buildingIRIs);
		
		assertEquals(2, result.BldIRI.size());
		int i = 0;
		if (result.BldIRI.get(1).equals(BUILDING_IRI_THE_HAGUE_WITHOUT_PARTS)) {
			i = 1;
		}

		assertTrue("BuildingGUID_83EFA0E4-FC06-46B3-8482-E38C8CF602BC".endsWith(result.BldName.get(i)));
		assertEquals(0, (int) result.BldType.get(i));

		assertEquals(79434.15625, (double) result.BldX.get(i), 0.1);
		assertEquals(454735.8125, (double) result.BldY.get(i), 0.1);
		assertEquals(10.07, (double) result.BldLength.get(i), 0.01);
		assertEquals(5.15, (double) result.BldWidth.get(i), 0.01);
		assertEquals(145.94, (double) result.BldAngle.get(i), 0.01);
		assertEquals(9.432, (double) result.BldHeight.get(i), 0.01);
	}
	
	public void testSimpleBuildingDataWithoutExceptions() {
		
		List<String> buildingIRIs = createQueryPerformerForTheHague().performQueryBuildingsFromRegion(BuildingQueryPerformer.THE_HAGUE_IRI, 25, 79000., 454000., 79800., 455200.);
		SimpleBuildingData result = createQueryPerformerForTheHague().performQuerySimpleBuildingData(BuildingQueryPerformer.THE_HAGUE_IRI, buildingIRIs);
		
		assertEquals(25, result.BldIRI.size());
		
		System.out.println("\n\nJSON building IRIs:");
		String json = new Gson().toJson(buildingIRIs);
		System.out.println(json);
		System.out.println("\n\nJSON Simple Building Data:");
		json = new Gson().toJson(result);
		System.out.println(json);
	}
	
	public void testBerlinPlantBuilding() {
		
		List<String> buildingIRIs = Arrays.asList(BUILDING_IRI_BERLIN_PLANT);
		SimpleBuildingData data = new BuildingQueryPerformer().performQuerySimpleBuildingData(BuildingQueryPerformer.BERLIN_IRI, buildingIRIs);
		assertEquals(1, data.BldIRI.size());
	
		// the center of the best shrinked box is transformed back to Berlin coordinates
		double[] center = new double[] {data.BldX.get(0), data.BldY.get(0)};
		double[] transformed = CRSTransformer.transform(BuildingQueryPerformer.DEFAULT_CRS_NAME, CRSTransformer.EPSG_25833, center);
		
		// <http://www.theworldavatar.com/kb/deu/berlin/buildings/3920_5819.owl#V_x_Building_DEB_LOD2_UUID_ccf8cd7c-b41b-4c1e-a60c-0a645c6c5c4b>
		//"392825.8570880443"^^xsd:double
		assertEquals(392825, transformed[0], 1);
		// <http://www.theworldavatar.com/kb/deu/berlin/buildings/3920_5819.owl#V_y_Building_DEB_LOD2_UUID_ccf8cd7c-b41b-4c1e-a60c-0a645c6c5c4b>
		//"5819122.55186522"^^xsd:double
		// the next value is the center of the best shrinked box
		assertEquals(5819122, transformed[1], 1);
		// <http://www.theworldavatar.com/kb/deu/berlin/buildings/3920_5819.owl#V_EstimatedHeight_Building_DEB_LOD2_UUID_ccf8cd7c-b41b-4c1e-a60c-0a645c6c5c4b>
		//"99.28999999999999"^^xsd:double
		// the next value was read from the knowledge base
		assertEquals(99.29, data.BldHeight.get(0), 0.1);
	}
	
	public void testBerlinPerformQueryBuildingsFromRegionAroundPlant() {
		
		// transform the points into the CRS of GUI, BuildingQueryPerformer will translate them back to Berlin CRS
		String sourceCRS = CRSTransformer.EPSG_25833; // Berlin
		//double[] sourcePoints = new double[]{390000., 5815000., 396000., 5826000.};
		double[] sourcePoints = new double[]{370000., 5805000., 406000., 5926000.};
		String targetCRS = CRSTransformer.EPSG_28992; // The Hague
		double[] targetPoints = CRSTransformer.transform(sourceCRS, targetCRS, sourcePoints);
		
		List<String> buildingIRIs = new BuildingQueryPerformer().performQueryBuildingsFromRegion(BuildingQueryPerformer.BERLIN_IRI, 25, 
				targetPoints[0], targetPoints[1], targetPoints[2], targetPoints[3]);
		assertEquals(25, buildingIRIs.size());
		
		String[] expectedBuildingNames = new String[] {"BuildingBLDG_0003000a0041a96a",
				"BuildingBLDG_0003000a0044ad95",  
				"BuildingBLDG_0003000f00041375",  
				"BuildingBLDG_0003000900350d71",  
				"BuildingDEB_LOD2_UUID_fa5ba7fe-f85d-4a60-a4ba-52df87c6b968",  
				"BuildingDEB_LOD2_UUID_de691875-c5db-476d-abf9-d4e98d9546b4",  
				"BuildingBLDG_0003000f00445803",  
				"BuildingBLDG_0003000000266736",  
				"BuildingBLDG_00030009001b8b74",  
				"BuildingBLDG_0003000f00041373",  
				"BuildingBLDG_00030002003cd9b9",  
				"BuildingBLDG_0003000900350d26",  
				"BuildingBLDG_0003000900350cfa",  
				"BuildingBLDG_0003000900350d1d",  
				"BuildingBLDG_0003000e00006df3",  
				"BuildingBLDG_0003000900350d02",  
				"BuildingBLDG_0003000a0044acab",  
				"BuildingBLDG_0003000900350d0f",  
				"BuildingBLDG_0003000900350d6f",  
				"BuildingDEB_LOD2_UUID_78a2c5f2-8d75-4b06-bdf7-8bdc60b3d751",  
				"BuildingBLDG_0003000e00686a85",  
				"BuildingDEB_LOD2_UUID_22426381-bfbb-48cf-9057-6e58de219076",  
				"BuildingBLDG_0003000f00445810",  
				"BuildingBLDG_0003000900350d00",  
				"BuildingBLDG_00030009001b8ba2"	
			};

		for (String expectedName : expectedBuildingNames) {
			boolean found = false;
			for (String buildingIRI : buildingIRIs) {
				String[] splits = buildingIRI.split("#");
				if (expectedName.equals(splits[1])) {
					found = true;
					break;
				}
			}	
			assertTrue("building not found, buildingName = " + expectedName, found);
		}
	}
	
	public void testBerlinPerformQueryCLosestBuildingsFromRegionAroundPlant() {

		double plantx = 392825;
		double planty = 5819122;
		
		// transform the points around the plant in Berlin into the CRS of GUI, BuildingQueryPerformer will translate them back to Berlin CRS
		// this transformed target plants are the correct ones for ADMS Python scripts
		String sourceCRS = CRSTransformer.EPSG_25833; // Berlin
		double[] sourceCenter = new double[]{plantx, planty};
		String targetCRS = CRSTransformer.EPSG_28992; // The Hague
		double[] targetCenter = CRSTransformer.transform(sourceCRS, targetCRS, sourceCenter);
		
		double lowerx = targetCenter[0] - 400;
		double lowery = targetCenter[1] - 400;
		double upperx = targetCenter[0] + 400;
		double uppery = targetCenter[1] + 400;
		
		List<String> buildingIRIs = createQueryPerformerForTheHague().performQueryClosestBuildingsFromRegion( 
				BuildingQueryPerformer.BERLIN_IRI, plantx, planty, 25, lowerx, lowery, upperx, uppery);
				
		assertEquals(25, buildingIRIs.size());
	}
}
