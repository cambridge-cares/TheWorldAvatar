package uk.ac.cam.cares.jps.building.test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.openimaj.math.geometry.shape.Polygon;

import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.building.BuildingQueryPerformer;
import uk.ac.cam.cares.jps.building.CRSTransformer;
import uk.ac.cam.cares.jps.building.SimpleBuildingData;
import uk.ac.cam.cares.jps.building.SimpleShapeConverter;
import uk.ac.cam.cares.jps.building.SparqlConstants;

public class TestBuildingQueryPerformer extends TestCase implements SparqlConstants {
	
	public static final String BUILDING_IRI_THE_HAGUE_PREFIX = "http://www.theworldavatar.com/kb/nld/thehague/buildings/";

	// the following building from The Hague doesn't have any building parts and only contains one ground surface
	public static final String BUILDING_IRI_THE_HAGUE_WITHOUT_PARTS = BUILDING_IRI_THE_HAGUE_PREFIX + "10_buildings0.owl#BuildingGUID_83EFA0E4-FC06-46B3-8482-E38C8CF602BC";
	public static final String BUILDING_IRI_THE_HAGUE_WITH_THREE_PARTS= BUILDING_IRI_THE_HAGUE_PREFIX + "10_buildings0.owl#BuildingGUID_E77C9F0F-554A-4986-8332-75EDFF2DCF07";
	public static final String BUILDING_IRI_THE_HAGUE_PLANT = BUILDING_IRI_THE_HAGUE_PREFIX + "10_buildings0.owl#BuildingGUID_E59B86F5-443F-49AE-BD94-BAB9AAAA278A";
	public static final String BUILDING_IRI_BERLIN_PLANT = "http://www.theworldavatar.com/kb/deu/berlin/buildings/3920_5819.owl#BuildingDEB_LOD2_UUID_ccf8cd7c-b41b-4c1e-a60c-0a645c6c5c4b";
	
	public static BuildingQueryPerformer createQueryPerformerForTheHague() {
		return new BuildingQueryPerformer();
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
		
		Map<String, List<String>> map = MatrixConverter.fromCsv(result);
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
		
		Map<String, List<String>> map = MatrixConverter.fromCsv(result);
		assertEquals(1, map.get("x").size());
		assertEquals(1, map.get("y").size());
	}
	
	public void testTheHagueOneBuildingMeasuredHeight() {
		
		String query = createQueryPerformerForTheHague().getQueryBdnHeight(BUILDING_IRI_THE_HAGUE_WITHOUT_PARTS);
		String result = createQueryPerformerForTheHague().performQuery(BuildingQueryPerformer.THE_HAGUE_IRI, query);
		
		Map<String, List<String>> map = MatrixConverter.fromCsv(result);
		assertEquals(1, map.get("h").size());
		assertEquals("9.432", map.get("h").get(0));
	}
	
	public void testTheHagueOneBuildingOneGroundSurface() {
				
		String query = createQueryPerformerForTheHague().getQueryBdnVerticesWithAndWithoutBuildingParts(BUILDING_IRI_THE_HAGUE_WITHOUT_PARTS);		
		String result = createQueryPerformerForTheHague().performQuery(BuildingQueryPerformer.THE_HAGUE_IRI, query);
		
		Map<String, List<String>> map = MatrixConverter.fromCsv(result);
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
	
	public void testBERLINOneBuildingOneGroundSurface() {
		
		String query = createQueryPerformerForTheHague().getQueryBdnVerticesWithAndWithoutBuildingParts("http://www.theworldavatar.com/kb/deu/berlin/buildings/3920_5819.owl#BuildingBLDG_0003000f0029558f");		
		String result = createQueryPerformerForTheHague().performQuery(BuildingQueryPerformer.BERLIN_IRI, query);
		
		Map<String, List<String>> map = MatrixConverter.fromCsv(result);
		assertEquals(26, map.get("x").size());
		assertEquals(26, map.get("y").size());
		assertEquals(26, map.get("z").size());
		assertEquals("392747.720106856", map.get("x").get(2));
		assertEquals("5819071.11806408", map.get("y").get(2));
		assertEquals("35.3300018310547", map.get("z").get(2));
		
		List<Polygon> polygons = SimpleShapeConverter.convertTo2DPolygons(map, "groundsurface", "x", "y");
		assertEquals(4, polygons.size());
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
	
	
	//temporarily disabled to try the new selection method
	
	public void testselectClosestBuilding() {
		
		double centerx = 10;
		double centery = 10;
		
		Map<String, List<String>> map = new HashMap<String, List<String>>();
		map.put("bdn", Arrays.asList("b1", "b2", "b3", "b4", "b5"));
		map.put("x", Arrays.asList("9", "5", "9.5", "3", "15"));
		map.put("y", Arrays.asList("11", "16", "10.5", "4", "1"));
		map.put("h", Arrays.asList("40", "30", "20.5", "10.3", "10"));
		
		List<String> buildingIRIs = createQueryPerformerForTheHague().selectClosestBuilding(centerx, centery, 2, map,20.0);
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
		
		System.out.println(new Gson().toJson(buildingIRIs));
				
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
		
		Map<String, List<String>> map = MatrixConverter.fromCsv(result);
		assertEquals(10, map.get("bdn").size());
	}
	
	
	/**
	 * The building has three building parts, i.e. 3 ground surfaces, i.e. 3 polygons 
	 * (each with 5 points where the last point coincides with the first one)
	 */
	public void testTheHagueOneBuildingSeveralParts() {
		
		String query = createQueryPerformerForTheHague().getQueryBdnVerticesWithAndWithoutBuildingParts(BUILDING_IRI_THE_HAGUE_WITH_THREE_PARTS);
		
		String result = createQueryPerformerForTheHague().performQuery(BuildingQueryPerformer.THE_HAGUE_IRI, query);
		
		Map<String, List<String>> map = MatrixConverter.fromCsv(result);
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
		
		List<String> buildingIRIs = new BuildingQueryPerformer().performQueryClosestBuildingsFromRegion(BuildingQueryPerformer.THE_HAGUE_IRI, 79831, 454766,  25, 79000., 454000., 79800., 455200.);
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
	
	public void testgetqueryofclosestbuildingandresult() {
		//int buildingLimit=25;
		double lx= 1.270664E7;
		double ly=2545539.0 ;
		double ux=1.27082E7 ;
		double uy=2547099.0 ;
		
		BuildingQueryPerformer v= new BuildingQueryPerformer();
		String query = v.getQueryClosestBuildingsFromRegion(200, lx, ly, ux, uy);
		System.out.println("queryformat= " +query);
		
		String queryresult=v.performQuery("http://dbpedia.org/resource/Hong_Kong", query);
		System.out.println("queryresult= " +queryresult);
	}
}
