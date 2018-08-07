package uk.ac.cam.cares.jps.building.test;

import java.util.ArrayList;
import java.util.Arrays;
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
	
	private boolean queryServer = true;
	// the following building from The Hague doesn't have any building parts and only contains one ground surface
	public static final String BUILDING_IRI_THE_HAGUE_WITHOUT_PARTS = "http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_83EFA0E4-FC06-46B3-8482-E38C8CF602BC";
	public static final String BUILDING_IRI_THE_HAGUE_WITH_THREE_PARTS= "http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_E77C9F0F-554A-4986-8332-75EDFF2DCF07";
	public static final String BUILDING_IRI_THE_HAGUE_PLANT = "http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_E59B86F5-443F-49AE-BD94-BAB9AAAA278A";
	
	public String performQuery(String city, String query) {
		
		String result = null;
		
		if (!queryServer) {
			if (city.equalsIgnoreCase(BuildingQueryPerformer.BERLIN_IRI)) {
				result = performQueryOnLocalHost("berlinbuildings", query);
			}
		} else {
			result = new BuildingQueryPerformer().performQuery(city, query);
		}
		
		System.out.println(result);
		
		return result;
	}
	
	public String performQueryOnLocalHost(String dataset, String query) {
		URIBuilder builder = new URIBuilder().setScheme("http").setHost("localhost").setPort(3030)
				.setPath("/" + dataset + "/query")
				.setParameter("query", query);
		return new BuildingQueryPerformer().executeGet(builder);
	}
	
	public void testTheHagueTenBuildings() {
		
		String query = PREFIX_ONTOCAPE_SYS + PREFIX_ONTOCAPE_SPACE_AND_TIME_EXTENDED + PREFIX_CITYGML +
				"SELECT ?building ?name\n" + 
				"WHERE {\n" + 
				"?building citygml:name ?name .\n" + 
				"?building a citygml:BuildingType .\n" +
				"}\n" +
				"LIMIT 10";
		
		String result = new BuildingQueryPerformer().performQuery(BuildingQueryPerformer.THE_HAGUE_IRI, query);
		
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
		
		query = String.format(query, BUILDING_IRI_THE_HAGUE_WITHOUT_PARTS);
		
		System.out.println(query);
		
		String result = new BuildingQueryPerformer().performQuery(BuildingQueryPerformer.THE_HAGUE_IRI, query);
		
		Map<String, List<String>> map = MatrixToJsonConverter.fromCsv(result);
		assertEquals(1, map.get("x").size());
		assertEquals(1, map.get("y").size());
	}
	
	public void testTheHagueOneBuildingMeasuredHeight() {
		
		String query = new BuildingQueryPerformer().getQueryBdnHeight(BUILDING_IRI_THE_HAGUE_WITHOUT_PARTS);
		String result = new BuildingQueryPerformer().performQuery(BuildingQueryPerformer.THE_HAGUE_IRI, query);
		
		Map<String, List<String>> map = MatrixToJsonConverter.fromCsv(result);
		assertEquals(1, map.get("h").size());
		assertEquals("9.432", map.get("h").get(0));
	}
	
	public void testTheHagueOneBuildingOneGroundSurface() {
				
		String query = new BuildingQueryPerformer().getQueryBdnVerticesWithAndWithoutBuildingParts(BUILDING_IRI_THE_HAGUE_WITHOUT_PARTS);		
		String result = new BuildingQueryPerformer().performQuery(BuildingQueryPerformer.THE_HAGUE_IRI, query);
		
		Map<String, List<String>> map = MatrixToJsonConverter.fromCsv(result);
		assertEquals(5, map.get("x").size());
		assertEquals(5, map.get("y").size());
		assertEquals(5, map.get("z").size());
		assertEquals("79429.187", map.get("x").get(2));
		assertEquals("454738.596", map.get("y").get(2));
		assertEquals("1.841", map.get("z").get(2));
		
		List<Polygon> polygons = SimpleShapeConverter.convertTo2DPolygons(result, "groundsurface", "x", "y");
		assertEquals(1, polygons.size());
		assertEquals(5, polygons.get(0).size());
	}
	
	public void testTheHaguePlantBuilding() {
				
		// old plant IRI from The Hague
		List<String> buildingIRIs = Arrays.asList(BUILDING_IRI_THE_HAGUE_PLANT);
		SimpleBuildingData data = new BuildingQueryPerformer().performQuerySimpleBuildingData(BuildingQueryPerformer.THE_HAGUE_IRI, buildingIRIs);
		assertEquals(1, data.BldIRI.size());
	
		//<http://www.theworldavatar.com/Building/10_buildings0.owl#V_x_Building_GUID_E59B86F5-443F-49AE-BD94-BAB9AAAA278A>
		//"79813.66794742201"^^xsd:double
		// the next value is the center of the best shrinked box
		assertEquals(79831., data.BldX.get(0), 0.1);
		//<http://www.theworldavatar.com/Building/10_buildings0.owl#V_y_Building_GUID_E59B86F5-443F-49AE-BD94-BAB9AAAA278A>
		//"454799.0413769716"^^xsd:double
		// the next value is the center of the best shrinked box
		assertEquals(454766.375, data.BldY.get(0), 0.1);
		// <http://www.theworldavatar.com/Building/10_buildings0.owl#V_EstimatedHeight_Building_GUID_E59B86F5-443F-49AE-BD94-BAB9AAAA278A>
		//"83.607"^^xsd:double
		// the next value was read from the knowledge base
		assertEquals(83.607, data.BldHeight.get(0), 0.1);
	}
	
	public void testTheHaguePerformQueryBuildingsFromRegionAroundPlant() {
		
		List<String> buildingIRIs = new BuildingQueryPerformer().performQueryBuildingsFromRegion(BuildingQueryPerformer.THE_HAGUE_IRI, 25, 79000., 454000., 79800., 455200.);
		assertEquals(25, buildingIRIs.size());
		
		String[] expectedBuildingNames = new String[] {
				"BuildingGUID_83EFA0E4-FC06-46B3-8482-E38C8CF602BC", "BuildingGUID_21FFA968-1D0D-46F1-9C6A-DEB511EDE8EC", "BuildingGUID_E7EAC652-9675-4075-9B77-9119130FFC01", 
				"BuildingGUID_0DDFE8F6-C689-411B-A40B-7AB0B322DAA4", "BuildingGUID_75633FA6-1816-4681-AED1-28477B9E8306", "BuildingGUID_8FD8BE3F-EA1A-43E5-B1C3-C8C041F93F8F", 
				"BuildingGUID_AA6B15F5-3CFA-43F6-AB89-E775D5417EA5", "BuildingGUID_9A460324-F094-49C8-99CC-8D22EFAAD44A", "BuildingGUID_175A22B3-5F74-48B7-B3D9-6B63262428C8", 
				"BuildingGUID_8E8447BD-78FB-43E3-B58B-9E3D08969CE2", "BuildingGUID_F5DB3C73-0EEC-400D-8ADC-12D9B3EBECCB", "BuildingGUID_CEA4BE77-3E65-4839-BA27-96862FB6F94F", 
				"BuildingGUID_A8192F36-EC80-4BDB-8180-98F82986F85C", "BuildingGUID_BB852817-BC7D-4DDF-A6FD-5C7A48C7D4F5", "BuildingGUID_7B178A87-680D-49DF-AB8B-2241E3E03C19", 
				"BuildingGUID_20D43A4B-A213-473B-BACE-2AC07274EEF0", "BuildingGUID_797DD1C0-0D05-439B-8BC1-10E85FB992C5", "BuildingGUID_73E89625-9CE2-4A7C-8A07-0E66B497366A", 
				"BuildingGUID_A99CBE3D-710E-46C5-B7C1-3205D6E50E88", "BuildingGUID_B2BB7029-D5EF-46B7-9517-918121935409", "BuildingGUID_9B9071F4-A36F-4A1F-AC8E-1BECF958A2E7", 
				"BuildingGUID_8E3E7C70-E12B-4413-889C-A8809EC95F3B", "BuildingGUID_C7AA599B-C39B-4459-AD40-B0F661FE1D8C", "BuildingGUID_780A7409-5CDE-4E85-A29C-08DA3796DB31", 
				"BuildingGUID_244EC50A-A357-44F5-BA8A-422C4EDAF37E"};

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
		http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_E77C9F0F-554A-4986-8332-75EDFF2DCF07
		http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_E5F7DC6A-59FE-4DC2-A826-0CABEEB70451
		http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_94405F3C-FB53-4EC8-93A1-5F95FEC74CBD
		http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_8D251403-40CD-463F-AE65-AC8951A1B2FD
		http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_24834678-6E28-47A3-8535-7276348DF078
		http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_885FA69C-CA8B-46CC-8D34-D38FD14B52C8
		http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_00075B44-10B6-4E22-8AC1-1E212EAB803E
		http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_55E12A29-6A2E-4032-BEF0-FA55038D04CB
		http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_0C6D00D2-8473-4801-9787-3ECEE3720B71
		http://www.theworldavatar.com/Building/10_buildings0.owl#BuildingGUID_612BE82E-7A6A-4356-9A18-0434EF6AA180
		*/
		
		String result = new BuildingQueryPerformer().performQuery(BuildingQueryPerformer.THE_HAGUE_IRI, query);
		
		Map<String, List<String>> map = MatrixToJsonConverter.fromCsv(result);
		assertEquals(10, map.get("bdn").size());
	}
	
	
	/**
	 * The building has three building parts, i.e. 3 ground surfaces, i.e. 3 polygons 
	 * (each with 5 points where the last point coincides with the first one)
	 */
	public void testTheHagueOneBuildingSeveralParts() {
		
		String query = new BuildingQueryPerformer().getQueryBdnVerticesWithAndWithoutBuildingParts(BUILDING_IRI_THE_HAGUE_WITH_THREE_PARTS);
		
		String result = new BuildingQueryPerformer().performQuery(BuildingQueryPerformer.THE_HAGUE_IRI, query);
		
		Map<String, List<String>> map = MatrixToJsonConverter.fromCsv(result);
		List<String> distinctGroundSurface = new ArrayList<String>();
		for (Object current : map.get("groundsurface")) {
			if (!distinctGroundSurface.contains(current)) {
				distinctGroundSurface.add((String) current);
			}
		}
		assertEquals(3, distinctGroundSurface.size());
		assertEquals(15, map.get("x").size());

		List<Polygon> polygons = SimpleShapeConverter.convertTo2DPolygons(result, "groundsurface", "x", "y");
		assertEquals(3, polygons.size());
		assertEquals(5, polygons.get(0).size());
		assertEquals(5, polygons.get(1).size());
		assertEquals(5, polygons.get(2).size());
	}
	
	public void testTheHagueOneBuildingSimpleBuildingData() {
		
		List<String> buildingIRIs = Arrays.asList(BUILDING_IRI_THE_HAGUE_WITHOUT_PARTS, BUILDING_IRI_THE_HAGUE_WITH_THREE_PARTS);
	
		SimpleBuildingData result = new BuildingQueryPerformer().performQuerySimpleBuildingData(BuildingQueryPerformer.THE_HAGUE_IRI, buildingIRIs);
		
		assertEquals(2, result.BldIRI.size());
		int i = 0;
		if (result.BldIRI.get(1).equals(BUILDING_IRI_THE_HAGUE_WITHOUT_PARTS)) {
			i = 1;
		}

		assertEquals("BuildingGUID_83EFA0E4-FC06-46B3-8482-E38C8CF602BC", result.BldName.get(i));
		assertEquals(0, (int) result.BldType.get(i));

		assertEquals(79434.15625, (double) result.BldX.get(i), 0.1);
		assertEquals(454735.8125, (double) result.BldY.get(i), 0.1);
		assertEquals(10.145, (double) result.BldLength.get(i), 0.01);
		assertEquals(5.119, (double) result.BldWidth.get(i), 0.01);
		assertEquals(145.955, (double) result.BldAngle.get(i), 0.01);
		assertEquals(9.432, (double) result.BldHeight.get(i), 0.01);
	}
	
	public void testSimpleBuildingDataWithoutExceptions() {
		
		List<String> buildingIRIs = new BuildingQueryPerformer().performQueryBuildingsFromRegion(BuildingQueryPerformer.THE_HAGUE_IRI, 25, 79000., 454000., 79800., 455200.);
		SimpleBuildingData result = new BuildingQueryPerformer().performQuerySimpleBuildingData(BuildingQueryPerformer.THE_HAGUE_IRI, buildingIRIs);
		
		assertEquals(25, result.BldIRI.size());
		
		System.out.println("\n\nJSON building IRIs:");
		String json = new Gson().toJson(buildingIRIs);
		System.out.println(json);
		System.out.println("\n\nJSON Simple Building Data:");
		json = new Gson().toJson(result);
		System.out.println(json);
	}
	
	public void testBerlinPlantBuilding() {
		
		// plant IRI from Berlin
		List<String> buildingIRIs = Arrays.asList("http://www.theworldavatar.com/kb/deu/berlin/buildings/3920_5819.owl#BuildingDEB_LOD2_UUID_ccf8cd7c-b41b-4c1e-a60c-0a645c6c5c4b");
		SimpleBuildingData data = new BuildingQueryPerformer().performQuerySimpleBuildingData(BuildingQueryPerformer.BERLIN_IRI, buildingIRIs);
		assertEquals(1, data.BldIRI.size());
	
		// <http://www.theworldavatar.com/kb/deu/berlin/buildings/3920_5819.owl#V_x_Building_DEB_LOD2_UUID_ccf8cd7c-b41b-4c1e-a60c-0a645c6c5c4b>
		//"392825.8570880443"^^xsd:double
		// the next value is the center of the best shrinked box
		assertEquals(392821.625, data.BldX.get(0), 0.1);
		// <http://www.theworldavatar.com/kb/deu/berlin/buildings/3920_5819.owl#V_y_Building_DEB_LOD2_UUID_ccf8cd7c-b41b-4c1e-a60c-0a645c6c5c4b>
		//"5819122.55186522"^^xsd:double
		// the next value is the center of the best shrinked box
		assertEquals(5819111.0, data.BldY.get(0), 0.1);
		// <http://www.theworldavatar.com/kb/deu/berlin/buildings/3920_5819.owl#V_EstimatedHeight_Building_DEB_LOD2_UUID_ccf8cd7c-b41b-4c1e-a60c-0a645c6c5c4b>
		//"99.28999999999999"^^xsd:double
		// the next value was read from the knowledge base
		assertEquals(99.29, data.BldHeight.get(0), 0.1);
	}
	
	public void testBerlinPerformQueryBuildingsFromRegionAroundPlant() {
		
		// tranform the points into the CRS of GUI, BuildingQueryPerformer will translate them back to Berlin CRS
		String sourceCRS = CRSTransformer.EPSG_25833; // Berlin
		double[] sourcePoints = new double[]{390000., 5815000., 396000., 5826000.};
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
	
	//TODO-AE remove this
	public void xxxtestOLDQuery1 () {
		
		String query = 
		"PREFIX p3: <http://www.theworldavatar.com/CityGMLOntology.owl#>\r\n" + 
		"PREFIX j1: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n" + 
		"PREFIX j2: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>\r\n" + 
		"SELECT ?polygon ?coordinates ?xval ?yval ?zval\r\n" + 
		"WHERE {\r\n" + 
		"                {\r\n" + 
		"                    <http://www.theworldavatar.com/Building/3940_5818.owl#BuildingBLDG_0003000900350d26> p3:id ?buildingID .\r\n" + 
		"                    <http://www.theworldavatar.com/Building/3940_5818.owl#BuildingBLDG_0003000900350d26> p3:boundedBy ?groundSurface .\r\n" + 
		"                     ?groundSurface a p3:GroundSurfaceType .\r\n" + 
		"                     ?groundSurface p3:lod2MultiSurface ?multiSurface .\r\n" + 
		"                      ?multiSurface p3:surfaceMember ?polygon .\r\n" + 
		"                       ?polygon p3:exterior ?linearRing .\r\n" + 
		"                        ?linearRing j2:contains ?points .\r\n" + 
		"                         ?points j1:hasGISCoordinateSystem ?coordinates .\r\n" + 
		"                          ?coordinates j1:hasProjectedCoordinate_x ?x .\r\n" + 
		"                           ?x j2:hasValue ?xv .\r\n" + 
		"                            ?xv j2:numericalValue ?xval .\r\n" + 
		"                          ?coordinates j1:hasProjectedCoordinate_y ?y .\r\n" + 
		"                           ?y j2:hasValue ?yv .\r\n" + 
		"                            ?yv j2:numericalValue ?yval.\r\n" + 
		"                          ?coordinates j1:hasProjectedCoordinate_z ?z .\r\n" + 
		"                           ?z j2:hasValue ?zv .\r\n" + 
		"                            ?zv j2:numericalValue ?zval\r\n" + 
		"                }\r\n" + 
		"                UNION\r\n" + 
		"                {\r\n" + 
		"                     <http://www.theworldavatar.com/Building/3940_5818.owl#BuildingBLDG_0003000900350d26> p3:consistsOfBuildingPart ?buildingPart .\r\n" + 
		"                     ?buildingPart p3:boundedBy ?groundSurface .\r\n" + 
		"                     ?groundSurface a p3:GroundSurfaceType .\r\n" + 
		"                     ?groundSurface p3:lod2MultiSurface ?multiSurfaceType .\r\n" + 
		"                      ?multiSurfaceType p3:surfaceMember ?polygon .\r\n" + 
		"                       ?polygon p3:exterior ?linearRing .\r\n" + 
		"                        ?linearRing j2:contains ?points .\r\n" + 
		"                         ?points j1:hasGISCoordinateSystem ?coordinates .\r\n" + 
		"                          ?coordinates j1:hasProjectedCoordinate_x ?x .\r\n" + 
		"                           ?x j2:hasValue ?xv .\r\n" + 
		"                            ?xv j2:numericalValue ?xval .\r\n" + 
		"                          ?coordinates j1:hasProjectedCoordinate_y ?y .\r\n" + 
		"                           ?y j2:hasValue ?yv .\r\n" + 
		"                            ?yv j2:numericalValue ?yval .\r\n" + 
		"                          ?coordinates j1:hasProjectedCoordinate_z ?z .\r\n" + 
		"                           ?z j2:hasValue ?zv .\r\n" + 
		"                            ?zv j2:numericalValue ?zval\r\n" + 
		"                }\r\n" + 
		"}\r\n" + 
		"ORDER BY ?polygon ?points";
		
		System.out.println(query);
	}
	
	//TODO-AE remove this
	public void xxxtestOLDBerlinPlant() {
		String query = "PREFIX sys: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>\r\n" + 
		"PREFIX space_and_time_extended: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n" + 
		"PREFIX citygml:<http://www.theworldavatar.com/CityGMLOntology.owl#>\r\n" + 
		"\r\n" + 
		"SELECT  ?pred ?obj\r\n" + 
		"WHERE {\r\n" + 
		"<http://www.theworldavatar.com/kb/deu/berlin/buildings/3920_5819.owl#BuildingDEB_LOD2_UUID_ccf8cd7c-b41b-4c1e-a60c-0a645c6c5c4b> ?pred ?obj .\r\n" + 
		"}";
		
		query = "PREFIX sys: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>\r\n" + 
				"PREFIX space_and_time_extended: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n" + 
				"PREFIX citygml:<http://www.theworldavatar.com/CityGMLOntology.owl#>\r\n" + 
				"\r\n" + 
				"SELECT  ?pred ?obj\r\n" + 
				"WHERE {\r\n" + 
				"<http://www.theworldavatar.com/kb/deu/berlin/buildings/3940_5818.owl#BuildingBLDG_0003000a0041a96a> ?pred ?obj .\r\n" + 
				"}\r\n" +
				"LIMIT 100";
		
		query = "PREFIX sys: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>\r\n" + 
				"PREFIX space_and_time_extended: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n" + 
				"PREFIX citygml:<http://www.theworldavatar.com/CityGMLOntology.owl#>\r\n" + 
				"\r\n" + 
				"SELECT  ?building ?name\r\n" + 
				"WHERE {\r\n" + 
				"?building <http://www.theworldavatar.com/CityGMLOntology.owl#name> ?name .\r\n" + 
				"}\r\n" +
				"LIMIT 100";
		
//		query = "PREFIX sys: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>\r\n" + 
//				"PREFIX space_and_time_extended: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n" + 
//				"PREFIX citygml:<http://www.theworldavatar.com/CityGMLOntology.owl#>\r\n" + 
//				"\r\n" + 
//				"SELECT  ?pred ?obj\r\n" + 
//				"WHERE {\r\n" + 
//				"<http://www.theworldavatar.com/kb/deu/berlin/buildings/3940_5818.owl#V_EstimatedHeight_Building_BLDG_0003000a0041a96a> ?pred ?obj .\r\n" + 
//				"}\r\n" +
//				"LIMIT 25";
		
		
		//String result = performQuery("berlinbuildings", query);
		String result = new BuildingQueryPerformer().performQuery(BuildingQueryPerformer.BERLIN_IRI, query);
		
		System.out.println(result);
		

		
		
		//double[] p = CRSTransformer.transform(CRSTransformer.EPSG_25833, BuildingQueryPerformer.DEFAULT_CRS_NAME, 390000., 5815000., 396000., 5826000.);
		//new BuildingQueryPerformer().performQueryFilterBdns(BuildingQueryPerformer.BERLIN, 100, p[0], p[1], p[2], p[3]);
		
		
		
		// ZULETZT:
		// Compare the result of both query concerning how many building and coordinate transf. and assert !!!
		

		//https://rdflib.github.io/sparqlwrapper/
		// rdflib.plugins.sparql.results.jsonresults
		//http://rdflib.readthedocs.io/en/stable/_modules/rdflib/plugins/sparql/results/jsonresults.html
	}
}
