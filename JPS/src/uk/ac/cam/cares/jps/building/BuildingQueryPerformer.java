package uk.ac.cam.cares.jps.building;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.openimaj.math.geometry.point.Point2d;
import org.openimaj.math.geometry.point.Point2dImpl;
import org.openimaj.math.geometry.shape.Polygon;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.building.SimpleShapeConverter.SimpleShape;

public class BuildingQueryPerformer implements SparqlConstants {

	// TODO-AE should be changed to "EPSG:4326"; // WGS 84 which is the standard of GeoJSON, GIS, Google
	public static final String DEFAULT_CRS_NAME = CRSTransformer.EPSG_28992;
	public static final String BERLIN_IRI = "http://dbpedia.org/page/Berlin";
	public static final String THE_HAGUE_IRI = "http://dbpedia.org/page/The_Hague";
	
	private Logger logger = LoggerFactory.getLogger(BuildingQueryPerformer.class);
	private String host;
	private int port;
	private String path;

	
	public BuildingQueryPerformer() {	
	}
	
	public BuildingQueryPerformer(String host, int port, String path) {
		this.host = host;
		this.port = port;
		this.path = path;
	}
	
	public String performQuery(String cityIRI, String query) {
		
		logger.debug("city = " + cityIRI);
		logger.debug("query = \n" + query);
		
		String myHost = host;
		int myPort = port;
		String myPath = path;
		// TODO-AE hard coded hosts and paths and datasets but can be overwritten be parameter in constructor
		if (myHost == null) {
			myHost = "www.theworldavatar.com";
			myPort = 80;
			if (cityIRI.equalsIgnoreCase(BERLIN_IRI)) {
				myPath = "/damecoolquestion/berlinbuildings/query";
			} else if (cityIRI.equalsIgnoreCase(THE_HAGUE_IRI)) {
				// the old dataset for The Hague with different IRIs
				//myPath = "/damecoolquestion/buildingsLite/query";
				myPath = "/damecoolquestion/thehaguebuildings/query";
			}
		}
		
		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(myPath)
				.setParameter("query", query);
	
		String result = executeGet(builder);
		
		logger.debug("query result = \n" + result);
		
		return result;
	}
	
	// TODO-AE: move method to JPS BASE (AgentCaller)
	public String executeGet(URIBuilder builder) {
		try {
			URI uri = builder.build();
			logger.debug(uri.toString());
			HttpGet request = new HttpGet(uri);
			request.setHeader(HttpHeaders.ACCEPT, "text/csv");
			//request.setHeader(HttpHeaders.ACCEPT, "application/json");
			//request.setHeader(HttpHeaders.ACCEPT, "application/sparql-results+json");
			HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
			
			if (httpResponse.getStatusLine().getStatusCode() != 200) {
				throw new JPSRuntimeException("HTTP response with error = " + httpResponse.getStatusLine());
			}
		
			return EntityUtils.toString(httpResponse.getEntity());
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		} 
	}
	
	public String getCRSName(String cityIRI) {
		
		// the following query will return for the hague: urn:ogc:def:crs:EPSG::28992
		// therefore, we will not use the query at the moment
		String query = 
				"PREFIX sys: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>\r\n" + 
				"PREFIX space_and_time_extended: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n" + 
				"PREFIX citygml:<http://www.theworldavatar.com/CityGMLOntology.owl#>\r\n" + 
				"SELECT ?type\r\n" + 
				"WHERE {\r\n" + 
				"?srs citygml:srsname ?type\r\n" + 
				"}";
		
		if (cityIRI.equalsIgnoreCase(BERLIN_IRI)) {
			return CRSTransformer.EPSG_25833;
		} else if (cityIRI.equalsIgnoreCase(THE_HAGUE_IRI)) {
			return CRSTransformer.EPSG_28992;
		} 
		
		return DEFAULT_CRS_NAME;
	}
	
	public List<String> performQueryBuildingsFromRegion(String cityIRI, int buildingLimit, double lowerx, double lowery, double upperx, double uppery) {
		
		double lx = lowerx;
		double ux = upperx;
		double ly = lowery;
		double uy = uppery;
		
		String targetCRSName = getCRSName(cityIRI);
		if (!DEFAULT_CRS_NAME.equals(targetCRSName)) {
			
			double[] p = CRSTransformer.transform(DEFAULT_CRS_NAME, targetCRSName, new double[] {lowerx, lowery});
			lx = p[0];
			ly = p[1];
			p = CRSTransformer.transform(DEFAULT_CRS_NAME, targetCRSName, new double[] {upperx, uppery});
			ux = p[0];
			uy = p[1];
		}
		
		String query = getQueryBuildingsFromRegion(buildingLimit, lx, ly, ux, uy);		
		String result = performQuery(cityIRI, query);
		Map<String, List<String>> map = MatrixConverter.fromCsv(result);
		return map.get("bdn");
	}
	
	public List<String> performQueryClosestBuildingsFromRegion(String cityIRI, double plantx, double planty, int buildingLimit, double lowerx, double lowery, double upperx, double uppery) {
		
		double plx = plantx;
		double ply = planty;
		double lx = lowerx;
		double ux = upperx;
		double ly = lowery;
		double uy = uppery;
		
		String targetCRSName = getCRSName(cityIRI);
		if (!DEFAULT_CRS_NAME.equals(targetCRSName)) {
			
			double[] p = CRSTransformer.transform(DEFAULT_CRS_NAME, targetCRSName, new double[] {plantx, planty});
			plx = p[0];
			ply = p[1];
			p = CRSTransformer.transform(DEFAULT_CRS_NAME, targetCRSName, new double[] {lowerx, lowery});
			lx = p[0];
			ly = p[1];
			p = CRSTransformer.transform(DEFAULT_CRS_NAME, targetCRSName, new double[] {upperx, uppery});
			ux = p[0];
			uy = p[1];
		}
		
		String query = getQueryClosestBuildingsFromRegion(200, lx, ly, ux, uy);		
		String result = performQuery(cityIRI, query);
		Map<String, List<String>> map = MatrixConverter.fromCsv(result);
		
		return selectClosestBuilding(plx, ply, buildingLimit, map);
	}
	
	public List<String> selectClosestBuilding(double centerx, double centery, int buildingLimit, Map<String, List<String>> map) {
		
		List<String> result = new ArrayList<String>();
		
		class DistanceBuildingPair {
			double distance;
			String buildingIRI;
		}
		
		int size = map.get("bdn").size();
		DistanceBuildingPair[] pairs = new DistanceBuildingPair[size];
		for (int i=0; i<size; i++) {
	
			double x = Double.valueOf(map.get("x").get(i));
			double y = Double.valueOf(map.get("y").get(i));
			Point2d diff = new Point2dImpl(x,y).minus(new Point2dImpl(centerx, centery));
			double distance = PolygonUtil.length(diff);
			
			DistanceBuildingPair newPair = new DistanceBuildingPair();
			newPair.distance = distance;
			newPair.buildingIRI = map.get("bdn").get(i);
			pairs[i] = newPair;
		}
		
		Comparator<DistanceBuildingPair> comparator = new Comparator<DistanceBuildingPair>() {

			@Override
			public int compare(DistanceBuildingPair o1, DistanceBuildingPair o2) {
				if (o1.distance == o2.distance) {
					return 0;
				}
				if (o1.distance < o2.distance) {
					return -1;
				}
				return 1;
			}
		};
		
		Arrays.sort(pairs, comparator);
		
		int min = Math.min(size, buildingLimit);
		for (int i=0; i<min; i++) {
			result.add(pairs[i].buildingIRI);
		}
		
		return result;
	}

	// TODO-AE Shaocong said that this method actually is not needed, remove it
	private String getQueryFilterBdnEnvelope(int buildingLimit, double lowerx, double lowery, double upperx, double uppery) {
		
		String query =
			"PREFIX sys: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>\r\n" + 
			"PREFIX citygml: <http://www.theworldavatar.com/CityGMLOntology.owl#>\r\n" + 
			"PREFIX space_and_time_extended: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n" + 
			"PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\r\n" + 
			"SELECT distinct ?bdn\r\n" +
			"WHERE{\r\n" + 
			"?cityM a citygml:CityModelType.\r\n" + 
			"?cityM citygml:boundedBy ?envelope.\r\n" + 
			"?envelope a citygml:EnvelopeType.\r\n" +  // get all envelopes
			"?envelope citygml:upperCornerPoint ?upoint.\r\n" + //# get bounds of envelope	
			"?upoint space_and_time_extended:hasGISCoordinateSystem ?uco.\r\n" + 
			"?uco space_and_time_extended:hasProjectedCoordinate_x ?uxe.\r\n" + 
			"?uxe sys:hasValue ?uxv.\r\n" + 
			"?uxv sys:numericalValue ?ux.\r\n" + 
			"?uco space_and_time_extended:hasProjectedCoordinate_y ?uye.\r\n" + 
			"?uye sys:hasValue ?uyv.\r\n" + 
			"?uyv sys:numericalValue ?uy.\r\n" + 
			"?envelope citygml:lowerCornerPoint ?lpoint.\r\n" + 
			"?lpoint space_and_time_extended:hasGISCoordinateSystem ?lco.\r\n" + 
			"?lco space_and_time_extended:hasProjectedCoordinate_x ?lxe.\r\n" + 
			"?lxe sys:hasValue ?lxv.\r\n" + 
			"?lxv sys:numericalValue ?lx.\r\n" + 
			"?lco space_and_time_extended:hasProjectedCoordinate_y ?lye.\r\n" + 
			"?lye sys:hasValue ?lyv.\r\n" + 
			"?lyv sys:numericalValue ?ly.\r\n" + 
			"?cityM citygml:cityObjectMember ?bdn.\r\n" + 	// #get bdn belongs to filterd envelope
			"Filter(xsd:double(?lx) > \"%f\"^^xsd:double && xsd:double(?ly) > \"%f\"^^xsd:double && xsd:double(?ux) < \"%f\"^^xsd:double && xsd:double(?uy) < \"%f\"^^xsd:double) \r\n" + //#filter envelope within range		
			"}\r\n" + 
			"LIMIT %d";		//  #limit of building num	
		
		return format(query, lowerx, lowery, upperx, uppery, buildingLimit);
	}

	/**
	 * refers to the Python method admsInputDataRetriever.filterbdns
	 * 
	 * @param buildingLimit
	 * @param lowerx
	 * @param lowery
	 * @param upperx
	 * @param uppery
	 * @return
	 */
	private String getQueryBuildingsFromRegion(int buildingLimit, double lowerx, double lowery, double upperx, double uppery) {
		
		// TODO-AE MID: the query return each building having any point of any groundsurface polygons in the given region
		// Therefore, it doesn't seem to be very different to SPARQL query from getQueryBdnVerticesWithAndWithoutBuildingParts
		// but then it doesn't make sense to query once again for each selected building
		// Ask Kevin: I thought the original idea was to use the calculated weighted and stored center coordinates from the entire building as criterion?
		// agreed with Kevin: we leave it as it is at the moment; but in the long run it should be changed
		
		String query = PREFIX_ONTOCAPE_SYS + PREFIX_ONTOCAPE_SPACE_AND_TIME_EXTENDED + PREFIX_CITYGML + PREFIX_XSD + 
			"SELECT  distinct ?bdn\r\n" + 
			"WHERE {\r\n" + 
			"{\r\n" +                                            				// case1:building has no parts
			"?bdn a citygml:BuildingType.\r\n" + 
			"?bdn citygml:boundedBy ?g.\r\n" +									// building boundBy surface
			"?g a citygml:GroundSurfaceType.\r\n" + 							// surface is a ground 
			"?g citygml:lod2MultiSurface ?ms.\r\n" + 							// ground has lod2multisurface ms
			"?ms citygml:surfaceMember ?pol.\r\n" + 							// ms has member polygon
			"?pol citygml:exterior ?lring.\r\n" + 								// polygon exterior is linear ring
			"?lring sys:contains ?po.\r\n" + 									// linear ring consists of points
			"?po space_and_time_extended:hasGISCoordinateSystem ?co.\r\n" + 	// point has coordinate system cs     
			"?co space_and_time_extended:hasProjectedCoordinate_x ?xe.\r\n" + 	// extract cs to get x,y,z value 
			"?xe sys:hasValue ?xv.\r\n" + 
			"?xv sys:numericalValue ?x.\r\n" + 
			"?co space_and_time_extended:hasProjectedCoordinate_y ?ye.\r\n" + 
			"?ye sys:hasValue ?yv.\r\n" + 
			"?yv sys:numericalValue ?y.\r\n" + 
			"} UNION {\r\n" + 													// case 2: 
			"?bdn a citygml:BuildingType.\r\n" + 								// bdns that consists of part 
			"?bdn citygml:consistsOfBuildingPart ?part.\r\n" + 
			"?part a citygml:BuildingPartType.\r\n" + 
			"?part citygml:boundedBy ?g.\r\n" + 
			"?g a citygml:GroundSurfaceType.\r\n" + 
			"?g citygml:lod2MultiSurface ?ms.\r\n" + 
			"?ms citygml:surfaceMember ?pol.\r\n" + 
			"?pol citygml:exterior ?lring.\r\n" + 
			"?lring sys:contains ?po.\r\n" + 
			"?po space_and_time_extended:hasGISCoordinateSystem ?co.\r\n" + 
			"?co space_and_time_extended:hasProjectedCoordinate_x ?xe.\r\n" + 
			"?xe sys:hasValue ?xv.\r\n" + 
			"?xv sys:numericalValue ?x.\r\n" + 
			"?co space_and_time_extended:hasProjectedCoordinate_y ?ye.\r\n" + 
			"?ye sys:hasValue ?yv.\r\n" + 
			"?yv sys:numericalValue ?y.\r\n" + 
			"}\r\n" + 		
			"Filter(xsd:double(?x) > \"%f\"^^xsd:double && xsd:double(?y) > \"%f\"^^xsd:double && xsd:double(?x) < \"%f\"^^xsd:double && xsd:double(?y) < \"%f\"^^xsd:double) \r\n" + 	
			"}\r\n" + 
			"LIMIT %d";	

		return format(query, lowerx, lowery, upperx, uppery, buildingLimit);
	}
	
	public String getQueryClosestBuildingsFromRegion(int buildingLimit, double lowerx, double lowery, double upperx, double uppery) {
		
		// TODO-AE URGENT do we need distinct here. does it only effect buildings or also x and y
		String query = PREFIX_ONTOCAPE_SYS + PREFIX_ONTOCAPE_SPACE_AND_TIME_EXTENDED + PREFIX_CITYGML + PREFIX_XSD +
			"SELECT distinct ?bdn ?x ?y\n" + 
			"WHERE {\n" + 
			"?bdn a citygml:BuildingType .\n" + 
			"?bdn space_and_time_extended:hasGISCoordinateSystem ?coordinates .\n" + 
			CITYGML_HASCOORDINATES_XY +
			"Filter(xsd:double(?x) > \"%f\"^^xsd:double && xsd:double(?y) > \"%f\"^^xsd:double && xsd:double(?x) < \"%f\"^^xsd:double && xsd:double(?y) < \"%f\"^^xsd:double) \n" + 	
			"}\n" + 
			"LIMIT %d";

		return format(query, lowerx, lowery, upperx, uppery, buildingLimit);
	}
	
	public String getQueryBdnVerticesWithAndWithoutBuildingParts(String buildingIRI) {
		
		String navigationToCoordinates = 
				"?groundsurface a citygml:GroundSurfaceType.\n" + 
				"?groundsurface citygml:lod2MultiSurface ?ms.\n" + 
				"?ms citygml:surfaceMember ?pol.\n" + 
				"?pol citygml:exterior ?lring.\n" +
				"?lring sys:contains ?po.\n" +
				"?po space_and_time_extended:hasGISCoordinateSystem ?coordinates.\n";
		
		String query = PREFIX_ONTOCAPE_SYS + PREFIX_ONTOCAPE_SPACE_AND_TIME_EXTENDED + PREFIX_CITYGML +
				"SELECT ?groundsurface ?x ?y ?z ?coordinates \n" + 
				"WHERE {{\n" +
				"<%s> citygml:boundedBy ?groundsurface.\n" +
				navigationToCoordinates +
				CITYGML_HASCOORDINATES_XYZ +
				"} UNION {\n" +
				"<%s> citygml:consistsOfBuildingPart ?part.\n" +
				"?part citygml:boundedBy ?groundsurface.\n" +
				navigationToCoordinates +
				CITYGML_HASCOORDINATES_XYZ +
				"}}" +
				"ORDER BY ?groundsurface ?coordinates";
		
		return format(query, buildingIRI, buildingIRI);
	}
	
	public String getQueryBdnHeight(String buildingIRI) {
		
		String query = PREFIX_ONTOCAPE_SYS + PREFIX_ONTOCAPE_SPACE_AND_TIME_EXTENDED + PREFIX_CITYGML +
				"SELECT ?h\n" + 
				"WHERE {\n" + 
				"<%s> citygml:measuredHeight ?he .\n" +
				"?he sys:hasValue ?hv .\n" + 
				"?hv sys:numericalValue ?h .\n" + 
				"}\n";
		
		return format(query, buildingIRI);
	}
	
	public SimpleBuildingData performQuerySimpleBuildingData(String cityIRI, List<String> buildingIRIs) {
		
		SimpleBuildingData result = new SimpleBuildingData();
		
		for (String currentIRI : buildingIRIs) {
					
			String query = getQueryBdnVerticesWithAndWithoutBuildingParts(currentIRI);
			String queryResult = performQuery(cityIRI, query);
			Map<String, List<String>> map = MatrixConverter.fromCsv(queryResult);
			
			String sourceCRSName = getCRSName(cityIRI);
			if (!DEFAULT_CRS_NAME.equals(sourceCRSName)) {
				logger.info("transforming coordinate from " + sourceCRSName + " to " + DEFAULT_CRS_NAME + " ...");
				map = transformCoordinates(sourceCRSName, DEFAULT_CRS_NAME, map);
			}
					
			List<Polygon> polygons = SimpleShapeConverter.convertTo2DPolygons(map, "groundsurface", "x", "y");
			// TODO-AE URGENT switch between only one algorithm and both algorithms
			//SimpleShape shape = SimpleShapeConverter.simplifyShapes(polygons);
			Object[] shapes = SimpleShapeConverter.simplifyShapesWithTwoAlgorithms(polygons);
			SimpleShape shape = (SimpleShape) shapes[0]; // the result of the algorithm with convex hull;
			if (shapes[2] != null) {
				double intersectionAreaAlg1 = (Double) shapes[1];
				double intersectionAreaAlg2 = (Double) shapes[3];
				if (intersectionAreaAlg2 > intersectionAreaAlg1) {
					shape = (SimpleShape) shapes[2]; 	// the result of the algorithm with box along lines
				}
			}
					
			// TODO-AE URGENT transformation to CRS coordinates
			result.BldIRI.add(currentIRI);
			String name = currentIRI.split("#")[1];
			if (name.length() > 19) {
				name = name.substring(name.length()-19);
			}		
			result.BldName.add(name);
			result.BldType.add(shape.shapeType);
			result.BldX.add(shape.centerX);
			result.BldY.add(shape.centerY);
			result.BldLength.add(shape.length);
			result.BldWidth.add(shape.width);
			result.BldAngle.add(shape.angle);
			
			String queryHeigt = getQueryBdnHeight(currentIRI);
			String queryHeightResult = performQuery(cityIRI, queryHeigt);
			map = MatrixConverter.fromCsv(queryHeightResult);
			double height = Double.valueOf((String) map.get("h").get(0));
			result.BldHeight.add(height);
		}
		
		return result;
	}
	
	public Map<String, List<String>> transformCoordinates(String sourceCRSName, String targetCRSName, Map<String, List<String>> coordinateMap) {

		List<String> xColumn = coordinateMap.get("x");
		List<String> yColumn = coordinateMap.get("y");
		
		for (int i=0; i<xColumn.size(); i++) {
			
			float x = Float.valueOf(xColumn.get(i));
			float y = Float.valueOf(yColumn.get(i));
			double[] p = CRSTransformer.transform(sourceCRSName, targetCRSName, new double[] {x, y});
			xColumn.set(i, "" + p[0]);
			yColumn.set(i, "" + p[1]);
		}
		
		return coordinateMap;
	}
	
	public static String format(String s, Object... args) {
		return String.format(Locale.ENGLISH, s, args);
	}
}
