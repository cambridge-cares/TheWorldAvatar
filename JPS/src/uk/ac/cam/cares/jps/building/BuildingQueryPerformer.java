package uk.ac.cam.cares.jps.building;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
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

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.building.SimpleShapeConverter.SimpleShape;

public class BuildingQueryPerformer implements SparqlConstants {

	// TODO-AE should be changed to "EPSG:4326"; // WGS 84 which is the standard of GeoJSON, GIS, Google
	public static final String DEFAULT_CRS_NAME = CRSTransformer.EPSG_28992;
	public static final String BERLIN_IRI = "http://dbpedia.org/resource/Berlin";
	public static final String THE_HAGUE_IRI = "http://dbpedia.org/resource/The_Hague"; // The IRIs have be changed to /resource instead of /page
	public static final String SINGAPORE_IRI = "http://dbpedia.org/resource/Singapore";
	public static final String HONG_KONG_IRI = "http://dbpedia.org/resource/Hong_Kong";
	
	private Logger logger = LoggerFactory.getLogger(BuildingQueryPerformer.class);
	
	public String performQuery(String cityIRI, String query) {
		
		String urlKey = IKeys.URL_BUILDINGSQUERY_THEHAGUE;
		if (cityIRI.equalsIgnoreCase(BERLIN_IRI)) {
			urlKey = IKeys.URL_BUILDINGSQUERY_BERLIN;
		} else if (cityIRI.equalsIgnoreCase(SINGAPORE_IRI)) {
			urlKey = IKeys.URL_BUILDINGSQUERY_SINGAPORE;
		} else if (cityIRI.equalsIgnoreCase(HONG_KONG_IRI)) {
			urlKey = IKeys.URL_BUILDINGSQUERY_HONGKONG;
		}
		
		logger.info("urlKey: " + urlKey);
		return AgentCaller.executeGetWithURLKey(urlKey, AgentCaller.MediaType.TEXT_CSV, "query", query);
	}
	
	// TODO-AE: move method to JPS BASE (AgentCaller)
	public String executeGet(URIBuilder builder) {
		try {
			URI uri = builder.build();
			String message = uri.toString();
			int min = Math.min(message.length(), 100);
			logger.info(message.substring(0, min));
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
				"PREFIX sys: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\r\n" + 
				"PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n" + 
				"PREFIX citygml:<http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#>\r\n" + 
				"SELECT ?type\r\n" + 
				"WHERE {\r\n" + 
				"?srs citygml:srsname ?type\r\n" + 
				"}";
		
		if (cityIRI.equalsIgnoreCase(BERLIN_IRI)) {
			return CRSTransformer.EPSG_25833;
		} else if (cityIRI.equalsIgnoreCase(THE_HAGUE_IRI)) {
			return CRSTransformer.EPSG_28992;
		} else if (cityIRI.equalsIgnoreCase(SINGAPORE_IRI) || cityIRI.equalsIgnoreCase(HONG_KONG_IRI)) {
			return CRSTransformer.EPSG_4326;
		}
		
		return DEFAULT_CRS_NAME;
	}
	
	public List<String> performQueryClosestBuildingsFromRegion(String cityIRI, double plantx, double planty, int buildingLimit, double lowerx, double lowery, double upperx, double uppery) {
		//To-DO : later need to connect this to query for the height of plant
		double plh = 20.0; 
		double plx = plantx;
		double ply = planty;
		double lx = lowerx;
		double ux = upperx;
		double ly = lowery;
		double uy = uppery;
		
		String targetCRSName = getCRSName(cityIRI);
		String sourceCRSName = null;
		if (cityIRI.equalsIgnoreCase(BERLIN_IRI) || cityIRI.equalsIgnoreCase(THE_HAGUE_IRI)) {
			sourceCRSName = DEFAULT_CRS_NAME;
		} else if (cityIRI.equalsIgnoreCase(SINGAPORE_IRI) || cityIRI.equalsIgnoreCase(HONG_KONG_IRI)) {
			sourceCRSName = CRSTransformer.EPSG_3857; 
		}
		
		if (lowerx <= 180) {
			sourceCRSName = CRSTransformer.EPSG_4326;
			double[] p = CRSTransformer.transform(sourceCRSName, targetCRSName, new double[] {plantx, planty});
			plx = p[0];
			ply = p[1];
			p = CRSTransformer.transform(sourceCRSName, targetCRSName, new double[] {lowerx, lowery});
			lx = p[0];
			ly = p[1];
			p = CRSTransformer.transform(sourceCRSName, targetCRSName, new double[] {upperx, uppery});
			ux = p[0];
			uy = p[1];
			
		} else {
			if (!DEFAULT_CRS_NAME.equals(targetCRSName)) {
				
				double[] p = CRSTransformer.transform(sourceCRSName, targetCRSName, new double[] {plantx, planty});
				plx = p[0];
				ply = p[1];
				p = CRSTransformer.transform(sourceCRSName, targetCRSName, new double[] {lowerx, lowery});
				lx = p[0];
				ly = p[1];
				p = CRSTransformer.transform(sourceCRSName, targetCRSName, new double[] {upperx, uppery});
				ux = p[0];
				uy = p[1];
				
//				if (targetCRSName.equals(CRSTransformer.EPSG_4326)) {
//					double temp = ly;
//					ly = uy;
//					uy = temp;
//				}
			}
		}

		String query = getQueryClosestBuildingsFromRegion(350, lx, ly, ux, uy);
		//logger.info("BEFORE performQuery");
		String result = performQuery(cityIRI, query);
		//logger.info("AFTER performQuery");
		//system.out.println("=============== query result ===============");
		//system.out.println("With query:\n" + query);
		//system.out.println(result);
		//system.out.println("============================================");
		Map<String, List<String>> map = MatrixConverter.fromCsv(result);
		
		return selectClosestBuilding(plx, ply, buildingLimit, map, plh);
	}
	
	
	
	public List<String> selectClosestBuilding(double centerx, double centery, int buildingLimit, Map<String, List<String>> map, double sourceheight) {
		
		List<String> result = new ArrayList<String>();
		
		class DistanceBuildingPair {
			double distance;
			String buildingIRI;
			double height;

		}
		
		
		
		int size = map.get("bdn").size();
		DistanceBuildingPair[] pairs = new DistanceBuildingPair[size];
		System.out.println("pairsize= "+size);
		for (int i=0; i<size; i++) {
	
			double x = Double.valueOf(map.get("x").get(i));
			double y = Double.valueOf(map.get("y").get(i));
			
			
			Point2d diff = new Point2dImpl(x,y).minus(new Point2dImpl(centerx, centery));
			double distance = PolygonUtil.length(diff);
			
			DistanceBuildingPair newPair = new DistanceBuildingPair();
			newPair.distance = distance;
			newPair.buildingIRI = map.get("bdn").get(i);
			newPair.height=Double.valueOf(map.get("h").get(i));
			pairs[i] = newPair;
			
			
			//score = constant1*(1/distance+1) + height;
		}
		
		

		
		Comparator<DistanceBuildingPair> comparator2 = new Comparator<DistanceBuildingPair>() {

			@Override
			public int compare(DistanceBuildingPair o1, DistanceBuildingPair o2) {
				if (o1.height == o2.height) {
					return 0;
				}
				if (o1.height > o2.height) {
					return -1;
				}
				return 1;
			}
		};
		

		
		Arrays.sort(pairs, comparator2);
		
		DistanceBuildingPair[] newpairs2 = new DistanceBuildingPair[size];

		int sizepair=pairs.length;

		double selectionnumber2=0.25*new Double(sizepair);
		int selectionnumber=(int) selectionnumber2;
		
		if(selectionnumber<25) {
			selectionnumber=sizepair;
		}
		for(int a=0;a<selectionnumber;a++) {
			newpairs2[a]=pairs[a];
			newpairs2[a].distance=pairs[a].distance;
			newpairs2[a].buildingIRI=pairs[a].buildingIRI;
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
		Arrays.sort(pairs, comparator); //test
		
		int min = Math.min(size, buildingLimit);    //buildinglimit=25
//		for (int i=0; i<min; i++) {
//			result.add(pairs[i].buildingIRI);
//		}
		int i=0;
		while (result.size()<min){
			//if(newpairs2[i].height>sourceheight/3) {  //temporarily not considering the height of plant yet
			result.add(newpairs2[i].buildingIRI);
			System.out.println("the height of building picked= "+newpairs2[i].height);
			//}
			i++;
		}
		
		return result;
	}
	
	public String getQueryClosestBuildingsFromRegion(int buildingLimit, double lowerx, double lowery, double upperx, double uppery) {
		
		String query = PREFIX_ONTOCAPE_SYS + PREFIX_ONTOCAPE_SPACE_AND_TIME_EXTENDED + PREFIX_CITYGML + PREFIX_XSD +
			"SELECT distinct ?bdn ?x ?y ?h \n" + 
			"WHERE {\n" + 
			"?bdn a citygml:BuildingType .\n" + 
			"?bdn space_and_time_extended:hasGISCoordinateSystem ?coordinates .\n" + 
			CITYGML_HASCOORDINATES_XY +
			"?bdn a citygml:BuildingType .\n" + 
			"?bdn citygml:measuredHeight ?hgt .\n" +
			"?hgt sys:hasValue ?vhgt .\n" + 
			"?vhgt sys:numericalValue ?h .\n" + 
			
			"Filter(xsd:double(?x) > \"%f\"^^xsd:double && xsd:double(?y) > \"%f\"^^xsd:double && xsd:double(?x) < \"%f\"^^xsd:double && xsd:double(?y) < \"%f\"^^xsd:double) \n" + 	
			"}\n" + 
			"LIMIT %d";

		//add the format for the height
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
		//system.out.println("=========================== buildingIRIs ===========================");
		//system.out.println(buildingIRIs);
		//system.out.println("=============================================================");
		SimpleBuildingData result = new SimpleBuildingData();
		
		for (String currentIRI : buildingIRIs) {
					
			String query = getQueryBdnVerticesWithAndWithoutBuildingParts(currentIRI);
			String queryResult = performQuery(cityIRI, query);
			Map<String, List<String>> map = MatrixConverter.fromCsv(queryResult);
			
			String sourceCRSName = getCRSName(cityIRI);
			logger.info("sourceCRSName: " + sourceCRSName);
			
			if (sourceCRSName.equalsIgnoreCase(CRSTransformer.EPSG_25833)) {
				logger.info("transforming coordinate from " + sourceCRSName + " to " + DEFAULT_CRS_NAME + " ...");
				logger.info("map: " + map.toString());
				map = transformCoordinates(sourceCRSName, DEFAULT_CRS_NAME, map);
			} else if (!sourceCRSName.equalsIgnoreCase(CRSTransformer.EPSG_28992)) {
				logger.info("transforming coordinate from " + sourceCRSName + " to " + CRSTransformer.EPSG_3857 + " ...");
				logger.info("map: " + map.toString());
				map = transformCoordinates(sourceCRSName, CRSTransformer.EPSG_3857, map);
			}
					
			List<Polygon> polygons = SimpleShapeConverter.convertTo2DPolygons(map, "groundsurface", "x", "y");
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
			if (cityIRI.equalsIgnoreCase(SINGAPORE_IRI)) {
				double[] centrePoint = CRSTransformer.transform("EPSG:3857", 
						"EPSG:3414", new double[] {shape.centerX, shape.centerY});
				result.BldX.add(centrePoint[0]);
				result.BldY.add(centrePoint[1]);
				logger.info("coordinate x: " + centrePoint[0]);
				logger.info("coordinate y: " + centrePoint[1]);
			} else if (cityIRI.equalsIgnoreCase(HONG_KONG_IRI)) {
				double[] centrePoint = CRSTransformer.transform("EPSG:3857", 
						"EPSG:2326", new double[] {shape.centerX, shape.centerY});
				result.BldX.add(centrePoint[0]);
				result.BldY.add(centrePoint[1]);
				logger.info("coordinate x: " + centrePoint[0]);
				logger.info("coordinate y: " + centrePoint[1]);
			} else {
				result.BldX.add(shape.centerX);
				result.BldY.add(shape.centerY);
			}
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
