package uk.ac.cam.cares.jps.misc.performance;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.query.SparqlOverHttpService;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

public class TestBuildings extends TestCase {

	public static final String SPARQL_PREFIXES_NEW = "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" 
			+ "PREFIX sys: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\n"
			+ "PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>\n"
			+ "PREFIX citygml:<http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#>\n";
	
	public static final String SPARQL_PREFIXES = "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" 
			+ "PREFIX sys: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>\n"
			+ "PREFIX space_and_time_extended: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#>\n"
			+ "PREFIX citygml:<http://www.theworldavatar.com/CityGMLOntology.owl#>\n";
	
	public static final String CITYGML_HASCOORDINATES_XY = 		
			"?coordinates space_and_time_extended:hasProjectedCoordinate_x ?xe .\n" + 
			"?xe sys:hasValue ?xv .\n" + 
			"?xv sys:numericalValue ?x .\n" + 
			"?coordinates space_and_time_extended:hasProjectedCoordinate_y ?ye .\n" + 
			"?ye sys:hasValue ?yv .\n" + 
			"?yv sys:numericalValue ?y .\n";	
	
	private String datasetUrl = null;

	public void printHelp() {
		System.out.println("\nTestBuildings <SPARQL query number> <url for dataset> <id-start>");
		System.out.println("1 = SelectBuildings");
		System.out.println("2 = SelectPredicateObjectForConcreteBuildingTwoLevels");
		System.out.println("3 = SelectCoordinates");
		System.out.println("4 = SelectCoordinatesOfAllBuildings");
		System.out.println("5 = SelectClosestBuildingsForOneBuilding");
		System.out.println("example url for fuseki      = http://localhost:3030/<dataset name>");
		System.out.println("example url for rdf4j       = http://localhost:8080/rdf4j-server/repositories/<dataset name>");
		System.out.println("example url for blazegraph  = http://localhost:9999/blazegraph/namespace/<dataset name>/sparql");
		System.out.println("example id-start = GUID_75F6C (optional parameter, default is empty string)");
	}
	
	public void start(String[] args) throws IOException {
		
		if (args.length == 0) {
			printHelp();
			return;
		}
		
		int queryNumber = Integer.valueOf(args[0]);
		datasetUrl = args[1];
		String id = "";
		if (args.length > 1) {
			id = args[2];
		}
		
		long start = System.currentTimeMillis();
		
		switch(queryNumber) {
		case 1: 
			test1SelectBuildings(id);
			break;
		case 2:
			test2SelectPredicateObjectTwoLevels(id);
			break;
		case 3:
			test3SelectHeight(id);
			break;
		case 4: 
			test4SelectCoordinatesOfAllBuildings(id);
			break;
		case 5:
			test5SelectClosestBuildingsForOneBuilding(id);
			break;
		default:
			System.out.println("unknown SPARQL query number");
		}
		
		long stop = System.currentTimeMillis();

		System.out.println("elapsed time in milli = " + (stop - start));
		
		System.out.println("\n\nfinished for arguments:");
		
		for (String current : args) {
			System.out.println(current);
		}
		
	}

	public void testStart() throws IOException {
		//String[] args = new String[] {"5", "http://localhost:3030/buildingsthehague", "GUID_2F4006F6"};
		String[] args = new String[] {"5", "http://localhost:9999/blazegraph/namespace/namedbuildings/sparql", "GUID_2F4"};
		start(args);
	}
	
	private SparqlOverHttpService createSparqlOverHttpService() {
		return new SparqlOverHttpService(datasetUrl);
	}
	
	public String  performQuery(String query, Object... args) {
		query = MiscUtil.format(query, args);
		System.out.println(query);
		// The result variable is initialised with null to use later in the
		// code if an exception occurs. 
		String result = null;
		try {
			result = createSparqlOverHttpService().executeGet(query);
		} catch (SQLException e) {
			e.printStackTrace();
		}
		//System.out.println(result);
		// The result remains null if an SQL exception is thrown.
		// Hence, null is returned to the calling method.
		if(result == null){
			return null;
		}
		
		StringTokenizer t = new StringTokenizer(result, "\r\n");
		int maxNumber = 10;
		int i=-1; // don't count the column header line
		while (t.hasMoreTokens()) {
			i++;
			String nextLine = t.nextToken();
			if (i<=maxNumber) {
				System.out.println(i+": " + nextLine);
			}

		}
		
		System.out.println("...number of rows = " + i);

		return result;
	}
	
	public void test1SelectBuildings(String id) {		
		String query = SPARQL_PREFIXES 
				+ "SELECT ?bdn WHERE { "
				+ "?bdn a citygml:BuildingType . ?bdn citygml:id ?id . FILTER(STRSTARTS(?id, \"%s\")). "
				+ "}";
		
		performQuery(query, id);
	}
	
	public String test2SelectPredicateObjectTwoLevels(String id) {
		String query = SPARQL_PREFIXES 
				+ "SELECT ?bdn ?pred ?obj ?pred2 ?obj2 WHERE { "
				+ "?bdn a citygml:BuildingType . ?bdn citygml:id ?id . FILTER(STRSTARTS(?id, \"%s\")). "
				+ "?bdn ?pred ?obj . ?obj ?pred2 ?obj2 . } LIMIT 20000";
		
		return performQuery(query, id);
	}
	
	public void test3SelectHeight(String id) {
		String query = SPARQL_PREFIXES 
				+ "SELECT ?bdn ?height WHERE { "
				+ "?bdn a citygml:BuildingType . ?bdn citygml:id ?id . FILTER(STRSTARTS(?id, \"%s\")). "
				+ "?bdn citygml:measuredHeight ?he .\n"
				+ "?he sys:hasValue ?hv .\n" 
				+ "?hv sys:numericalValue ?height .\n" 
				+ "}\n";
		performQuery(query, id); // height for id=GUID_75F6C3 is 9.547
	}
	
	public String test4SelectCoordinatesOfAllBuildings(String id) {
		String query = SPARQL_PREFIXES  
			+ "SELECT distinct ?bdn ?x ?y\n"  
			+ "WHERE {\n" 
			+ "?bdn a citygml:BuildingType . ?bdn citygml:id ?id . FILTER(STRSTARTS(?id, \"%s\")). "
			+ "?bdn space_and_time_extended:hasGISCoordinateSystem ?coordinates .\n"  
			+ CITYGML_HASCOORDINATES_XY 
			+ "}\n"  
			+ "LIMIT %d";

		int buildingLimit = 20000;
		String result = performQuery(query, id, buildingLimit); 
		printCoordinatesOfBoundingBox(result);
		
		return result;
	}
	
	public void printCoordinatesOfBoundingBox(String result) { 
		
		Map<String, List<String>> map = MatrixConverter.fromCsv(result);
		
		double xmin = -1;
		double xmax = -1;
		for (String current : map.get("x")) {
			double d = Double.valueOf(current);
			if (xmin == -1) {
				xmin = d;
				xmax = d;
			} else {
				xmin = Math.min(xmin, d);
				xmax = Math.max(xmax, d);
			}
		}
		
		double ymin = -1;
		double ymax = -1;
		for (String current : map.get("y")) {
			double d = Double.valueOf(current);
			if (ymin == -1) {
				ymin = d;
				ymax = d;
			} else {
				ymin = Math.min(ymin, d);
				ymax = Math.max(ymax, d);
			}
		}
		
		System.out.println("xmin=" + xmin + ", xmax=" + xmax + ", ymin=" + ymin + ", ymax=" + ymax);
	}
	
	public void test5SelectClosestBuildingsForOneBuilding(String id) {
		
		String result = test4SelectCoordinatesOfAllBuildings(id);
		Map<String, List<String>> map = MatrixConverter.fromCsv(result);
		double centerx = Double.valueOf(map.get("x").get(0));
		double centery = Double.valueOf(map.get("y").get(0));
		
		String query = SPARQL_PREFIXES 
			+ "SELECT distinct ?bdn ?x ?y\n" 
			+ "WHERE {\n" 
			+ "?bdn a citygml:BuildingType . \n"
			+ "?bdn space_and_time_extended:hasGISCoordinateSystem ?coordinates .\n"  
			+ CITYGML_HASCOORDINATES_XY
			+ "Filter(xsd:double(?x) > \"%f\"^^xsd:double && xsd:double(?y) > \"%f\"^^xsd:double && xsd:double(?x) < \"%f\"^^xsd:double && xsd:double(?y) < \"%f\"^^xsd:double) \n"
			+ "}\n" 
			+ "LIMIT %d";
		
		// coordinates of the plant in The Hague
		//double centerx = 79831;
		//double centery = 454766;
		
		// coordates of http://www.theworldavatar.com/Building/01_buildings4.owl#BuildingGUID_75F6C3E1-4D6D-4087-A81B-3E218741173B
		//double centerx = 78698;
		//double centery = 458014;
		
		// xmin=75546, xmax=80447, ymin=452429, ymax=458997
		//double centerx = 76000;
		//double centery = 453000;
		
		double length = 100;
		
		double lowerx = centerx - length;
		double lowery = centery - length;
		double upperx = centerx + length;
		double uppery = centery + length;
		int buildingLimit = 50;

		String closestBuildings = performQuery(query, lowerx, lowery, upperx, uppery, buildingLimit);
		map = MatrixConverter.fromCsv(closestBuildings);
		
		int i=1;
		for (String current : map.get("bdn")) {
			int index = current.indexOf("GUID_");
			String currentid = current.substring(index);
			System.out.println(i + ". query for " + currentid);
			test2SelectPredicateObjectTwoLevels(currentid);
			i++;
		}
	}
}
