package uk.ac.cam.cares.jps.misc.powerplants.performance;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.query.SparqlOverHttpService;
import uk.ac.cam.cares.jps.base.util.MiscUtil;


public class PowerPlantQueries {
	/*
	 * An instance of Logger class is created to log the background knowledge<br>
	 * of a situation when something goes wrong at run-time within this class.
	 */
	private static Logger logger = LoggerFactory.getLogger(PowerPlantQueries.class);
	public static final String SPARQL_PREFIXES = "PREFIX : <http://www.theworldavatar.com/kb/powerplants/>\r\n"
			+ "PREFIX powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>\r\n"
			+ "PREFIX system_v1: <http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#>\r\n"
			+ "PREFIX spacetimeext: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n"
			+ "PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\r\n"
			+ "PREFIX system_realization: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>\r\n"
			+ "PREFIX system_performance: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#>\r\n"
			+ "PREFIX technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>\r\n";
	
	public static final String SPARQL_PLANT = SPARQL_PREFIXES
			+ "SELECT ?emissionvaluenum \r\n" 
			+ "WHERE {\r\n"
			+ "<%s> technical_system:realizes ?generation . ?generation system_performance:hasEmission ?emission . ?emission system:hasValue ?emissionvalue . ?emissionvalue system:numericalValue ?emissionvaluenum .\r\n"
			+ "<%s> system_realization:designCapacity ?capa . ?capa  system:hasValue ?capavalue . ?capavalue system:numericalValue ?capavaluenum .\r\n"
			+ "}";

	public static final String SPARQL_ALL_PLANTS = SPARQL_PREFIXES
			+ "SELECT ?plant \r\n" + "WHERE {\r\n" + "?plant a powerplant:PowerPlant .\r\n" + "}";
	
	public static final String SPARQL_PLANT_UPDATE_EMISSION = SPARQL_PREFIXES 
			+ "DELETE { ?emissionvalue system:numericalValue ?emissionvaluenum .} "
			+ "INSERT { ?emissionvalue system:numericalValue %f .} "
			+ "WHERE { <%s> technical_system:realizes ?generation . ?generation system_performance:hasEmission ?emission . ?emission system:hasValue ?emissionvalue . "
			+ "?emissionvalue system:numericalValue ?emissionvaluenum . }";
	
	SparqlOverHttpService sparqlsService = null;

	public PowerPlantQueries(SparqlOverHttpService sparqlService) {
		this.sparqlsService = sparqlService;
	}
	
	private SparqlOverHttpService getSparqlsService() {
		return sparqlsService;
	}

	/**
	 * Queries all power plants available in a triple store under the given repository.
	 * 
	 * @return the list of power plants 
	 * @throws SQLException
	 */
	public List<String> queryAllPowerplants() throws SQLException{

		List<String> plantList = new ArrayList<String>();
		
		System.out.println(SPARQL_ALL_PLANTS);
		
		String result;
		try {
			result = getSparqlsService().executeGet(SPARQL_ALL_PLANTS);
		} catch (SQLException e) {
			logger.error("PowerPlantQueries: Querying all power plants was not successful due to "+e.getMessage());
			System.out.println("PowerPlantQueries: Querying all power plants failed because of "+e.getMessage());
			throw new SQLException(e.getMessage());
		}
	
		StringTokenizer tokenizer = new StringTokenizer(result, "\r\n");
		tokenizer.nextElement(); // remove the header
		
		
		while (tokenizer.hasMoreElements()) {
			String plant = "" + tokenizer.nextElement();
			System.out.println(plant);
			plantList.add(plant);
		}
		
		System.out.println("number of power plants = " + plantList.size());
		
		return plantList;
	}

	/**
	 * Retrieves the amount of emission from the current plant.
	 * 
	 * @param iri the IRI of the current plant.
	 * @return the amount of emission from the current plant.
	 * @throws SQLException
	 */
	public double queryEmission(String iri) throws SQLException {

		String query = MiscUtil.format(SPARQL_PLANT, iri, iri);

		//System.out.println(query);
		
		String result;
		try {
			result = getSparqlsService().executeGet(query);
		} catch (SQLException e) {
			logger.error("PowerPlantQueries: Querying emission was not successful due to "+e.getMessage());
			System.out.println("PowerPlantQueries: Querying emission failed because of "+e.getMessage());
			throw new SQLException(e.getMessage());
		}
		StringTokenizer tokenizer = new StringTokenizer(result, "\r\n");
		tokenizer.nextToken();
		double emission = 0.;
		if (tokenizer.hasMoreTokens()) {
			emission = Double.valueOf(tokenizer.nextToken());
		}
		
		System.out.println(iri +", emission = " + emission);

		return emission;
	}

	/**
	 * Updates the amount of emission from the current plant. 
	 * 
	 * @param iri the IRI of the current plant.
	 * @param emission the amount of emission from the current plant. 
	 */
	public void updateEmission(String iri, double emission) {
		
		String query = MiscUtil.format(SPARQL_PLANT_UPDATE_EMISSION, emission, iri);
		//System.out.println(query);
		
		try {
			getSparqlsService().executePost(query);
		} catch (SQLException e) {
			logger.error("PowerPlantQueries: Updating emission was not successful due to "+e.getMessage());
			System.out.println("PowerPlantQueries: Updating emission failed because of "+e.getMessage());
		}
	}
	
	/**
	 * Iterates on the list of plant IRIs to show the emission from corresponding plants.
	 * 
	 * @param numberPlants
	 * @param select
	 * @param insert
	 * @param emission
	 * @throws SQLException
	 */
	public void loopOnPlants(int numberPlants, boolean select, boolean insert, double emission) throws SQLException{
		try{
		List<String> plants = queryAllPowerplants();

		long start = System.currentTimeMillis();
		
		double sumEmission = 0;
		int i = 0;
		for (String current : plants) {
			i++;
			System.out.println(i + " " + current);
			if (insert) {
				updateEmission(current, emission);
			}
			if (select) {
				double queriedEmission = queryEmission(current);
				sumEmission += queriedEmission;
			}
			if (i == numberPlants) {
				break;
			}
		}
		
		long stop = System.currentTimeMillis();

		System.out.println("elapsed time in milli = " + (stop - start));
		System.out.println("number of queried or updated plants = " + i);

		if (select) {
			System.out.println("sum of queried emissions (after possible update) = " + sumEmission);
		}
		}catch(SQLException e){
			throw new SQLException(e.getMessage());
		}
	}
}
