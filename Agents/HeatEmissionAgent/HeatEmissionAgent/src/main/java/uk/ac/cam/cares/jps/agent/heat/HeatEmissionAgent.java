package uk.ac.cam.cares.jps.agent.heat;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jooq.DSLContext;
import org.jooq.SQLDialect;
import org.jooq.exception.DataAccessException;
import org.jooq.impl.DSL;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import java.sql.*;

/** 
 * ---------------------------------- Heat emission agent ------------------------------------------
 * 
 * This agent is to return the heat emission information of different buildings/objects presented
 * in a given area. Therefore, the heat emission data can be automatically assigned to corresponding
 * buildings within a specific area. This is of interest to the Cooling Singapore 2.0 Project. 
 * 
 * This class file demonstrates (1) the feasibility of a cross-domain query and (2) the evaluation of 
 * the heat emission data in terms of emission values and respective coordinates within a bounding 
 * box in Jurong Island. To achieve this, it consists of four parts. First, we obtain all the chemical 
 * plants, plant items, IRIs and CO2 emission via query in "jibusinessunits"; Second, for a particular
 * chemical plant, its fuel CEI and efficiency are queried; then, all the heat emission coordinates 
 * are evaluated via query in "jriEPSG24500"; finally, the heat emission values are calculated with
 * CO2 emission, CEI and efficiency and assigned to the emission coordinates, after a filter based on
 * a boundary area specified. 
 * 
 * @author Hansong Xue
 *
 *------------------------------------------------------------------------------------------------
 */

/**
 * Servlet implementation class HeatEmissionAgent; URL pattern to execute the
 * agent: <http://
 * www.theworldavatar.com/Agents/HeatEmissionAgent/performheatquery>
 */

@WebServlet(urlPatterns = { HeatEmissionAgent.URL_PATH })

public class HeatEmissionAgent extends JPSAgent {

	public static final String URL_PATH = "/performheatquery";
	// Display messages
	private static final String BAD_INPUT = "Error in input parameters, please check the" +
			" input file";

	private static final String HEAT_COLUMN = "heat_emissions";

	// RDBStore client to query postgis
	private RemoteRDBStoreClient rdbStoreClient;

	private static final Logger LOGGER = LogManager.getLogger(HeatEmissionAgent.class);

	// Receive input as JSON objects, execute the agent and return the results as
	// JSON object as well
	// Pass the method "HeatEmissionQuery" to execute the actual query
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		if (validateInput(requestParams)) {

			// Properties of database containing buildings data.
			String dbUrl = null;
			String dbUser = null;
			String dbPassword = null;

			if (requestParams.has("dbUrl")) {
				dbUrl = requestParams.getString("dbUrl");
				dbUser = requestParams.getString("dbUser");
				dbPassword = requestParams.getString("dbPassword");
			} else {
				String dbName = "postgres";
				EndpointConfig endpointConfig = new EndpointConfig();
				dbUrl = endpointConfig.getDbUrl(dbName);
				dbUser = endpointConfig.getDbUser();
				dbPassword = endpointConfig.getDbPassword();
			}

			rdbStoreClient = new RemoteRDBStoreClient(dbUrl, dbUser, dbPassword);

			updateCityFurnitureEmissions();
			updateFactoryEmissions();
			updatePrintingEmissions();
			updatePrecisionEmissions();

			return new JSONObject().put("success", "true");

		} else {
			LOGGER.error("bad input");
			throw new JPSRuntimeException(BAD_INPUT);
		}
	}

	// Validate the input parameters and check if all the necessary parameters are
	// provided or not
	@Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {
		if (requestParams.isEmpty()) {
			throw new BadRequestException();
		}

		if (!requestParams.has("endpoint") && !requestParams.has("database")) {
			throw new BadRequestException(
					"Either a postgres database name or URL must be specified when running the heat emisson agent.\n");

		}

		return true;
	}

	void updateCityFurnitureEmissions() {
		String tableName = "jurong_island_city_furniture";
		try (Connection conn = rdbStoreClient.getConnection();
				Statement stmt = conn.createStatement();) {

			if (checkTableExists(tableName, conn)) {
				String sqlString = String.format("alter table \"%s\" drop column if exists %s ;" +
						" alter table \"%s\" add column %s double precision; " +
						" update table \"%s\" set %s = co2_emissions*0.5*(1.0^12)/(63.0*365*24*60*60*(1.0^6)) ; ",
						tableName, HEAT_COLUMN, tableName, HEAT_COLUMN, tableName, HEAT_COLUMN);
				stmt.executeUpdate(sqlString);
			}

		} catch (SQLException e) {
			LOGGER.error(e.getMessage());
		}

	}

	void updateFactoryEmissions() {
		String tableName = "factories";
		try (Connection conn = rdbStoreClient.getConnection();
				Statement stmt = conn.createStatement();) {

			if (checkTableExists(tableName, conn)) {
				String sqlString = String.format("alter table \"%s\" drop column if exists %s ;" +
						" alter table \"%s\" add column %s double precision; " +
						" update table \"%s\" set %s = case when specific_energy_consumption > 0.0 then " +
						"production_volume*specific_energy_consumption*(1.0 - thermal_efficiency) " +
						"when specific_energy_consumption < 0.0 then -1.0*production_volume*specific_energy_consumption end ; ",
						tableName, HEAT_COLUMN, tableName, HEAT_COLUMN, tableName, HEAT_COLUMN);
				stmt.executeUpdate(sqlString);
			}

		} catch (SQLException e) {
			LOGGER.error(e.getMessage());
		}

	}

	void updatePrintingEmissions() {
		String tableName = "printing";
		try (Connection conn = rdbStoreClient.getConnection();
				Statement stmt = conn.createStatement();) {

			if (checkTableExists(tableName, conn)) {
				String sqlString = String.format("alter table \"%s\" drop column if exists %s ;" +
						" alter table \"%s\" add column %s double precision; " +
						" update table \"%s\" set %s = 6*heat_emissions_per_printer ;",
						tableName, HEAT_COLUMN, tableName, HEAT_COLUMN, tableName, HEAT_COLUMN);
				stmt.executeUpdate(sqlString);
			}

		} catch (SQLException e) {
			LOGGER.error(e.getMessage());
		}

	}

	void updatePrecisionEmissions() {
		String tableName = "precision_engineering";
		try (Connection conn = rdbStoreClient.getConnection();
				Statement stmt = conn.createStatement();) {

			if (checkTableExists(tableName, conn)) {
				String sqlString = String.format("alter table \"%s\" drop column if exists %s ;" +
						" alter table \"%s\" add column %s double precision; " +
						" update table \"%s\" set %s = floor_area*specific_energy_consumption*(1.0 - thermal_efficiency) ;",
						tableName, HEAT_COLUMN, tableName, HEAT_COLUMN, tableName, HEAT_COLUMN);
				stmt.executeUpdate(sqlString);
			}

		} catch (SQLException e) {
			LOGGER.error(e.getMessage());
		}

	}

	boolean checkTableExists(String table, Connection conn) {
		try {
			String condition = String.format("table_name = '%s'", table);
			return getContext(conn).select(DSL.count()).from("information_schema.tables").where(condition).fetchOne(0,
					int.class) == 1;
		} catch (DataAccessException e) {
			LOGGER.error(e.getMessage());
			throw new RuntimeException(e.getMessage());
		}
	}

	DSLContext getContext(Connection conn) {
		return DSL.using(conn, SQLDialect.POSTGRES);
	}

}
