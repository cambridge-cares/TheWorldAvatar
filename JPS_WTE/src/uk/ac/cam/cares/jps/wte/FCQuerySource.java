package uk.ac.cam.cares.jps.wte;

import java.util.List;

import org.apache.jena.arq.querybuilder.Order;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;
/** Util Class to prepare necessary queries
 * 
 *
 */
public class FCQuerySource {

	/** gets the food court name, xy coordinates
	 */
	public static SelectBuilder getFCQuery() {
		SelectBuilder sb = new SelectBuilder().addPrefix("j1","http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#" )
				.addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addPrefix("j7", "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#")
				.addPrefix("j8", "http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#")
				.addVar("?entity").addVar("?name").addVar("?xvalue").addVar("?yvalue")
				.addWhere("?entity" ,"a", "j1:FoodCourt").addWhere("?entity" ,"j8:hasName", "?name")
				.addWhere("?entity" ,"j7:hasGISCoordinateSystem", "?coorsys")
				.addWhere("?coorsys" ,"j7:hasProjectedCoordinate_x", "?x")
				.addWhere("?x" ,"j2:hasValue", "?xval").addWhere("?xval" ,"j2:numericalValue", "?xvalue")
				.addWhere("?coorsys" ,"j7:hasProjectedCoordinate_y", "?y")
				.addWhere("?y" ,"j2:hasValue", "?yval").addWhere("?yval" ,"j2:numericalValue", "?yvalue");
		return sb;
	}
	
	/** general WasteTreatmentQuery 
	 */
	public static SelectBuilder getWasteTreatmentQuery() {
		SelectBuilder sb = new SelectBuilder().addPrefix("j1","http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#" )
				.addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addPrefix("j7", "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#")
				.addVar("?entity").addVar("?xvalue").addVar("?yvalue")
				.addWhere("?entity" ,"j7:hasGISCoordinateSystem", "?coorsys")
				.addWhere("?coorsys" ,"j7:hasProjectedCoordinate_x", "?x")
				.addWhere("?x" ,"j2:hasValue", "?xval").addOptional("?xval" ,"j2:numericalValue", "?xvalue")
				.addWhere("?coorsys" ,"j7:hasProjectedCoordinate_y", "?y")
				.addWhere("?y" ,"j2:hasValue", "?yval").addOptional("?yval" ,"j2:numericalValue", "?yvalue");
		return sb;
	}
	
	/**gets the OffsiteWasteTreatment entity, xy coordinates
	 */
	public static String getOffsiteWasteTreatmentQuery() {
		SelectBuilder sb =getWasteTreatmentQuery().addWhere("?entity" ,"a", "j1:OffsiteWasteTreatmentFacility");
		return sb.build().toString();
	}
	
	
	/**gets the OffsiteWasteTreatment entity, xy coordinates
	 */
	public static String getOnsiteWasteTreatmentQuery() {
		SelectBuilder sb =getWasteTreatmentQuery().addWhere("?entity" ,"a", "j1:OnsiteWasteTreatmentFacility");
		return sb.build().toString();
	}
	
	
	/** Grabs the Waste treatment facility's 
	 * pollution treatment tax value, tech capcity, installation cost, operational cost, 
	 * transfer rate of electricity and consumption value. 
	 * Sorted by technology. 
	 * Can be assigned offsite or onsite
	 */
	public static SelectBuilder getTechQuery() {
		SelectBuilder sb = new SelectBuilder().addPrefix("j1","http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#" )
				.addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addPrefix("j3", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#")
				.addVar("?pollutiontreatmenttaxvalue").addVar("?Tech1Capvalue").addVar("?installationcostvalue")
				.addVar("?operationcostvalue").addVar("?transferrateelectricvalue").addVar("?energyconsumptionvalue")
				.addVar("?laborcostvalue")
				.addWhere("?entity" ,"j1:useTechnology", "?Tech1")
				.addWhere("?Tech1" ,"j1:hasTax", "?PTT")
				.addWhere("?PTT" ,"j2:hasValue", "?vPTT").addWhere("?vPTT" ,"j2:numericalValue", "?pollutiontreatmenttaxvalue")
				.addWhere("?Tech1" ,"j1:hasTechnologyCapacity", "?Tech1Cap")
				.addWhere("?Tech1Cap" ,"j2:hasValue", "?vTech1Cap").addWhere("?vTech1Cap" ,"j2:numericalValue", "?Tech1Capvalue")
				.addWhere("?Tech1" ,"j3:hasInstallationCost", "?IC")
				.addWhere("?IC" ,"j2:hasValue", "?vIC").addWhere("?vIC" ,"j2:numericalValue", "?installationcostvalue")
				.addWhere("?Tech1" ,"j3:hasCost", "?OC")
				.addWhere("?OC" ,"j2:hasValue", "?vOC").addWhere("?vOC" ,"j2:numericalValue", "?operationcostvalue")				
				.addWhere("?Tech1" ,"j1:hasTransferRate", "?TR3").addWhere("?TR3" ,"j1:obtainedFrom", "j1:electricity")
				.addWhere("?TR3","j2:hasValue", "?vTR3").addWhere("?vTR3" ,"j2:numericalValue", "?transferrateelectricvalue")
				.addWhere("?Tech1" ,"j1:requiredConsumption", "?RC2").addWhere("?RC2" ,"j1:inContextOf", "j1:energy")
				.addWhere("?RC2","j2:hasValue", "?vRC2").addWhere("?vRC2" ,"j2:numericalValue", "?energyconsumptionvalue")
				.addWhere("?Tech1" ,"j3:hasLaborCost", "?LC")
				.addWhere("?LC" ,"j2:hasValue", "?vLC").addWhere("?vLC" ,"j2:numericalValue", "?laborcostvalue")
				.addOrderBy("?Tech1", Order.DESCENDING).setDistinct(true);
		return sb;
	}
	 
	 
	/** feeds a query and gets a result
	 * 
	 * @param model
	 * @param query
	 * @return
	 */
	public static List<String[]> queryResult(OntModel model, String query) {
		
		ResultSet resultSet = JenaHelper.query(model, query);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		return resultListfromquery;
	}
	
	/** Temporary translation fix for switch from KBClient to KGRouter. In the end, it should be querying appropriately from the 
	 * correct Source
	 * 
	 * @param iriofnetwork The file name to be changed. 
	 * @return iriofnetwork (changed)
	 */
	public static String tempIRItoFile(String iriofnetwork) {
		String translatedIRI = iriofnetwork.replace("http://www.theworldavatar.com", AgentLocator.getProperty("absdir.root"));
		translatedIRI = ScenarioHelper.cutHash(translatedIRI);
		iriofnetwork = translatedIRI.replace("C:/", "C:\\");
		return iriofnetwork;
	}
	
}
