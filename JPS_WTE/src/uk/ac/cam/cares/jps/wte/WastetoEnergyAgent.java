package uk.ac.cam.cares.jps.wte;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.http.client.methods.HttpPost;
import org.apache.jena.arq.querybuilder.Order;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@WebServlet(urlPatterns= {"/startsimulation"})

public class WastetoEnergyAgent extends JPSHttpServlet {
	
    @Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(WastetoEnergyAgent.class);
    }
    /**
     *  create logger to log changes attached to WasteToEnergyAgent class. 
     */
    protected Logger logger = LoggerFactory.getLogger(WastetoEnergyAgent.class);
	
	
	private static final long serialVersionUID = 1L;
	public static final String KEY_WATCH = "watch";
	public static final String KEY_CALLBACK_URL = "callback";
	// first called to begin simulation. 
	public static final String SIM_START_PATH = "/startsimulation";
	//called to produce this result directly after start simulation is called. Waits for the first simulation to finish. 
	public static final String SIM_PROCESS_PATH = "/processresult";
	/**gets the food court name, xy coordinates, amount of waste, the year
	 */
	public static String FCQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j6:<http://www.w3.org/2006/time#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
			+ "SELECT ?name ?xvalue ?yvalue ?wasteproductionvalue ?year ?entity " //YEAR IS NOT INCLUDED IF JUST USING SIMPLIFIED VERSION
			+ "WHERE {"
			+ "?entity  a j1:FoodCourt ."
			+ "?entity   j8:hasName ?name ." 
            + "?entity   j7:hasGISCoordinateSystem ?coorsys ."
            + "?coorsys   j7:hasProjectedCoordinate_x ?x ."
            + "?x   j2:hasValue ?xval ."
            + "?xval   j2:numericalValue ?xvalue ."
            + "?coorsys   j7:hasProjectedCoordinate_y ?y ."
            + "?y   j2:hasValue ?yval ."
            + "?yval   j2:numericalValue ?yvalue ."

			+ "?entity   j1:produceWaste ?WP ." 
			+ "?WP     j2:hasValue ?vWP ."
			+ "?vWP  j2:numericalValue ?wasteproductionvalue ."

			+ "?vWP   j6:hasTime ?time ." 
			+ "?time     j6:inDateTime ?vdatetime ."
			+ "?vdatetime  j6:year ?year ." 
			+ "}"
			+ "ORDER BY ASC(?year)";//1,10,2,3...
	
	public static String getFCQuery() {
		SelectBuilder sb = FCQuerySource.getFCQuery();
		sb.addPrefix("j6", "http://www.w3.org/2006/time#").addVar("?entity").addVar("?name")
		.addVar("?wasteproductionvalue").addVar("?year")
		.addWhere("?entity", "j1:produceWaste","?WP").addWhere("?WP", "j2:hasValue","?vWP")
		.addWhere("?vWP", "j2:numericalValue","?wasteproductionvalue").addWhere("?vWP", "j6:hasTime","?time")
		.addWhere("?time", "j6:inDateTime","?vdatetime")
		.addWhere("?vdatetime", "j6:year","?year").addOrderBy("?year");
		Query q = sb.build();
		return q.toString();
	}
	/**gets the Offsite Waste Treatment facility, and its xy coordinates. 
	 */
	public static String WTquery="PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.w3.org/2006/time#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
			+ "SELECT ?entity ?xvalue ?yvalue "
			+ "WHERE {" 
			+ "?entity  a j1:OffsiteWasteTreatmentFacility ."			
			+ "?entity   j7:hasGISCoordinateSystem ?coorsys ." 
			+ "?coorsys   j7:hasProjectedCoordinate_x ?x ."
			+ "?x   j2:hasValue ?xval ." 
			+ "?xval   j2:numericalValue ?xvalue ."
			+ "?coorsys   j7:hasProjectedCoordinate_y ?y ." 
			+ "?y   j2:hasValue ?yval ."
			+ "?yval   j2:numericalValue ?yvalue ."
			+ "}"+ "ORDER BY ASC(?entity)";
	/** gets the transportation route, the tax on the route, the capacity of travel, the cost of travel, and
	 * emission rate of travelling on that route. 
	 */
	//only one set of result is returned
	public String transportQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.w3.org/2006/time#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
			+ "SELECT ?Unit_transport_capacity ?Unit_transport_cost ?pollutionTransportTax ?dieselConsTruck " 
			+ "WHERE {"
			+ "?entity  a j8:TransportationRoute ."
			+ "?entity   j8:suitFor ?truck ." 
			+ "?truck   j1:hasTax ?PTT ." 
			+ "?PTT     j2:hasValue ?vPTT ."
			+ "?vPTT  j2:numericalValue ?pollutionTransportTax ."
			
			+ "?truck   j8:hasTransportationCapacity ?TC ." 
			+ "?TC     j2:hasValue ?vTC ."
			+ "?vTC  j2:numericalValue ?Unit_transport_capacity ."

			+ "?truck   j8:hasTransportationCost ?TCost ." 
			+ "?TCost     j2:hasValue ?vTCost ."
			+ "?vTCost  j2:numericalValue ?Unit_transport_cost ." 
			
			+ "?truck   j8:hasEmission ?Temission ." 
			+ "?Temission     j2:hasValue ?vTemission ."
			+ "?vTemission  j2:numericalValue ?dieselConsTruck ." 
			
			+ "}";
	/** Gets the Benchmark land cost of implementing the Composite system, the operation value, managing cost, 
	 * installation cost, life cycle of the system, energy and water consumption; 
	 * as well as discount rate, lifecycle, and disposal fee. 
	 */
	public String wasteSystemQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.w3.org/2006/time#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
			+ "SELECT DISTINCT ?entity ?BLandCostvalue ?BOpexvalue ?BManCostvalue ?BInstallationCostvalue ?BLifeCyclevalue ?BEnergyconsumptionvalue ?BWaterConsumptionvalue ?DiscountRatevalue ?LifeCyclevalue ?DisposalFeevalue " 
			+ "WHERE {"
			+ "?entity  a j2:CompositeSystem ."
			+ "?entity   j1:hasBenchmark ?B1 ." 
			+ "?B1   a j3:CostsForLand ." 
			+ "?B1     j2:hasValue ?vB1 ."
			+ "?vB1  j2:numericalValue ?BLandCostvalue ."
			
			+ "?entity   j1:hasBenchmark ?B2 ." 
			+ "?B2   a j3:OperationalExpenditureCosts ." 
			+ "?B2     j2:hasValue ?vB2 ."
			+ "?vB2  j2:numericalValue ?BOpexvalue ."

			+ "?entity   j1:hasBenchmark ?B3 ." 
			+ "?B3   a j3:OperatingLaborCosts ." 
			+ "?B3     j2:hasValue ?vB3 ."
			+ "?vB3  j2:numericalValue ?BManCostvalue ."
			
			+ "?entity   j1:hasBenchmark ?B4 ." 
			+ "?B4   a j3:InstallationCostsForSystemsRealization ." 
			+ "?B4     j2:hasValue ?vB4 ."
			+ "?vB4  j2:numericalValue ?BInstallationCostvalue ."
			
			+ "?entity   j1:hasBenchmark ?B5 ." 
			+ "?B5   a j1:LifeCycle ." 
			+ "?B5     j2:hasValue ?vB5 ."
			+ "?vB5  j2:numericalValue ?BLifeCyclevalue ."
			
			+ "?entity   j1:hasBenchmark ?B6 ." 
			+ "?B6   a j1:ResourceConsumption ." 
			+ "?B6     j1:inContextOf <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#energy> ."
			+ "?B6     j2:hasValue ?vB6 ."
			+ "?vB6  j2:numericalValue ?BEnergyconsumptionvalue ."
			
			+ "?entity   j1:hasBenchmark ?B7 ." 
			+ "?B7   a j1:ResourceConsumption ." 
			+ "?B7     j1:inContextOf <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#water> ."
			+ "?B7     j2:hasValue ?vB7 ."
			+ "?vB7  j2:numericalValue ?BWaterConsumptionvalue ."
			
			+ "?entity   j1:hasDiscountRate ?DC ." 
			+ "?DC     j2:hasValue ?vDC ."
			+ "?vDC  j2:numericalValue ?DiscountRatevalue ." 
			
			+ "?entity   j1:hasLifeCycle ?LC ." 
			+ "?LC     j2:hasValue ?vLC ."
			+ "?vLC  j2:numericalValue ?LifeCyclevalue ." 
			
			+ "?entity   j3:hasUtilityCost ?Dispfee ." 
			+ "?Dispfee     j2:hasValue ?vDispfee ."
			+ "?vDispfee  j2:numericalValue ?DisposalFeevalue ." 
			
			+ "}";
	/** Gets the following output values of the composite waste system. 
	 * the name, revenue, installation cost, operational cost, labor cost, land cost, pollution cost, transport cost
	 * resource cost. 
	 */
	public static String wasteSystemOutputQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.w3.org/2006/time#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
			+ "SELECT ?entity ?vrevenue ?vinstallationcost ?voperationalcost ?vlaborcost  ?vlandcost ?vpollutioncost ?vtransportcost ?vresourcecost  " 
			+ "WHERE {"
			+ "?entity  a j2:CompositeSystem ."
			+ "?entity   j3:hasUtilityCost ?UC1 ."
			+ "?UC1  a j3:UtilityCosts ."
			+ "?UC1     j2:hasValue ?vresourcecost ." 
						
			+ "?entity   j3:hasInstallationCost ?IC1 ."
			+ "?IC1     j2:hasValue ?vinstallationcost ."  
			
			+ "?entity   j3:hasRevenue ?Rev1 ."
			+ "?Rev1     j2:hasValue ?vrevenue ." 
			
			+ "?entity   j3:hasCost ?LC1 ."
			+ "?LC1  a j3:CostsForLand ."
			+ "?LC1     j2:hasValue ?vlandcost ."
			
			+ "?entity   j3:hasCost ?OC1 ."
			+ "?OC1  a j3:OperationalExpenditureCosts ."
			+ "?OC1     j2:hasValue ?voperationalcost ."
			
			+ "?entity   j1:hasTax ?PC1 ."
			+ "?PC1     j2:hasValue ?vpollutioncost ."  
			
			+ "?entity   j3:hasLaborCost ?LabC1 ."
			+ "?LabC1     j2:hasValue ?vlaborcost ." 
			
			+ "?entity   j8:hasTransportationCost ?TC1 ."
			+ "?TC1     j2:hasValue ?vtransportcost ." 
			
			+ "}";
	/** gets the OffsiteWasteTreatmentFacility's 
	 * Incineration upper bound, CoDigestion upper bound, and Anerobic Digestion upper bound. 
	 */
	public static String compquery= "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.w3.org/2006/time#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
			+ "SELECT ?entity ?tech1upp ?tech2upp ?tech3upp "
			+ "WHERE {" + "?entity  a j1:OffsiteWasteTreatmentFacility ." // specified class declared (off or on)
			
			+ "?entity   j1:hasOffsiteIncinerationUpperBound ?tech1upp ."
			+ "?entity   j1:hasOffsiteCoDigestionUpperBound ?tech2upp ."
			+ "?entity   j1:hasOffsiteAnerobicDigestionUpperBound ?tech3upp ."

			+ "}"+ "ORDER BY ASC(?entity)";
	/** Grabs the Offsite Waste treatment facility's 
	 * pollution treatment tax value, tech capcity, installation cost, operational cost, 
	 * transfer rate of electricity and consumption value. 
	 * Sorted by technology. 
	 */
	 public static String WTFTechQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.w3.org/2006/time#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
			+ "SELECT DISTINCT ?pollutiontreatmenttaxvalue ?Tech1Capvalue ?installationcostvalue ?operationcostvalue ?transferrateelectricvalue ?energyconsumptionvalue ?laborcostvalue "
			+ "WHERE {" + "?entity  a j1:OffsiteWasteTreatmentFacility ." // specified class declared (off or on)
			
			+ "?entity   j1:useTechnology ?Tech1 ." 
			//+ "?Tech1 a "+techclass+" ." // specified class declared (tech 1,2,3, or 4,5,6)
			+ "?Tech1 j1:hasTechnologyCapacity ?Tech1Cap ." 
			+ "?Tech1Cap j2:hasValue ?vTech1Cap ."
			+ "?vTech1Cap  j2:numericalValue ?Tech1Capvalue ."
			
			+ "?Tech1   j1:hasTax ?PTT ." + "?PTT     j2:hasValue ?vPTT ."
			+ "?vPTT  j2:numericalValue ?pollutiontreatmenttaxvalue ."

			+ "?Tech1   j3:hasCost ?OC ." + "?OC     j2:hasValue ?vOC ."
			+ "?vOC  j2:numericalValue ?operationcostvalue ."

			+ "?Tech1   j3:hasInstallationCost ?IC ." + "?IC     j2:hasValue ?vIC ."
			+ "?vIC  j2:numericalValue ?installationcostvalue ."
			
			+ "?Tech1   j3:hasLaborCost ?LC ."+"?LC     j2:hasValue ?vLC . "
			+ "?vLC  j2:numericalValue ?laborcostvalue ."
			
			+ "?Tech1   j1:hasTransferRate ?TR3 ."
			+ "?TR3     j1:obtainedFrom <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#electricity> ."
			+ "?TR3     j2:hasValue ?vTR3 ." + "?vTR3  j2:numericalValue ?transferrateelectricvalue ."

			+ "?Tech1   j1:requiredConsumption ?RC2 ."
			+ "?RC2     j1:inContextOf <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#energy> ."
			+ "?RC2     j2:hasValue ?vRC2 ." + "?vRC2  j2:numericalValue ?energyconsumptionvalue ."

			+ "}"
			+ "ORDER BY DESC(?Tech1)";
	 /** Extracts the onsite facility's tech capacity, installation cost, operation cost, transferrate electric value, energy consumption
	  * 
	  */
	 public static String WTFTechOnsiteQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.w3.org/2006/time#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
			+ "SELECT DISTINCT ?pollutiontreatmenttaxvalue ?Tech1Capvalue ?installationcostvalue ?operationcostvalue ?transferrateelectricvalue ?energyconsumptionvalue ?laborcostvalue "
			+ "WHERE {" + "?entity  a j1:OnsiteWasteTreatmentFacility ." // specified class declared (off or on)
			
			+ "?entity   j1:useTechnology ?Tech1 ." 
			+ "?Tech1 a <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#OnSiteDigester> ." // specified class declared (tech 1,2,3, or 4,5,6)
			+ "?Tech1 j1:hasTechnologyCapacity ?Tech1Cap ." 
			+ "?Tech1Cap j2:hasValue ?vTech1Cap ."
			+ "?vTech1Cap  j2:numericalValue ?Tech1Capvalue ."
			
			+ "?Tech1   j1:hasTax ?PTT ." + "?PTT     j2:hasValue ?vPTT ."
			+ "?vPTT  j2:numericalValue ?pollutiontreatmenttaxvalue ."

			+ "?Tech1   j3:hasCost ?OC ." + "?OC     j2:hasValue ?vOC ."
			+ "?vOC  j2:numericalValue ?operationcostvalue ."

			+ "?Tech1   j3:hasInstallationCost ?IC ." + "?IC     j2:hasValue ?vIC ."
			+ "?vIC  j2:numericalValue ?installationcostvalue ."
			
			+ "?Tech1   j3:hasLaborCost ?LC ."+"?LC     j2:hasValue ?vLC . "
			+ "?vLC  j2:numericalValue ?laborcostvalue ."
			
			+ "?Tech1   j1:hasTransferRate ?TR3 ."
			+ "?TR3     j1:obtainedFrom <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#electricity> ."
			+ "?TR3     j2:hasValue ?vTR3 ." + "?vTR3  j2:numericalValue ?transferrateelectricvalue ."

			+ "?Tech1   j1:requiredConsumption ?RC2 ."
			+ "?RC2     j1:inContextOf <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#energy> ."
			+ "?RC2     j2:hasValue ?vRC2 ." + "?vRC2  j2:numericalValue ?energyconsumptionvalue ."

			+ "}"
			+ "ORDER BY DESC(?Tech1)";
	
	/** main function. Reads the values in and copies the templates back. 
	 * 
	 */
	@Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		String baseUrl= requestParams.optString("baseUrl", "testFood");
		String wasteIRI=requestParams.optString("wastenetwork", "http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem");
		//render ontological model of waste network
		OntModel model= readModelGreedy(wasteIRI);
		//creates the csv of FCs, with Site_xy reading for location, waste containing the level of waste in years 1-15
		List<String[]> fcMarkers = prepareCSVFC("Site_xy.csv","Waste.csv", baseUrl,model,15); 
		String n_cluster= requestParams.optString("n_cluster", Integer.toString(fcMarkers.size()));
        new QueryBroker().putLocal(baseUrl + "/n_cluster.txt",n_cluster ); 
		prepareCSVWT(WTquery,"Location.csv", baseUrl,model); 
		prepareCSVTransport(transportQuery,"transport.csv", baseUrl,model); 
		prepareCSVCompTECHBased(compquery,baseUrl,model);
		prepareCSVTECHBased(WTFTechQuery,baseUrl,model,"offsite");
		prepareCSVTECHBased(WTFTechOnsiteQuery,baseUrl,model,"onsite");
		copyTemplate(baseUrl, "SphereDist.m");
		copyTemplate(baseUrl, "Main.m");
		copyTemplate(baseUrl, "D2R.m");
		
		try {
			createBat(baseUrl, n_cluster);
            notifyWatcher(requestParams, baseUrl+"/year by year_NPV.txt",
                    request.getRequestURL().toString().replace(SIM_START_PATH, SIM_PROCESS_PATH));
		} catch (Exception e) {
			e.printStackTrace();
		}
		return requestParams;
	}
	 
	
	/** reads the topnode into an OntModel of all its subsystems. 
	 * @param iriofnetwork
	 * @return
	 */
	public static OntModel readModelGreedy(String iriofnetwork) { //model will get all the offsite wtf, transportation and food court
		SelectBuilder sb = new SelectBuilder().addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addWhere("?entity" ,"a", "j2:CompositeSystem").addWhere("?entity" ,"j2:hasSubsystem", "?component");
		String wasteInfo = sb.build().toString();

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofnetwork, wasteInfo);
	}
	/** Creates the CSV of the foodcourt for the Matlab code to read. 
	 * a. reading the model and getting the number of FC *number of years of waste levels 
	 * b. creating the csv file of site locations, and waste levels of those FoodCourts per year
	 * 
	 * @param mainquery
	 * @param filename
	 * @param filename2
	 * @param baseUrl
	 * @param model
	 * @return
	 */
	public List<String[]> prepareCSVFC(String filename,String filename2, String baseUrl,OntModel model, int noOfYears) { //create csv for food court and giving the list of complete food court iri
		//csv input file		
		ResultSet resultSet = JenaHelper.query(model,getFCQuery());
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keysfc = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keysfc);
        List<String[]> resultxy = new ArrayList<String[]>();
        List<String[]> resultfcmapper = new ArrayList<String[]>();
		for (int d = 0; d < resultList.size(); d++) {
			String[] comp = { resultList.get(d)[2], resultList.get(d)[3] };// only extract and y
			String[] mapper = {resultList.get(d)[0],resultList.get(d)[2], resultList.get(d)[3] };// only extract and y
			if (resultList.get(d)[5].contentEquals("1")) { //get the initial year first
				resultxy.add(comp);
				resultfcmapper.add(mapper);
			}
		}
        if (filename2 != null) {//waste.csv
			List<String[]> resultwaste = new ArrayList<String[]>();
			int size = resultList.size();
			int amountinst = size / noOfYears; // assume it's from year 1

			for (int n = 0; n < amountinst; n++) {
				String[] consumption = new String[noOfYears];
				for (int r = 0; r < noOfYears; r++) { 
					consumption[r] = resultList.get(r * amountinst + n)[4];
				}
				resultwaste.add(consumption);
			}
			new QueryBroker().putLocal(baseUrl + "/" + filename2, MatrixConverter.fromArraytoCsv(resultwaste));
		}
        String[]header= {keysfc[2],keysfc[3]};
       // String[]headerwaste= {"waste year1"};
        resultxy.add(0,header);
        //resultwaste.add(0,headerwaste);
        new QueryBroker().putLocal(baseUrl + "/"+filename, MatrixConverter.fromArraytoCsv(resultxy));
    	
        return resultfcmapper;
	}
	/** prepares the CSV file for the Waste treatment facility and it's xy coordinates. 
	 * 
	 * @param mainquery String
	 * @param filename String
	 * @param baseUrl String
	 * @param model
	 */
	public void prepareCSVWT(String mainquery,String filename,String baseUrl,OntModel model) {		
		ResultSet resultSet = JenaHelper.query(model, mainquery);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keyswt = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keyswt);
        List<String[]> resultxy = new ArrayList<String[]>();
		for (int d = 0; d < resultList.size(); d++) {
			String[] comp = { resultList.get(d)[1], resultList.get(d)[2] };// only extract and y
			resultxy.add(comp);
		}
        String[]header= {keyswt[1],keyswt[2]};
        resultxy.add(0,header);
        new QueryBroker().putLocal(baseUrl + "/"+filename, MatrixConverter.fromArraytoCsv(resultxy)); 	
	}
	/** grabs the offsite waste treatment facility's costs, sorted by technology type. 
	 * 
	 * @param mainquery String
	 * @param baseUrl String
	 * @param model OntModel 
	 */
	public void prepareCSVTransport(String mainquery,String filename,String baseUrl,OntModel model) {		
		ResultSet resultSet = JenaHelper.query(model, mainquery);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keyswt = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keyswt);
        List<String[]> resultxy = new ArrayList<String[]>();
		for (int d = 0; d < resultList.size(); d++) {
			String[] comp = resultList.get(d);// only extract and y
			resultxy.add(comp);
		}
        resultxy.add(0,keyswt);
        new QueryBroker().putLocal(baseUrl + "/"+filename, MatrixConverter.fromArraytoCsv(resultxy)); 	
	}
	/** Creates the CSV file for the upper bound as described by compquery. 
	 * 
	 * @param mainquery compquery. 
	 * @param baseUrl String
	 * @param model Ontological Model
	 * @return List
	 */
	public void prepareCSVCompTECHBased(String mainquery,String baseUrl,OntModel model) {		
		ResultSet resultSet = JenaHelper.query(model, mainquery);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keyswt = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keyswt);
        List<String[]> resultTechOffsiteWTF = new ArrayList<String[]>();
        int technumber=3;
        String[] header = new String[resultList.size()];
		for (int d = 0; d < technumber; d++) {
			String[] comp = new String[resultList.size()];
			
			for (int dd = 0; dd < resultList.size(); dd++) {
				comp[dd] = resultList.get(dd)[d+1];
				header[dd]=resultList.get(dd)[0];

			}
			resultTechOffsiteWTF.add(comp);
		}
		resultTechOffsiteWTF.add(0,header);
		
        new QueryBroker().putLocal(baseUrl + "/n_unit_max_offsite.csv",MatrixConverter.fromArraytoCsv(resultTechOffsiteWTF));
        
	}
	/** 
	 * 
	 * @param mainquery: costs of waste treatment facility, either offsite or onsite
	 * @param baseUrl location of folder
	 * @param model of WasteNetwork topnode
	 * @param keyword
	 * @return
	 */
	public List<String[]> prepareCSVTECHBased(String mainquery,String baseUrl,OntModel model,String keyword) {//keyword offsite or onsite		
		ResultSet resultSet = JenaHelper.query(model, mainquery);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keyswt = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keyswt);
        List<String[]> tax = new ArrayList<String[]>();
        List<String[]> capacity = new ArrayList<String[]>();
        List<String[]> inscost = new ArrayList<String[]>();
        List<String[]> opcost = new ArrayList<String[]>();
        List<String[]> transferrate = new ArrayList<String[]>();
        List<String[]> consumption = new ArrayList<String[]>();
		for (int d = 0; d < resultList.size(); d++) {
			//?pollutiontreatmenttaxvalue ?Tech1Capvalue ?installationcostvalue ?operationcostvalue ?transferrateelectricvalue ?energyconsumptionvalue ?tech "
			String[] comp0 = { resultList.get(d)[0]};
			String[] comp1 = { resultList.get(d)[1]};
			String[] comp2 = { resultList.get(d)[2]};
			String[] comp3 = { resultList.get(d)[3]};
			String[] comp4 = { resultList.get(d)[4]};
			String[] comp5 = { resultList.get(d)[5]};
			tax.add(comp0);
			capacity.add(comp1);
			inscost.add(comp2);
			opcost.add(comp3);
			transferrate.add(comp4);
			consumption.add(comp5);	
		}
		String[] header0= {keyswt[0]};
		String[] header1= {keyswt[1]};
		String[] header2= {keyswt[2]};
		String[] header3= {keyswt[3]};
		String[] header4= {keyswt[4]};
		String[] header5= {keyswt[5]};
		tax.add(0,header0);
		capacity.add(0,header1);
		inscost.add(0,header2);
		opcost.add(0,header3);
		transferrate.add(0,header4);
		consumption.add(0,header5);
		
		new QueryBroker().putLocal(baseUrl + "/Conversion rate ("+keyword+").csv", MatrixConverter.fromArraytoCsv(transferrate));
        new QueryBroker().putLocal(baseUrl + "/Pollution treatment tax ("+keyword+").csv", MatrixConverter.fromArraytoCsv(tax)); 
        new QueryBroker().putLocal(baseUrl + "/Resource conversion ("+keyword+").csv", MatrixConverter.fromArraytoCsv(consumption)); 
        new QueryBroker().putLocal(baseUrl + "/Unit installation cost ("+keyword+").csv", MatrixConverter.fromArraytoCsv(inscost)); 
        new QueryBroker().putLocal(baseUrl + "/Unit operation cost ("+keyword+").csv", MatrixConverter.fromArraytoCsv(opcost));
        new QueryBroker().putLocal(baseUrl + "/Unit_capacity_"+keyword+".csv", MatrixConverter.fromArraytoCsv(capacity)); 
	
        return resultList;
	}
	/** copies over the files in the working directory over to scenario folder. 
	 * 
	 * @param newdir
	 * @param filename
	 */
	public void copyTemplate(String newdir, String filename) { //in this case for SphereDist.m; Main.m; D2R.m
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
		
		String destinationUrl = newdir + "/"+filename;
		new QueryBroker().putLocal(destinationUrl, file);
	}/** create the batch file in the mentioned folder. 
	 * 
	 * @param baseUrl
	 * @throws Exception
	 */
	public void createBat(String baseUrl, String n_cluster) throws Exception {
		String loc = baseUrl + "\\Main.m";
		
		String bat =  "matlab -nosplash -noFigureWindows -r \"try; run('"
				+ loc + "'); catch; end; quit\"";
		System.out.println(bat);
        CommandHelper.executeSingleCommand(baseUrl, bat);
	}
	/** runs the batch file. 
	 * 
	 * @param baseUrl
	 * @throws IOException
	 * @throws InterruptedException
	 */
	public void runModel(String baseUrl) throws IOException, InterruptedException {
		String startbatCommand =baseUrl+"/runm.bat";
		String result= executeSingleCommand(baseUrl,startbatCommand);
		logger.info("final after calling: "+result);
	}
	/** executes the process. Called by runModel. 
	 * 
	 * @param targetFolder
	 * @param command
	 * @return
	 * @throws InterruptedException
	 */
	private String executeSingleCommand(String targetFolder , String command) throws InterruptedException 
	{  
	 
		logger.info("In folder: " + targetFolder + " Executed: " + command);
		Runtime rt = Runtime.getRuntime();
		Process pr = null;
		try {
			pr = rt.exec(command, null, new File(targetFolder)); // IMPORTANT: By specifying targetFolder, all the cmds will be executed within such folder.
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
				 
		BufferedReader bfr = new BufferedReader(new InputStreamReader(pr.getInputStream()));
		String line = "";
		String resultString = "";
		try {
			
			while((line = bfr.readLine()) != null) {
				resultString += line;

			}
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		return resultString; 
	}
	/** notifies the watcher to return with the callback. 
	 * 
	 * @param agentArgs JSONObject
	 * @param filePath String
	 * @param callbackIRI String
	 */
    private void notifyWatcher(JSONObject agentArgs, String filePath, String callbackIRI) {
        agentArgs.put(KEY_WATCH, filePath);
        agentArgs.put(KEY_CALLBACK_URL, callbackIRI);
        execute(KeyValueMap.getInstance().get("url.jps_aws"), agentArgs.toString(), HttpPost.METHOD_NAME);
    }
    
	 /** Used for tests to create a folder for simulation, run simulation, before updating the relevant files. 
	  * @param wasteIRI IRI of topnode of Wastenetwork. 
	  */
//	 public void runTestInSequence(String wasteIRI) { //only for testing direct call packet
//			OntModel model= readModelGreedy(wasteIRI);
//			String baseUrl= QueryBroker.getLocalDataPath();
//			List<String[]>onsitereference=prepareCSVFC(FCQuery,"Site_xy.csv","Waste.csv", baseUrl,model); 
//			prepareCSVWT(WTquery,"Location.csv", baseUrl,model); 
//			prepareCSVTransport(transportQuery,"transport.csv", baseUrl,model); 
//			List<String[]>offsitereference=prepareCSVCompTECHBased(WastetoEnergyAgent.compquery,baseUrl,model);
//			prepareCSVTECHBased(WastetoEnergyAgent.WTFTechQuery,baseUrl,model,"offsite");
//			List<String[]>propertydataonsite=prepareCSVTECHBased(WastetoEnergyAgent.WTFTechOnsiteQuery,baseUrl,model,"onsite");
//			copyTemplate(baseUrl, "SphereDist.m");
//			copyTemplate(baseUrl, "Main.m");
//			copyTemplate(baseUrl, "D2R.m");
//			try {
//				createBat(baseUrl);
//				runModel(baseUrl);
//				Thread.sleep(80*1000);
//				List<String> onsiteiricomplete=updateinOnsiteWT(onsitereference,baseUrl,propertydataonsite);
//				List<String> onsiteiriselected=updateinFC(baseUrl,onsiteiricomplete,offsitereference,onsitereference);
//				updateKBForSystem(wasteIRI, baseUrl, wasteSystemOutputQuery,onsiteiriselected); //for waste system
//				updateinOffsiteWT(offsitereference,baseUrl);
//			} catch (Exception e) {
//				// TODO Auto-generated catch block
//				e.printStackTrace();
//			} 
//			
//	 }
	
    public static void main(String[] args) 
    {
    	
    }


}
