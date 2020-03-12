package uk.ac.cam.cares.jps.wte;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.http.client.methods.HttpPost;
import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Resource;
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
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@WebServlet(urlPatterns= {"/WastetoEnergyAgent/startsimulation","/WastetoEnergyAgent/processresult"})

public class WastetoEnergyAgent extends JPSHttpServlet {
	
    @Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(WastetoEnergyAgent.class);
    }
    /**
     *  create logger to log changes attached to WasteToEnergyAgent class. 
     */
    protected Logger logger = LoggerFactory.getLogger(WastetoEnergyAgent.class);
	/** derive property that defines numerical values as described in the ontology
	 * 
	 * @param jenaOwlModel
	 * @return
	 */
	private DatatypeProperty getNumericalValueProperty(OntModel jenaOwlModel) {
		return jenaOwlModel.getDatatypeProperty(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
	}
	/** derive property that defines subsystem relationships as described in the ontology
	 * 
	 * @param jenaOwlModel (OntModel)
	 * @return
	 */
	private ObjectProperty getHasSubsystemRelation(OntModel jenaOwlModel) {
		return jenaOwlModel.getObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem");
	}
	
	private static final long serialVersionUID = 1L;
	public static final String KEY_WATCH = "watch";
	public static final String KEY_CALLBACK_URL = "callback";
	// first called to begin simulation. 
	public static final String SIM_START_PATH = "/WastetoEnergyAgent/startsimulation";
	//called to produce this result directly after start simulation is called. Waits for the first simulation to finish. 
	public static final String SIM_PROCESS_PATH = "/WastetoEnergyAgent/processresult";
	/**gets the food court name, xy coordinates, amount of waste, the year and the type (FoodCourt)
	 */
	public static String FCQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
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
			+ "ORDER BY ASC(?year)";
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
			+ "}";
	/** gets the transportation route, the tax on the route, the capacity of travel, the cost of travel, and
	 * emission rate of travelling on that route. 
	 */
	public String transportQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.w3.org/2006/time#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
			+ "SELECT ?entity ?TransportTaxvalue ?TransportCapacityvalue ?TransportCostvalue ?TransportEmissionvalue " 
			+ "WHERE {"
			+ "?entity  a j8:TransportationRoute ."
			+ "?entity   j8:suitFor ?truck ." 
			+ "?truck   j1:hasTax ?PTT ." 
			+ "?PTT     j2:hasValue ?vPTT ."
			+ "?vPTT  j2:numericalValue ?TransportTaxvalue ."
			
			+ "?truck   j8:hasTransportationCapacity ?TC ." 
			+ "?TC     j2:hasValue ?vTC ."
			+ "?vTC  j2:numericalValue ?TransportCapacityvalue ."

			+ "?truck   j8:hasTransportationCost ?TCost ." 
			+ "?TCost     j2:hasValue ?vTCost ."
			+ "?vTCost  j2:numericalValue ?TransportCostvalue ." 
			
			+ "?truck   j8:hasEmission ?Temission ." 
			+ "?Temission     j2:hasValue ?vTemission ."
			+ "?vTemission  j2:numericalValue ?TransportEmissionvalue ." 
			
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
			+ "SELECT ?entity ?BLandCostvalue ?BOpexvalue ?BManCostvalue ?BInstallationCostvalue ?BLifeCyclevalue ?BEnergyconsumptionvalue ?BWaterConsumptionvalue ?DiscountRatevalue ?LifeCyclevalue ?DisposalFeevalue " 
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

			+ "}";
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
				+ "SELECT DISTINCT ?pollutiontreatmenttaxvalue ?Tech1Capvalue ?installationcostvalue ?operationcostvalue ?transferrateelectricvalue ?energyconsumptionvalue "
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

				+ "?Tech1   j1:hasTransferRate ?TR3 ."
				+ "?TR3     j1:obtainedFrom <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#electricity> ."
				+ "?TR3     j2:hasValue ?vTR3 ." + "?vTR3  j2:numericalValue ?transferrateelectricvalue ."

				+ "?Tech1   j1:requiredConsumption ?RC2 ."
				+ "?RC2     j1:inContextOf <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#energy> ."
				+ "?RC2     j2:hasValue ?vRC2 ." + "?vRC2  j2:numericalValue ?energyconsumptionvalue ."

				+ "}"
				+ "ORDER BY DESC(?Tech1)";
	 /** describes the entities that use a technology, sorted by their technology. 
	  * 
	  */
		public static String Offsiteoutput = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
				+ "SELECT ?entity ?Tech1 " 
				+ "WHERE {"
				+ "?entity   j1:useTechnology ?Tech1 ."  
				+ "}"
				+ "ORDER BY DESC(?Tech1)";
		 @Override
			protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
				 String path = request.getServletPath();
				 String baseUrl= QueryBroker.getLocalDataPath();
				 String wasteIRI=requestParams.getString("wastenetwork");
					OntModel model= readModelGreedy(wasteIRI);
					List<String[]> inputonsitedata=prepareCSVFC(FCQuery,"Site_xy.csv","Waste.csv", baseUrl,model); 
					prepareCSVWT(WTquery,"Location.csv", baseUrl,model); 
					List<String[]> inputoffsitedata=prepareCSVCompTECHBased(compquery,baseUrl,model);
					prepareCSVTECHBased(WTFTechQuery,baseUrl,model);
					copyTemplate(baseUrl, "SphereDist.m");
					copyTemplate(baseUrl, "Main.m");
					copyTemplate(baseUrl, "D2R.m");
				 if (SIM_START_PATH.equals(path)) {

						try {
							createBat(baseUrl);
							runModel(baseUrl);
				            notifyWatcher(requestParams, baseUrl+"/number of units (onsite).csv",
				                    request.getRequestURL().toString().replace(SIM_START_PATH, SIM_PROCESS_PATH));
						} catch (Exception e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					 
				 }else if (SIM_PROCESS_PATH.equals(path)) {
					 try {
						
							//=========================================update in onsite wtf================================================== //create new owl file		

										List<String> onsiteiricomplete=updateinOnsiteWT(inputonsitedata,baseUrl);
										
							//=======================================update in food court======================================================			

										List<String> onsiteiriselected=updateinFC(baseUrl,onsiteiricomplete,inputoffsitedata,inputonsitedata);
							//===================================update in waste system========================================================
										updateKBForSystem(wasteIRI, baseUrl, wasteSystemOutputQuery,onsiteiriselected); //for waste system
//										String revenue=economic.get(0)[0]; //ok
//										String totalinflow=economic.get(1)[0]; //similar to revenue
//										String capex=economic.get(2)[0]; //ok
//										String opex=economic.get(3)[0];//ok
//										String manpower=economic.get(4)[0]; //ok
//										String landcost=economic.get(5)[0];//ok
//										String pollution=economic.get(6)[0];
//										String transport=economic.get(7)[0];//ok
//										String resource=economic.get(8)[0];//ok
//										String totaloutflow=economic.get(9)[0]; //total output excluding capital cost
//										String netflow=economic.get(10)[0]; 
							//=============================================================================================	
										

										updateinOffsiteWT(inputoffsitedata,baseUrl);
										return requestParams;
					 }catch (Exception e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
										

					 
				 }


				
				
				return requestParams;
			}
	/** updates the Onsite Waste Treatment Facility OWL file
	 * 
	 * @param inputdata {[List<String[]>]}
	 * @param baseUrl String
	 * @return List<String>
	 * @throws Exception
	 */
	public List<String> updateinOnsiteWT(List<String[]> inputdata,String baseUrl) throws Exception { //creating needed onsite WTF while returning complete set of onsite iri

		List<String[]>unitofonsite=readResult(baseUrl,"number of units (onsite).csv");
		List<String[]>onsiteunitmapping=new ArrayList<String[]>();
		int size3=unitofonsite.size();
		int colamount3=unitofonsite.get(0).length;
		for(int x=0;x<size3;x++) {
			String[]linemapping= new String[colamount3];
			for(int y=0;y<colamount3;y++) { //1tech only	
				BigDecimal bd = new BigDecimal(unitofonsite.get(x)[y]);
				double newval= Double.parseDouble(bd.toPlainString());
				linemapping[y]=bd.toPlainString();
				if(newval<0) {
					linemapping[y]="0";
				}
				
				
			}
			onsiteunitmapping.add(linemapping);	
		}
		WTEKBCreator converter = new WTEKBCreator();
		converter.startConversion("onsitewtf",inputdata,onsiteunitmapping);
		List<String>mappedonsiteiri=converter.onsiteiri;
		return mappedonsiteiri;
	}
	/** Creates the CSV file for the upper bound as described by compquery. 
	 * 
	 * @param mainquery compquery. 
	 * @param baseUrl String
	 * @param model Ontological Model
	 * @return List
	 */
	public List<String[]> prepareCSVCompTECHBased(String mainquery,String baseUrl,OntModel model) {		
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
        return resultTechOffsiteWTF;
	}
	/** updates the OWL file for the Offsite Waste Treatment facilities. 
	 * 
	 * @param inputdata List<String[]>
	 * @param baseUrl String
	 * @throws Exception
	 */
	public void updateinOffsiteWT(List<String[]> inputdata,String baseUrl) throws Exception {
		//assume inputdata= input offsite data
		List<String[]>unitofoffsite=readResult(baseUrl,"number of units (offsite).csv");
		System.out.println("it goes to the offsite update");
		//filter the arrayfirst to take only non zero values
		List<String[]>filtered=new ArrayList<String[]>();
		for(int r=0;r<unitofoffsite.size();r++) {
			for(int i=0;i<unitofoffsite.get(0).length;i++) {
				String element=unitofoffsite.get(r)[i];
				if(Integer.valueOf(element)>0) {
					String[]component= {""+r,inputdata.get(0)[i],element};
					filtered.add(component);
				}
			}
		}
		
		if(filtered.size()>0) {
			String sparqlStart = "PREFIX OW:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> \r\n" 
					+"PREFIX OCPSYST:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> \r\n"
						+ "INSERT DATA { \r\n";
			for(int w=0;w<filtered.size();w++) {
				StringBuffer b = new StringBuffer();
				String currentunit = filtered.get(w)[1].split("#")[0] + "#UnitDeviceOf-" + filtered.get(w)[1].split("#")[1]+w; //w is precaution if duplicate instance
				int numunit = Integer.valueOf(filtered.get(w)[2]);
				//String currentwtf = inputdataonsite.get(onsiteindex);
				//0=incineration
				//1=codigestion
				//2=anaerobic
				String result = new QueryBroker().queryFile(filtered.get(w)[1], Offsiteoutput);
				String[] keyswt = JenaResultSetFormatter.getKeys(result);
				List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keyswt);
				String techiri=resultList.get(Integer.valueOf(filtered.get(w)[0]))[1];
				b.append("<" + techiri + "> OW:realizedByDevice <" + currentunit + "> . \r\n");
				b.append("<" + currentunit + "> a OW:WasteTreatmentDevice . \r\n");
				b.append("<" + currentunit + "> OW:usedInYear " + 1 + " . \r\n");
				b.append("<" + currentunit + "> OW:amountOfUnit " + numunit + " . \r\n");
				String sparql = sparqlStart + b.toString() + "} \r\n";
				new QueryBroker().updateFile(filtered.get(w)[1], sparql);
			}
		}
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
	/** reads the result from the csv file produced and returns as List<String[]>
	 * 
	 * @param baseUrl String
	 * @param filename name of the file. 
	 * @return
	 * @throws IOException
	 */
	private List<String[]> readResult(String baseUrl,String filename) throws IOException {

        String outputFile = baseUrl + "/"+filename;
        String csv = new QueryBroker().readFileLocal(outputFile);
        List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
		
		return simulationResult;
	}
	/** updates the Knowledge Base of the composite Waste System based on the Economic output CSV
	 *  
	 * @param iriofnetwork String
	 * @param baseUrl String
	 * @param queryupdate String
	 * @param onsiteiri List of onsite entities. 
	 * @throws IOException
	 */
	public void updateKBForSystem(String iriofnetwork, String baseUrl, String queryupdate,List<String> onsiteiri) throws IOException {
		List<String[]>economic=readResult(baseUrl,"Economic output.csv");
		String result = new QueryBroker().queryFile(iriofnetwork, wasteSystemOutputQuery);
		String[] keyswt = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keyswt);
		logger.info("answer number= " + resultList.size());
		System.out.println("answer number= " + resultList.size());
		OntModel model = JenaHelper.createModel();
		model.read(iriofnetwork, null);
		for (int ind = 1; ind < keyswt.length; ind++) {
			Individual inst = model.getIndividual(resultList.get(0)[ind]);
			if (ind == 1) {
				inst.setPropertyValue(getNumericalValueProperty(model),
						model.createTypedLiteral(new Double(economic.get(ind - 1)[0])));
			} else {
				inst.setPropertyValue(getNumericalValueProperty(model),
						model.createTypedLiteral(new Double(economic.get(ind)[0])));
			}
		}
		
		Individual entity = model.getIndividual(resultList.get(0)[0]);
		for(int wtfamount=0;wtfamount<onsiteiri.size();wtfamount++) {
			Resource entityonsite = model.createResource(onsiteiri.get(wtfamount));
			entity.addProperty(getHasSubsystemRelation(model), entityonsite);
		}
		
		
		String content = JenaHelper.writeToString(model);
		new QueryBroker().putOld(resultList.get(0)[0], content);

	}
	/** updates the Foodcourt owl file based on the value of the treated waste (onsite) CSV and treated waste (offsite) csv
	 * This updates the waste delivered to either facility
	 * @param baseUrl
	 * @param inputdataonsite
	 * @param inputdataoffsite
	 * @param foodcourtmap
	 * @return
	 * @throws Exception
	 */
	public List<String> updateinFC(String baseUrl,List<String> inputdataonsite,List<String[]> inputdataoffsite,List<String[]> foodcourtmap) throws Exception { //update the fc and giving selected onsite iri list
		List<String>selectedOnsite=new ArrayList<String>();
		//both of them have row= fc amount, col represents onsite or offsite per tech
		List<String[]>treatedwasteon=readResult(baseUrl,"Treated waste (onsite).csv");
		List<String[]>onsitemapping=new ArrayList<String[]>();
		int size=treatedwasteon.size();
		for(int x=0;x<size;x++) {
			for(int y=0;y<size;y++) {
				String wastetransfer=treatedwasteon.get(x)[y]; //in ton/day
				if(Double.parseDouble(wastetransfer)>0.01) {
					String[]linemapping= {""+x,""+y,wastetransfer};
					onsitemapping.add(linemapping);
				}
			}
		}
		
		List<String[]>treatedwasteoff=readResult(baseUrl,"Treated waste (offsite).csv");
		List<String[]>offsitemapping=new ArrayList<String[]>();
		int size2=treatedwasteoff.size();
		int colamount2=treatedwasteoff.get(0).length;
		for(int x=0;x<size2;x++) {
			for(int y=0;y<colamount2;y++) { //3tech*3instance
				String wastetransfer=treatedwasteoff.get(x)[y]; //in ton/day
				if(Double.parseDouble(wastetransfer)>0.01) {
					String[]linemapping= {""+x,""+y,wastetransfer};
					offsitemapping.add(linemapping);
				}
			}
		}
		
	
		
		String sparqlStart = "PREFIX OW:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> \r\n" 
		+"PREFIX OCPSYST:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> \r\n"
			+ "INSERT DATA { \r\n";
		
		//outputdata= treated waste onsite
		//input data onsite=onsiteiri
		for (int d = 0; d < inputdataonsite.size(); d++) {// each iri of foodcourt
			int wasteindex = 1;

			StringBuffer b = new StringBuffer();
			if (onsitemapping.size() > 0) {
				String currentwaste = foodcourtmap.get(d)[0].split("#")[0] + "#WasteDeliveredAmount-" + wasteindex;
				String valuecurrentwaste = foodcourtmap.get(d)[0].split("#")[0] + "#V_WasteDeliveredAmount-"
						+ wasteindex;
				Double numfromres = Double.parseDouble(onsitemapping.get(d)[2]);
				int onsiteindex = Integer.valueOf(onsitemapping.get(d)[1]);
				String currentwtf = inputdataonsite.get(onsiteindex);
				b.append("<" + foodcourtmap.get(d)[0] + "> OW:deliverWaste <" + currentwaste + "> . \r\n");
				b.append("<" + currentwaste + "> a OW:WasteTransfer . \r\n");
				b.append("<" + currentwaste + "> OCPSYST:hasValue <" + valuecurrentwaste + "> . \r\n");
				b.append("<" + valuecurrentwaste + "> a OCPSYST:ScalarValue . \r\n");
				b.append("<" + valuecurrentwaste + "> OCPSYST:numericalValue " + numfromres + " . \r\n");
				b.append("<" + valuecurrentwaste
						+ "> OCPSYST:hasUnitOfMeasure <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ton_per_day> . \r\n");
				b.append("<" + currentwaste + "> OW:isDeliveredTo <" + currentwtf + "> . \r\n");
				wasteindex++;
				selectedOnsite.add(currentwtf);
			}

			if (offsitemapping.size() > 0) {
				String currentwaste = foodcourtmap.get(d)[0].split("#")[0] + "#WasteDeliveredAmount-" + wasteindex;
				String valuecurrentwaste = foodcourtmap.get(d)[0].split("#")[0] + "#V_WasteDeliveredAmount-"
						+ wasteindex;
				Double numfromres = Double.parseDouble(offsitemapping.get(d)[2]);
				int offsiteindex = Integer.valueOf(offsitemapping.get(d)[1]);
				int IndexOffsiteHeader = offsiteindex % 3; // index 0,3,6 is the first wtf, 1,4,7 is the 2nd, 2,5,8 is
															// the 3rd
				String currentoffwtf = inputdataoffsite.get(0)[IndexOffsiteHeader];
				b.append("<" + foodcourtmap.get(d)[0] + "> OW:deliverWaste <" + currentwaste + "> . \r\n");
				b.append("<" + currentwaste + "> a OW:WasteTransfer . \r\n");
				b.append("<" + currentwaste + "> OCPSYST:hasValue <" + valuecurrentwaste + "> . \r\n");
				b.append("<" + valuecurrentwaste + "> a OCPSYST:ScalarValue . \r\n");
				b.append("<" + valuecurrentwaste + "> OCPSYST:numericalValue <" + numfromres + "> . \r\n");
				b.append("<" + valuecurrentwaste
						+ "> OCPSYST:hasUnitOfMeasure <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ton_per_day> . \r\n");
				b.append("<" + currentwaste + "> OW:isDeliveredTo <" + currentoffwtf + "> . \r\n");
						wasteindex++;
			}

			String sparql = sparqlStart + b.toString() + "} \r\n";
			new QueryBroker().updateFile(foodcourtmap.get(d)[0], sparql);

		}
		
		return selectedOnsite;
	}
	 /** Used for tests to create a folder for simulation, run simulation, before updating the relevant files. 
	  * @param wasteIRI IRI of topnode of Wastenetwork. 
	  */
	 public void runTestInSequence(String wasteIRI) { //only for testing direct call packet
			OntModel model= readModelGreedy(wasteIRI);
			String baseUrl= QueryBroker.getLocalDataPath();
			List<String[]>onsitereference=prepareCSVFC(FCQuery,"Site_xy.csv","Waste.csv", baseUrl,model); 
			prepareCSVWT(WTquery,"Location.csv", baseUrl,model); 
			List<String[]>offsitereference=prepareCSVCompTECHBased(WastetoEnergyAgent.compquery,baseUrl,model);
			prepareCSVTECHBased(WastetoEnergyAgent.WTFTechQuery,baseUrl,model);
			copyTemplate(baseUrl, "SphereDist.m");
			copyTemplate(baseUrl, "Main.m");
			copyTemplate(baseUrl, "D2R.m");
			try {
				createBat(baseUrl);
				runModel(baseUrl);
				Thread.sleep(80*1000);
				List<String> onsiteiricomplete=updateinOnsiteWT(onsitereference,baseUrl);
				List<String> onsiteiriselected=updateinFC(baseUrl,onsiteiricomplete,offsitereference,onsitereference);
				updateKBForSystem(wasteIRI, baseUrl, wasteSystemOutputQuery,onsiteiriselected); //for waste system
				updateinOffsiteWT(offsitereference,baseUrl);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} 
			
	 }
	/** reads the topnode into an OntModel of all its subsystems. 
	 * @param iriofnetwork
	 * @return
	 */
	public static OntModel readModelGreedy(String iriofnetwork) { //model will get all the offsite wtf, transportation and food court
		String wasteinfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity  a  j2:CompositeSystem  ." 
				+ "?entity   j2:hasSubsystem ?component ." 
				+ "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofnetwork, wasteinfo);
	}
	/** Creates the CSV of the foodcourt for the Matlab code to read. 
	 * gets the food court name, xy coordinates, amount of waste, the year and the type (FoodCourt)
	 * @param mainquery
	 * @param filename
	 * @param filename2
	 * @param baseUrl
	 * @param model
	 * @return
	 */
	public List<String[]> prepareCSVFC(String mainquery,String filename,String filename2, String baseUrl,OntModel model) { //create csv for food court and giving the list of complete food court iri
		//csv input file		
		ResultSet resultSet = JenaHelper.query(model, mainquery);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keysfc = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keysfc);
        List<String[]> resultxy = new ArrayList<String[]>();
        List<String[]> resultfcmapper = new ArrayList<String[]>();
		for (int d = 0; d < resultList.size(); d++) {
			String[] comp = { resultList.get(d)[1], resultList.get(d)[2] };// only extract and y
			String[] mapper = {resultList.get(d)[5],resultList.get(d)[1], resultList.get(d)[2] };// only extract and y
			if (resultList.get(d)[4].contentEquals("1")) {
				resultxy.add(comp);
				resultfcmapper.add(mapper);
			}
		}
        if (filename2 != null) {
			List<String[]> resultwaste = new ArrayList<String[]>();
			int size = resultList.size();
			int yearend = Integer.valueOf(resultList.get(resultList.size() - 1)[4]);
			int amountinst = size / yearend; // assume it's from year 1

			for (int n = 0; n < amountinst; n++) {
				yearend=1;//control how many year we want to use;assume yearend =1 (only 1 year)
				String[] consumption = new String[yearend];
				for (int r = 0; r < yearend; r++) { 
					consumption[r] = resultList.get(r * amountinst + n)[3];
				}
				resultwaste.add(consumption);
			}
			new QueryBroker().putLocal(baseUrl + "/" + filename2, MatrixConverter.fromArraytoCsv(resultwaste));
		}
        String[]header= {keysfc[1],keysfc[2]};
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
	public void prepareCSVTECHBased(String mainquery,String baseUrl,OntModel model) {		
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
        new QueryBroker().putLocal(baseUrl + "/Conversion rate.csv", MatrixConverter.fromArraytoCsv(transferrate)); 	
        new QueryBroker().putLocal(baseUrl + "/Pollution treatment tax.csv", MatrixConverter.fromArraytoCsv(tax)); 
        new QueryBroker().putLocal(baseUrl + "/Resource conversion.csv", MatrixConverter.fromArraytoCsv(consumption)); 
        new QueryBroker().putLocal(baseUrl + "/Unit installation cost (off site).csv", MatrixConverter.fromArraytoCsv(inscost)); 
        new QueryBroker().putLocal(baseUrl + "/Unit operation cost (off site).csv", MatrixConverter.fromArraytoCsv(opcost));
        new QueryBroker().putLocal(baseUrl + "/Unit_Capacity_offsite.csv", MatrixConverter.fromArraytoCsv(capacity)); 
	}
	/** create the batch file in the mentioned folder. 
	 * 
	 * @param baseUrl
	 * @throws Exception
	 */
	public void createBat(String baseUrl) throws Exception {
		String loc = baseUrl + "\\Main.m";
		String bat = "setlocal" + "\n" + "cd /d %~dp0" + "\n" + "matlab -nosplash -noFigureWindows -r \"try; run('"
				+ loc + "'); catch; end; quit\"";
		new QueryBroker().putLocal(baseUrl + "/runm.bat", bat);
	}
	/** copies over the files in the working directory over to scenario folder. 
	 * 
	 * @param newdir
	 * @param filename
	 */
	private void copyTemplate(String newdir, String filename) { //in this case for SphereDist.m; Main.m; D2R.m
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
		
		String destinationUrl = newdir + "/"+filename;
		new QueryBroker().putLocal(destinationUrl, file);
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
	


}
