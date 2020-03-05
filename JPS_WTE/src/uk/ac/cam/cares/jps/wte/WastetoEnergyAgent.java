package uk.ac.cam.cares.jps.wte;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@WebServlet("/WastetoEnergyAgent")
public class WastetoEnergyAgent extends JPSHttpServlet {
	
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	FileOutputStream fos;
	DataOutputStream dos;
	
	public static String FCQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.w3.org/2006/time#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
			+ "SELECT ?name ?xvalue ?yvalue ?wasteproductionvalue ?year " //YEAR IS NOT INCLUDED IF JUST USING SIMPLIFIED VERSION
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
	
	public static String compquery= "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.w3.org/2006/time#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
			+ "SELECT ?tech1upp ?tech2upp ?tech3upp "
			+ "WHERE {" + "?entity  a j1:OffsiteWasteTreatmentFacility ." // specified class declared (off or on)
			
			+ "?entity   j1:hasOffsiteIncinerationUpperBound ?tech1upp ."
			+ "?entity   j1:hasOffsiteCoDigestionUpperBound ?tech2upp ."
			+ "?entity   j1:hasOffsiteAnerobicDigestionUpperBound ?tech3upp ."

			+ "}";
	
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

				+ "}";
	
	public String createWTFQuery(String wtfclass,String techclass) {
		//j1:OffsiteWasteTreatmentFacility =default wtf class
		//j1:OffSiteIncineration or j1:OffSiteCoDigestion or j1:OffSiteAnaerobicDigestion or j1:OnSiteTechnology1 or j1:OnSiteDigester or j1:OnSiteTechnology3=default tech class
		
		 String WTFQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
				+ "SELECT ?entity ?tech1upp ?tech2upp ?tech3upp ?xvalue ?yvalue ?landcostvalue ?watercostvalue ?energycostvalue ?revenueavalue ?revenuebvalue ?revenueelectricvalue ?Tech1Capvalue ?mancostvalue ?operationcostvalue ?pollutiontreatmenttaxvalue ?EOSvalue ?installationcostvalue ?transferrateavalue ?transferratebvalue ?transferrateelectricvalue ?waterconsumptionvalue ?energyconsumptionvalue "
				+ "WHERE {" + "?entity  a "+wtfclass+" ." // specified class declared (off or on)
				
				+ "?entity   j1:hasOffsiteIncinerationUpperBound ?tech1upp ."
				+ "?entity   j1:hasOffsiteCoDigestionUpperBound ?tech2upp ."
				+ "?entity   j1:hasOffsiteAnerobicDigestionUpperBound ?tech3upp ."

				+ "?entity   j7:hasGISCoordinateSystem ?coorsys ." + "?coorsys   j7:hasProjectedCoordinate_x ?x ."
				+ "?x   j2:hasValue ?xval ." + "?xval   j2:numericalValue ?xvalue ."
				+ "?coorsys   j7:hasProjectedCoordinate_y ?y ." + "?y   j2:hasValue ?yval ."
				+ "?yval   j2:numericalValue ?yvalue ."

				+ "?entity   j3:hasCost ?LC ." + "?LC     j2:hasValue ?vLC ." + "?vLC  j2:numericalValue ?landcostvalue ."

				+ "?entity   j3:hasUtilityCost ?UC1 ."
				+ "?UC1     j1:inContextOf <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#water> ."
				+ "?UC1     j2:hasValue ?vUC1 ." + "?vUC1  j2:numericalValue ?watercostvalue ."

				+ "?entity   j3:hasUtilityCost ?UC2 ."
				+ "?UC2     j1:inContextOf <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#energy> ."
				+ "?UC2     j2:hasValue ?vUC2 ." + "?vUC2  j2:numericalValue ?energycostvalue ."

				+ "?entity   j3:hasRevenue ?R1 ."
				+ "?R1     j1:obtainedFrom <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#recoveredwaste_a> ."
				+ "?R1     j2:hasValue ?vR1 ." + "?vR1  j2:numericalValue ?revenueavalue ."

				+ "?entity   j3:hasRevenue ?R2 ."
				+ "?R2     j1:obtainedFrom <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#recoveredwaste_b> ."
				+ "?R2     j2:hasValue ?vR2 ." + "?vR2  j2:numericalValue ?revenuebvalue ."

				+ "?entity   j3:hasRevenue ?R3 ."
				+ "?R3     j1:obtainedFrom <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#electricity> ."
				+ "?R3     j2:hasValue ?vR3 ." + "?vR3  j2:numericalValue ?revenueelectricvalue ."

				+ "?entity   j1:useTechnology ?Tech1 ." + "?Tech1 a "+techclass+" ." // specified class declared (tech 1,2,3, or 4,5,6)
				+ "?Tech1 j1:hasTechnologyCapacity ?Tech1Cap ." + "?Tech1Cap j2:hasValue ?vTech1Cap ."
				+ "?vTech1Cap  j2:numericalValue ?Tech1Capvalue ."

				+ "?Tech1   j3:hasLaborCost ?MC ." + "?MC     j2:hasValue ?vMC ."
				+ "?vMC  j2:numericalValue ?mancostvalue ."

				+ "?Tech1   j3:hasCost ?OC ." + "?OC     j2:hasValue ?vOC ."
				+ "?vOC  j2:numericalValue ?operationcostvalue ."

				+ "?Tech1   j1:hasTax ?PTT ." + "?PTT     j2:hasValue ?vPTT ."
				+ "?vPTT  j2:numericalValue ?pollutiontreatmenttaxvalue ."

				+ "?Tech1   j2:hasProperty ?EOS ." + "?EOS     j2:hasValue ?vEOS ." + "?vEOS  j2:numericalValue ?EOSvalue ."

				+ "?Tech1   j3:hasInstallationCost ?IC ." + "?IC     j2:hasValue ?vIC ."
				+ "?vIC  j2:numericalValue ?installationcostvalue ."

				+ "?Tech1   j1:hasTransferRate ?TR1 ."
				+ "?TR1     j1:obtainedFrom <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#recoveredwaste_a> ."
				+ "?TR1     j2:hasValue ?vTR1 ." + "?vTR1  j2:numericalValue ?transferrateavalue ."

				+ "?Tech1   j1:hasTransferRate ?TR2 ."
				+ "?TR2     j1:obtainedFrom <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#recoveredwaste_b> ."
				+ "?TR2     j2:hasValue ?vTR2 ." + "?vTR2  j2:numericalValue ?transferratebvalue ."

				+ "?Tech1   j1:hasTransferRate ?TR3 ."
				+ "?TR3     j1:obtainedFrom <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#electricity> ."
				+ "?TR3     j2:hasValue ?vTR3 ." + "?vTR3  j2:numericalValue ?transferrateelectricvalue ."

				+ "?Tech1   j1:requiredConsumption ?RC1 ."
				+ "?RC1     j1:inContextOf <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#water> ."
				+ "?RC1     j2:hasValue ?vRC1 ." + "?vRC1  j2:numericalValue ?waterconsumptionvalue ."

				+ "?Tech1   j1:requiredConsumption ?RC2 ."
				+ "?RC2     j1:inContextOf <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#energy> ."
				+ "?RC2     j2:hasValue ?vRC2 ." + "?vRC2  j2:numericalValue ?energyconsumptionvalue ."

				+ "}";
		
		
		
		return WTFQuery;
	}

	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		JSONObject joforess = AgentCaller.readJsonParameter(request);
		String wasteIRI=joforess.getString("wastenetwork");
		OntModel model= readModelGreedy(wasteIRI);
		String baseUrl= QueryBroker.getLocalDataPath();
		prepareCSVFC(FCQuery,"Site_xy.csv","Waste.csv", baseUrl,model); 
		prepareCSVWT(WTquery,"Location.csv", baseUrl,model); 
	}
	
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
	
	
	public void prepareCSVFC(String mainquery,String filename,String filename2, String baseUrl,OntModel model) {
				
		
		ResultSet resultSet = JenaHelper.query(model, mainquery);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keysfc = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keysfc);
        List<String[]> resultxy = new ArrayList<String[]>();
		for (int d = 0; d < resultList.size(); d++) {
			String[] comp = { resultList.get(d)[1], resultList.get(d)[2] };// only extract and y
			if (resultList.get(d)[4].contentEquals("1")) {
				resultxy.add(comp);
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
    	
	}
	
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
			//?pollutiontreatmenttaxvalue ?Tech1Capvalue ?installationcostvalue ?operationcostvalue ?transferrateelectricvalue ?energyconsumptionvalue "
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
		for(int techonsite=0;techonsite<3;techonsite++) {
			String[]onsite= {"0"};
			tax.add(techonsite,onsite);
			capacity.add(techonsite,onsite);
			inscost.add(techonsite,onsite);
			opcost.add(techonsite,onsite);
			transferrate.add(techonsite,onsite);
			consumption.add(techonsite,onsite);
		}
        //String[]header= {keyswt[1],keyswt[2]};
        //resultxy.add(0,header);
        new QueryBroker().putLocal(baseUrl + "/Conversion rate.csv", MatrixConverter.fromArraytoCsv(transferrate)); 	
        new QueryBroker().putLocal(baseUrl + "/Pollution treatment tax.csv", MatrixConverter.fromArraytoCsv(tax)); 
        new QueryBroker().putLocal(baseUrl + "/Resource conversion.csv", MatrixConverter.fromArraytoCsv(consumption)); 
        new QueryBroker().putLocal(baseUrl + "/Unit installation cost (off site).csv", MatrixConverter.fromArraytoCsv(inscost)); 
        new QueryBroker().putLocal(baseUrl + "/Unit operation cost (off site).csv", MatrixConverter.fromArraytoCsv(opcost));
        new QueryBroker().putLocal(baseUrl + "/Unit_Capacity_offsite.csv", MatrixConverter.fromArraytoCsv(capacity)); 
        //new QueryBroker().putLocal(baseUrl + "/Unit_Capacity_onsite.csv", MatrixConverter.fromArraytoCsv(resultxy)); //not needed at the moment
	}
	
	public void createBat(String baseUrl) throws Exception
	{
		String loc=baseUrl+"\\Main.m";	
	String bat="setlocal"+"\n"+"cd /d %~dp0"+"\n"+"matlab -nosplash -noFigureWindows -r \"try; run('"+loc+"'); catch; end; quit\"";
	new QueryBroker().putLocal(baseUrl + "/runm.bat", bat);
	}
	
	public void copyTemplate(String newdir, String filename) { //in this case for SphereDist.m; Main.m; D2R.m
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
		
		String destinationUrl = newdir + "/"+filename;
		new QueryBroker().putLocal(destinationUrl, file);
	}
	
	public String executeSingleCommand(String targetFolder , String command) 
	{  
	 
		logger.info("In folder: " + targetFolder + " Excuted: " + command);
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
	
	public void runModel(String baseUrl) throws IOException {
		//String result = CommandHelper.executeCommands(baseUrl, args);
		String startbatCommand =baseUrl+"/runm.bat";
		String result= executeSingleCommand(baseUrl,startbatCommand);
		logger.info("final after calling: "+result);
	}
	
	public void prepareCSVCompTECHBased(String mainquery,String baseUrl,OntModel model) {		
		ResultSet resultSet = JenaHelper.query(model, mainquery);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keyswt = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keyswt);
        List<String[]> resultxy = new ArrayList<String[]>();
        int technumber=6;
		for (int d = 0; d < technumber; d++) {
			String[]comp=new String[resultList.size()];
			if (d >= 3) { //before 3 is for the onsite(all=0)
				for (int dd = 0; dd < resultList.size(); dd++) {
					comp[dd] = resultList.get(dd)[d-3];
				}
			}
			else {
				comp[0]="0";
				comp[1]="0";
				comp[2]="0";
			}
			resultxy.add(comp);
		}
		
        new QueryBroker().putLocal(baseUrl + "/n_unit_max_offsite.csv",MatrixConverter.fromArraytoCsv(resultxy));
	}

}
