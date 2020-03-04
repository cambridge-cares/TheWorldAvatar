package uk.ac.cam.cares.jps.wte;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
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
       // String[]headerwaste= {"waste year1"};
        resultxy.add(0,header);
        //resultwaste.add(0,headerwaste);
        new QueryBroker().putLocal(baseUrl + "/"+filename, MatrixConverter.fromArraytoCsv(resultxy));
    	
	}

}
