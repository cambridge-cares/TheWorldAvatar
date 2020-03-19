package uk.ac.cam.cares.jps.wte;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@WebServlet("/WastetoEnergyAgent")
public class WastetoEnergyAgent extends JPSHttpServlet {
	
    @Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(WastetoEnergyAgent.class);
    }
    Logger logger = LoggerFactory.getLogger(WastetoEnergyAgent.class);
	
	
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
	
	public String wasteSystemOutputQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
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
			//+ "?UC1  a j3:UtilityCosts ."
			+ "?IC1     j2:hasValue ?vinstallationcost ."  
			
			+ "?entity   j3:hasRevenue ?Rev1 ."
			//+ "?UC1  a j3:UtilityCosts ."
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
			
			+ "?entity   j8:hasTransportationcost ?TC1 ."
			+ "?TC1     j2:hasValue ?vtransportcost ." 
			
			+ "}";
	
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

	public List<String[]> readResult(String baseUrl,String filename) throws IOException {

        String outputFile = baseUrl + "/"+filename;
        String csv = new QueryBroker().readFileLocal(outputFile);
        List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
		
		
		return simulationResult;
		
	}
	
	 @Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		String wasteIRI=requestParams.getString("wastenetwork");
		OntModel model= readModelGreedy(wasteIRI);
		String baseUrl= QueryBroker.getLocalDataPath();
		List<String[]> inputonsitedata=prepareCSVFC(FCQuery,"Site_xy.csv","Waste.csv", baseUrl,model); 
		prepareCSVWT(WTquery,"Location.csv", baseUrl,model); 
		prepareCSVCompTECHBased(WastetoEnergyAgent.compquery,baseUrl,model);
		prepareCSVTECHBased(WastetoEnergyAgent.WTFTechQuery,baseUrl,model);
		copyTemplate(baseUrl, "SphereDist.m");
		copyTemplate(baseUrl, "Main.m");
		copyTemplate(baseUrl, "D2R.m");
		try {
			createBat(baseUrl);
			runModel(baseUrl);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		try {
			List<String[]>economic=readResult(baseUrl,"Economic output.csv");
			updateKBForSystem(model, economic, wasteSystemOutputQuery); //for waste system
//			String revenue=economic.get(0)[0]; //ok
//			String totalinflow=economic.get(1)[0]; //similar to revenue
//			String capex=economic.get(2)[0]; //ok
//			String opex=economic.get(3)[0];//ok
//			String manpower=economic.get(4)[0]; //ok
//			String landcost=economic.get(5)[0];//ok
//			String pollution=economic.get(6)[0];
//			String transport=economic.get(7)[0];//ok
//			String resource=economic.get(8)[0];//ok
//			String totaloutflow=economic.get(9)[0]; //total output excluding capital cost
//			String netflow=economic.get(10)[0]; 
			
			List<String[]>unitofonsite=readResult(baseUrl,"number of units (onsite).csv");
			List<String[]>onsiteunitmapping=new ArrayList<String[]>();
			int size3=unitofonsite.size();
			int colamount3=unitofonsite.get(0).length;
			for(int x=0;x<size3;x++) {
				String[]linemapping= new String[colamount3];
				for(int y=0;y<colamount3;y++) { //1tech only
					linemapping[y]=unitofonsite.get(x)[y]; //representing 1 onsite wtf for each technology									
				}
				onsiteunitmapping.add(linemapping);	
			}
			
			updateinWT(inputonsitedata,onsiteunitmapping);
			
			
			
			
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
			

			List<String[]>unitofoffsite=readResult(baseUrl,"number of units (offsite).csv");
			List<String[]>offsiteunitmapping=new ArrayList<String[]>();
			int size4=unitofoffsite.size();
			int colamount4=unitofoffsite.get(0).length;
			for(int x=0;x<size4;x++) {
				String[]linemapping= new String[colamount4];
				for(int y=0;y<colamount4;y++) { //3 instance only
					linemapping[y]=unitofoffsite.get(x)[y];  //representing 1 technology for each offsite wtf											
				}
				offsiteunitmapping.add(linemapping);
			}
			
			
			
			
			
			
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		return null;
	}
	 
	private DatatypeProperty getNumericalValueProperty(OntModel jenaOwlModel) {
		return jenaOwlModel.getDatatypeProperty(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
	}
	 
	public void updateKBForSystem(OntModel model,List<String[]> outputdata, String queryupdate) {
		ResultSet resultSet = JenaHelper.query(model, queryupdate);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keyswt = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keyswt);
		for (int ind = 1; ind < keyswt.length; ind++) {
			Individual inst = model.getIndividual(resultList.get(0)[ind]);
			if (ind == 1) {
				inst.setPropertyValue(getNumericalValueProperty(model),
						model.createTypedLiteral(new Double(outputdata.get(ind - 1)[0])));
			} else {
				inst.setPropertyValue(getNumericalValueProperty(model),
						model.createTypedLiteral(new Double(outputdata.get(ind)[0])));
			}

		}
		String content = JenaHelper.writeToString(model);
		new QueryBroker().putOld(resultList.get(0)[0], content);

	}
	
	public void updateinWT(List<String[]> inputdata,List<String[]> outputdata) throws Exception {
		//assume outputdata= number of unit in onsite and offiste
		//first focus onsite
		WTEKBCreator converter = new WTEKBCreator();
		converter.startConversion("onsitewtf",inputdata,outputdata);
		//==============================================================
		//left with the offsite
		
	}
	
	public void updateinFC(OntModel model,List<String[]> inputdata,List<String[]> outputdata, String queryupdate) throws Exception {
		//assume outputdata= number of unit in onsite and offiste
		//first focus onsite
		WTEKBCreator converter = new WTEKBCreator();
		List<String>onsiteiri=converter.onsiteiri;
		converter.startConversion("onsitewtf",inputdata,outputdata);
		//==============================================================
		//left with the offsite
		
	}
	 
	 public void runTestInSequence(String wasteIRI) { //only for testing direct call packet
			OntModel model= readModelGreedy(wasteIRI);
			String baseUrl= QueryBroker.getLocalDataPath();
			prepareCSVFC(FCQuery,"Site_xy.csv","Waste.csv", baseUrl,model); 
			prepareCSVWT(WTquery,"Location.csv", baseUrl,model); 
			prepareCSVCompTECHBased(WastetoEnergyAgent.compquery,baseUrl,model);
			prepareCSVTECHBased(WastetoEnergyAgent.WTFTechQuery,baseUrl,model);
			copyTemplate(baseUrl, "SphereDist.m");
			copyTemplate(baseUrl, "Main.m");
			copyTemplate(baseUrl, "D2R.m");
			try {
				createBat(baseUrl);
				runModel(baseUrl);
				List<String[]>economic=readResult(baseUrl,"Economic output.csv");
				updateKBForSystem(model, economic, wasteSystemOutputQuery); //for waste system
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} 
			
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
	
	public List<String[]> prepareCSVFC(String mainquery,String filename,String filename2, String baseUrl,OntModel model) {
				
		
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
	

	public void createBat(String baseUrl) throws Exception
	{
		String loc=baseUrl+"\\Main.m";	
	String bat="setlocal"+"\n"+"cd /d %~dp0"+"\n"+"matlab -nosplash -noFigureWindows -r \"try; run('"+loc+"'); catch; end; quit\"";
	new QueryBroker().putLocal(baseUrl + "/runm.bat", bat);
	}
	
	private void copyTemplate(String newdir, String filename) { //in this case for SphereDist.m; Main.m; D2R.m
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
		
		String destinationUrl = newdir + "/"+filename;
		new QueryBroker().putLocal(destinationUrl, file);
	}
	
	private String executeSingleCommand(String targetFolder , String command) throws InterruptedException 
	{  
	 
		logger.info("In folder: " + targetFolder + " Excuted: " + command);
		Runtime rt = Runtime.getRuntime();
		Process pr = null;
		try {
			pr = rt.exec(command, null, new File(targetFolder)); // IMPORTANT: By specifying targetFolder, all the cmds will be executed within such folder.
			pr.waitFor();
			Thread.sleep(90*1000);
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
	
	public void runModel(String baseUrl) throws IOException, InterruptedException {
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
        int technumber=3;
        String[] header = new String[resultList.size()];
		for (int d = 0; d < technumber; d++) {
			String[] comp = new String[resultList.size()];
			
			for (int dd = 0; dd < resultList.size(); dd++) {
				comp[dd] = resultList.get(dd)[d+1];
				header[dd]=resultList.get(dd)[0];

			}
			resultxy.add(comp);
		}
		resultxy.add(0,header);
		
        new QueryBroker().putLocal(baseUrl + "/n_unit_max_offsite.csv",MatrixConverter.fromArraytoCsv(resultxy));
	}

}
