package uk.ac.cam.cares.jps.wte;

import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;



public class WTEKBCreator {
	
	String transportiri="";
	List <String> foodcourt= new ArrayList<String>();
	List <String> onsiteiri= new ArrayList<String>();
	List<String> wtf= new ArrayList<String>();
	
	public static String baseURL2 = "C:/TOMCAT/webapps/ROOT/kb/temporary/"; // directory of output file
	public static String ontologymainiri="http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#"; 
	
	private OntClass scalarvalueclass = null;
	private OntClass coordinatesystemclass = null;
	private OntClass coordinateclass = null;
	private OntClass resourceconsumptionclass = null;
	private OntClass coordinatevalueclass = null;
	private OntClass timeinstanceclass = null;

	
	private OntClass transportationmeansclass = null;
	private OntClass transportcostclass = null;
	private OntClass wasteproductionclass = null;
	private OntClass operationcostclass = null;
	private OntClass emissionclass = null;
	private OntClass transportcapacityclass = null;
	private OntClass generaltimeclass = null;
	private OntClass tech1class = null;
	private OntClass tech2class = null;
	private OntClass tech3class = null;
	private OntClass tech4class = null;
	private OntClass tech5class = null;
	private OntClass tech6class = null;
	private OntClass techcapclass = null;
	private OntClass manpowercostclass = null;
	private OntClass treatmenttaxclass = null;
	private OntClass installationcostclass = null;
	private OntClass resourceconsumptioncostclass = null;
	private OntClass EOSclass = null;
	private OntClass compositeclass=null;
	private OntClass WasteTreatmentDeviceclass=null;
	
	private OntClass objectwtfon = null;
	private OntClass mainobjclass1 = null;
	private OntClass objectfoodcourt = null;
	private OntClass objectwtfoff = null;
	private OntClass benchmarkclass = null;
	private OntClass disposalfeeclass = null;
	private OntClass discountrateclass= null;
	private OntClass transporttaxclass = null;
	private OntClass revenueclass= null;
	private OntClass transferrateclass= null;
	private OntClass landcostclass = null;
	private OntClass lifecycleclass = null;
	
	private ObjectProperty suitsfor = null;
	private ObjectProperty hasTransportationcost = null;
	private ObjectProperty hasEmission = null;
	private ObjectProperty produceWaste = null;
	private ObjectProperty requiredConsumption = null;
	private ObjectProperty hastechcap = null;
	private ObjectProperty hasInstallationCost = null;
	private ObjectProperty hasTransportationCapacity = null;
	private ObjectProperty hasCost = null;
	private ObjectProperty hasTax = null;

	private ObjectProperty hasTransferRate = null;
	private ObjectProperty hasRevenue = null;


	private ObjectProperty hasLifeCycle = null;

	
	private ObjectProperty hasDiscountRate = null;
	private ObjectProperty hasUtilityCost = null;
	private ObjectProperty hasSubsystem = null;
	private ObjectProperty hascoordinatesystem = null;
	private ObjectProperty hasx = null;
	private ObjectProperty hasy = null;
	private ObjectProperty hasvalue = null;
	private ObjectProperty hasunit = null;
	private ObjectProperty indatetimedescription = null;
	private ObjectProperty inContextOf = null;
	private ObjectProperty hasLaborCost = null;
	private ObjectProperty hasProperty = null;
	private ObjectProperty hastime = null;
	private ObjectProperty useTechnology = null;
	private ObjectProperty obtainedFrom = null;

	private ObjectProperty realizedByDevice = null;
	private ObjectProperty hasBenchmark = null;

	private DatatypeProperty numval = null;
	private DatatypeProperty year = null;
	private DatatypeProperty name= null;
	
	private DatatypeProperty uppboundincineration = null;
	private DatatypeProperty uppbounddigestion = null;
	private DatatypeProperty uppboundanaerobic = null;
	private DatatypeProperty uppboundtech4 = null;
	private DatatypeProperty uppboundtech5 = null;
	private DatatypeProperty uppboundtech6 = null;
	private DatatypeProperty usedInYear= null;
	private DatatypeProperty amountOfUnit= null;

	static Individual kWh;
	static Individual energy;
	static Individual degree;
	static Individual ton_per_day;
	static Individual water;
	static Individual electricity;
	static Individual recoveredwaste_a;
	static Individual recoveredwaste_b;
	static Individual g;
	static Individual $;
	static Individual kgperh;
	
	static Individual gperkm;
	static Individual $perkm;
	static Individual $perton;
	static Individual ton;
	
	public void initOWLClasses(OntModel jenaOwlModel) {
		mainobjclass1 = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#TransportationRoute");
		objectfoodcourt = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#FoodCourt");
		objectwtfoff = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#OffsiteWasteTreatmentFacility");
		objectwtfon = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#OnsiteWasteTreatmentFacility");
		transportationmeansclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#Truck");
		transportcostclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#TransportationCost");
		wasteproductionclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#WasteProduction");
		emissionclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#Emission");
		transportcapacityclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#TransportingCapacity");
		timeinstanceclass=jenaOwlModel.getOntClass("http://www.w3.org/2006/time#Instant");
		generaltimeclass = jenaOwlModel.getOntClass("http://www.w3.org/2006/time#GeneralDateTimeDescription");
		tech1class = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#OffSiteIncineration");
		tech2class = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#OffSiteCoDigestion");
		tech3class = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#OffSiteAnaerobicDigestion");
		tech4class = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#OnSiteTechnology1");
		tech5class = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#OnSiteDigester");
		tech6class = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#OnSiteTechnology3");
		EOSclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#EconomyOfScaleFactor");
		resourceconsumptionclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#ResourceConsumption");
		installationcostclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#InstallationCostsForSystemsRealization");
		operationcostclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#OperationalExpenditureCosts");
		manpowercostclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#OperatingLaborCosts");
		transporttaxclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#PollutionTransportTax");
		landcostclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#CostsForLand");
		resourceconsumptioncostclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#UtilityCosts");
		
		treatmenttaxclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#PollutionTreatmentTax");			
		scalarvalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");	
		coordinatesystemclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#ProjectedCoordinateSystem");
		benchmarkclass =jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#Benchmark");
		coordinateclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#AngularCoordinate");
		techcapclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#Capacity");
		coordinatevalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue");
		revenueclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#Revenue");	
		transferrateclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#TransferRate");
		lifecycleclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#LifeCycle");
		disposalfeeclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#ServiceFacilityCosts");
		discountrateclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#DiscountRate");
		compositeclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#CompositeSystem");
		WasteTreatmentDeviceclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#WasteTreatmentDevice");
//		matamountclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#MaterialAmount");
//		genamountclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#GeneralizedAmount");
//		wastestreamclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#NonReusableWasteProduct");
//		processclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#PowerConsumption");
//		Pipeclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Pipe");
		
		suitsfor = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#suitFor");
		produceWaste = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#produceWaste");
		requiredConsumption = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#requiredConsumption");
		hasEmission = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#hasEmission");
		hastime = jenaOwlModel.getObjectProperty("http://www.w3.org/2006/time#hasTime");
		hascoordinatesystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
		hasx = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
		hasy = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
		hasTransportationcost = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#hasTransportationCost");
		hasTransportationCapacity = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#hasTransportationCapacity");
		indatetimedescription=jenaOwlModel.getObjectProperty("http://www.w3.org/2006/time#inDateTime");
		hasUtilityCost = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#hasUtilityCost");
		useTechnology = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#useTechnology");
		hastechcap = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#hasTechnologyCapacity");
		hasCost = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#hasCost");
		hasLaborCost = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#hasLaborCost");
		hasTax = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#hasTax");
		hasLifeCycle = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#hasLifeCycle");
		realizedByDevice = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#realizedByDevice");

		hasvalue = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		hasProperty=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");
		hasSubsystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem");
//		hasOutput= jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#hasOutput");
		hasProperty = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");
		inContextOf = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#inContextOf");
		obtainedFrom = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#obtainedFrom");
		hasDiscountRate = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#hasDiscountRate");
		hasBenchmark = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#hasBenchmark");
		hasRevenue=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#hasRevenue");
		hasTransferRate = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#hasTransferRate");
		hasInstallationCost=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#hasInstallationCost");
		
		numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		usedInYear = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#usedInYear");
		amountOfUnit = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#amountOfUnit");
		year=jenaOwlModel.getDatatypeProperty("http://www.w3.org/2006/time#year");
		name = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#hasName");
		uppboundincineration = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#hasOffsiteIncinerationUpperBound");
		uppbounddigestion = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#hasOffsiteCoDigestionUpperBound");
		uppboundanaerobic = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#hasOffsiteAnerobicDigestionUpperBound");
		uppboundtech4 = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#hasOnsiteTech1UpperBound");
		uppboundtech5 = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#hasOnsiteDigesterUpperBound");
		uppboundtech6 = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#hasOnsiteTech3UpperBound");
		
		kWh=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kWh");
		energy=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#energy");
		degree=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree");
		water=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#water");
		$perkm=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#USD_per_km"); 
		$perton=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#USD_per_ton"); 
		ton_per_day=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ton_per_day");
		gperkm=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#gCO2_per_km");
		ton=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#metric_ton");
		g=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#g");
		$=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#USD");
		kgperh=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kg_per_hr");
		//xsdDouble = jenaOwlModel.getRDFDatatypeByName("xsd:double");
		recoveredwaste_a=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#recoveredwaste_a");
		recoveredwaste_b=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#recoveredwaste_b");
		electricity=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#electricity");
		
	}
	
	public String doConversionTransport(OntModel jenaOwlModel,String Prefix, String mainobjectname/*tranport001*/){

		
		Individual mainobjinst = mainobjclass1.createIndividual(Prefix+mainobjectname+".owl#"+mainobjectname);
		//Individual sourcefrom = jenaOwlModel.createIndividual(Prefix+objectfoodcourtname+".owl#"+objectfoodcourtname);
		//Individual sourceto = jenaOwlModel.createIndividual(Prefix+objectwtfname+".owl#"+objectwtfname);
		
		String truckname="UnitTruckin"+mainobjectname;
		Individual truck = transportationmeansclass.createIndividual(Prefix+mainobjectname+".owl#"+truckname);
		mainobjinst.addProperty(suitsfor, truck);
		
		Individual emission = emissionclass.createIndividual(Prefix+mainobjectname+".owl#EmissionOf"+truckname);
		Individual vemission = scalarvalueclass.createIndividual(Prefix+mainobjectname+".owl#V_EmissionOf"+truckname);
		emission.addProperty(hasvalue, vemission);
		vemission.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (0.4)));
		vemission.addProperty(hasunit, gperkm);
		truck.addProperty(hasEmission, emission);
		
		Individual tranportcapacity = transportcapacityclass.createIndividual(Prefix+mainobjectname+".owl#TransportationCapacityOf"+truckname);
		Individual vtransportcapacity = scalarvalueclass.createIndividual(Prefix+mainobjectname+".owl#V_TransportationCapacityOf"+truckname);
		tranportcapacity.addProperty(hasvalue, vtransportcapacity);
		vtransportcapacity.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Integer(6)));
		vtransportcapacity.addProperty(hasunit, ton);
		truck.addProperty(hasTransportationCapacity, tranportcapacity);
		
		Individual transportcost = transportcostclass.createIndividual(Prefix+mainobjectname+".owl#TransportCostOf"+truckname);
		Individual vtransportcost = scalarvalueclass.createIndividual(Prefix+mainobjectname+".owl#V_TransportCostOf"+truckname);
		transportcost.addProperty(hasvalue, vtransportcost);
		vtransportcost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(3)));
		vtransportcost.addProperty(hasunit, $perkm);
		truck.addProperty(hasTransportationcost, transportcost);
		
		Individual polltransporttax = transporttaxclass.createIndividual(Prefix + mainobjectname + ".owl#PollutionTransportTaxOf" + truckname);
		Individual Vpolltransporttax = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_PollutionTransportTaxOf" + truckname);
		polltransporttax.addProperty(hasvalue, Vpolltransporttax);
		Vpolltransporttax.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0.1)));
		 Vpolltransporttax.addProperty(hasunit, $perton);
		truck.addProperty(hasTax, polltransporttax);
		
		return Prefix+mainobjectname+".owl#"+mainobjectname;
		
	}
	
	public String doConversionFC(OntModel jenaOwlModel,String Prefix, String mainobjectname/*foodcourtx*/,String[] data){

		

		//String Prefix="http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/";
		//ObjectProperty[] relationobject= {hastemperature};
		
		Individual mainobjinst = objectfoodcourt.createIndividual(Prefix+mainobjectname+".owl#"+mainobjectname);
		mainobjinst.setPropertyValue(name, jenaOwlModel.createTypedLiteral(data[0]));
		Individual WasteProduction = wasteproductionclass.createIndividual(Prefix+mainobjectname+".owl#WasteProductionOf"+mainobjectname);
		for (int x = 1; x <= 15; x++) {
			Individual vWasteProduction = scalarvalueclass
					.createIndividual(Prefix + mainobjectname + ".owl#V_WasteProductionOf" + mainobjectname+"_"+x);
			
			Individual timestampwaste = timeinstanceclass.createIndividual(Prefix+mainobjectname+".owl#TimestampOfWasteProducedOf"+mainobjectname+"_"+x);
			vWasteProduction.addProperty(hastime, timestampwaste);
			Individual generaldescriptiontimewaste = generaltimeclass.createIndividual(Prefix+mainobjectname+".owl#GeneralDescriptionYearOfWasteProducedOf"+mainobjectname+"_"+x);
			timestampwaste.setPropertyValue(indatetimedescription, generaldescriptiontimewaste); //value need to be changed later
			generaldescriptiontimewaste.setPropertyValue(year, jenaOwlModel.createTypedLiteral(new Integer(""+x))); //value need to be changed later
			
			WasteProduction.addProperty(hasvalue, vWasteProduction);
			vWasteProduction.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(data[2+x])));
			vWasteProduction.addProperty(hasunit, ton);
		}
		mainobjinst.addProperty(produceWaste, WasteProduction);
		
		
		Individual fccoordinate = coordinatesystemclass.createIndividual(Prefix+mainobjectname+".owl#CoordinateSystemOf"+mainobjectname);
		Individual xcoordinate = coordinateclass.createIndividual(Prefix+mainobjectname+".owl#xCoordinateOf"+mainobjectname);
		Individual ycoordinate = coordinateclass.createIndividual(Prefix+mainobjectname+".owl#yCoordinateOf"+mainobjectname);
		Individual xcoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname+".owl#V_xCoordinateOf"+mainobjectname);
		Individual ycoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname+".owl#V_yCoordinateOf"+mainobjectname);
		fccoordinate.addProperty(hasx,xcoordinate);
		fccoordinate.addProperty(hasy,ycoordinate);
		xcoordinate.addProperty(hasvalue,xcoordinatevalue);
		ycoordinate.addProperty(hasvalue,ycoordinatevalue);
		xcoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(data[1]));
		ycoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(data[2]));
		xcoordinatevalue.addProperty(hasunit, degree);
		ycoordinatevalue.addProperty(hasunit, degree);
		mainobjinst.addProperty(hascoordinatesystem, fccoordinate);
		
		return Prefix+mainobjectname+".owl#"+mainobjectname;
	}
	
	public String doConversionWTF(OntModel jenaOwlModel,String Prefix, String mainobjectname){

		Individual mainobjinst = objectwtfoff.createIndividual(Prefix+mainobjectname+".owl#"+mainobjectname);


			Individual tech1 = tech1class
					.createIndividual(Prefix + mainobjectname + ".owl#OffSiteIncinerationOf" + mainobjectname);
			mainobjinst.addProperty(useTechnology, tech1);
			
			Individual tech2 = tech2class
					.createIndividual(Prefix + mainobjectname + ".owl#OffSiteCoDigestionOf" + mainobjectname);
			mainobjinst.addProperty(useTechnology, tech2);

			Individual tech3 = tech3class
					.createIndividual(Prefix + mainobjectname + ".owl#OffSiteAnaerobicDigestionOf" + mainobjectname);
			mainobjinst.addProperty(useTechnology, tech3);
			
			if(mainobjectname.contains("-1")) {
				mainobjinst.setPropertyValue(uppboundincineration, jenaOwlModel.createTypedLiteral(new Integer(45)));
				mainobjinst.setPropertyValue(uppbounddigestion, jenaOwlModel.createTypedLiteral(new Integer(10)));
				mainobjinst.setPropertyValue(uppboundanaerobic, jenaOwlModel.createTypedLiteral(new Integer(10)));
			}else if(mainobjectname.contains("-2")) {
				mainobjinst.setPropertyValue(uppboundincineration, jenaOwlModel.createTypedLiteral(new Integer(26)));
				mainobjinst.setPropertyValue(uppbounddigestion, jenaOwlModel.createTypedLiteral(new Integer(10)));
				mainobjinst.setPropertyValue(uppboundanaerobic, jenaOwlModel.createTypedLiteral(new Integer(10)));
			} else if(mainobjectname.contains("-3")) {
				mainobjinst.setPropertyValue(uppboundincineration, jenaOwlModel.createTypedLiteral(new Integer(4)));
				mainobjinst.setPropertyValue(uppbounddigestion, jenaOwlModel.createTypedLiteral(new Integer(10)));
				mainobjinst.setPropertyValue(uppboundanaerobic, jenaOwlModel.createTypedLiteral(new Integer(10)));
			}else {
				mainobjinst.setPropertyValue(uppboundincineration, jenaOwlModel.createTypedLiteral(new Integer(0)));
				mainobjinst.setPropertyValue(uppbounddigestion, jenaOwlModel.createTypedLiteral(new Integer(0)));
				mainobjinst.setPropertyValue(uppboundanaerobic, jenaOwlModel.createTypedLiteral(new Integer(0)));
			}
			
			for (int tech=1;tech<=3;tech++) {
				Individual eos = EOSclass.createIndividual(Prefix + mainobjectname + ".owl#EOSOfTech"+tech+"Of" + mainobjectname);
				Individual Veos = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_EOSOfTech"+tech+"Of" + mainobjectname);
				eos.addProperty(hasvalue, Veos);
				Veos.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(1)));
				
				Individual techcap = techcapclass.createIndividual(Prefix + mainobjectname + ".owl#TechnologyCapacity"+tech+"Of" + mainobjectname);
				Individual Vtechcap = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_TechnologyCapacity"+tech+"Of" + mainobjectname);
				techcap.addProperty(hasvalue, Vtechcap);
				
				 Vtechcap.addProperty(hasunit, ton_per_day);
				
				Individual installcost = installationcostclass.createIndividual(Prefix + mainobjectname + ".owl#InstallationCost"+tech+"Of" + mainobjectname);
				Individual Vinstallcost = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_InstallationCost"+tech+"Of" + mainobjectname);
				installcost.addProperty(hasvalue, Vinstallcost);
				
				Vinstallcost.addProperty(hasunit, $);
				
				Individual operationcost = operationcostclass.createIndividual(Prefix + mainobjectname + ".owl#OperationalCost"+tech+"Of" + mainobjectname);
				Individual Voperationcost = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_OperationalCost"+tech+"Of" + mainobjectname);
				operationcost.addProperty(hasvalue, Voperationcost);
				
				Voperationcost.addProperty(hasunit, $);
				
				Individual manpowercost = manpowercostclass.createIndividual(Prefix + mainobjectname + ".owl#ManPowerCost"+tech+"Of" + mainobjectname);
				Individual Vmanpowercost = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_ManPowerCost"+tech+"Of" + mainobjectname);
				manpowercost.addProperty(hasvalue, Vmanpowercost);
				Vmanpowercost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
				Vmanpowercost.addProperty(hasunit, $);
				
				Individual polltreatmenttax = treatmenttaxclass.createIndividual(Prefix + mainobjectname + ".owl#PollutionTreatmentTax"+tech+"Of" + mainobjectname);
				Individual Vpolltreatmenttax = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_PollutionTreatmentTax"+tech+"Of" + mainobjectname);
				polltreatmenttax.addProperty(hasvalue, Vpolltreatmenttax);
				
				Vpolltreatmenttax.addProperty(hasunit, $);
				
				if(tech==1) {
					tech1.addProperty(hasProperty, eos);
					tech1.addProperty(hastechcap, techcap);
					tech1.addProperty(hasInstallationCost, installcost);
					tech1.addProperty(hasCost, operationcost);
					tech1.addProperty(hasLaborCost, manpowercost);
					tech1.addProperty(hasTax, polltreatmenttax);
					Vtechcap.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(500)));
					Vinstallcost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(60000)));
					Voperationcost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(100)));
					Vpolltreatmenttax.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(150)));
					if(mainobjectname.contains("-4")) {
						Vtechcap.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(1095000)));
					}
				}
				else if (tech==2) {
					tech2.addProperty(hasProperty, eos);
					tech2.addProperty(hastechcap, techcap);
					tech2.addProperty(hasInstallationCost, installcost);
					tech2.addProperty(hasCost, operationcost);
					tech2.addProperty(hasLaborCost, manpowercost);
					tech2.addProperty(hasTax, polltreatmenttax);
					Vtechcap.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(100)));
					Vinstallcost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(75000)));
					Voperationcost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(30)));
					Vpolltreatmenttax.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
				}
				else if (tech==3) {
					tech3.addProperty(hasProperty, eos);
					tech3.addProperty(hastechcap, techcap);
					tech3.addProperty(hasInstallationCost, installcost);
					tech3.addProperty(hasCost, operationcost);
					tech3.addProperty(hasLaborCost, manpowercost);
					tech3.addProperty(hasTax, polltreatmenttax);
					Vtechcap.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(100)));
					Vinstallcost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(100000)));
					Voperationcost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(30)));
					Vpolltreatmenttax.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
				}
				
				//resource consumption
				for (int resource = 1; resource <= 2; resource++) {
					Individual ResourceConsumption = resourceconsumptionclass.createIndividual(Prefix + mainobjectname
							+ ".owl#ResourceConsumption_Tech" + tech + "_" + resource + "Of" + mainobjectname);

					Individual vResourceConsumption = scalarvalueclass.createIndividual(Prefix + mainobjectname
							+ ".owl#V_ResourceConsumption_Tech" + tech + "_" + resource + "Of" + mainobjectname);
					ResourceConsumption.addProperty(hasvalue, vResourceConsumption);

					if (tech == 1) { // add property revenue to main object also
						tech1.addProperty(requiredConsumption, ResourceConsumption);
						Individual ResourceConsumptionCost = resourceconsumptioncostclass.createIndividual(Prefix
								+ mainobjectname + ".owl#ResourceConsumptionCost" + resource + "Of" + mainobjectname);

						Individual vResourceConsumptionCost = scalarvalueclass.createIndividual(Prefix + mainobjectname
								+ ".owl#V_ResourceConsumptionCost" + resource + "Of" + mainobjectname);
						ResourceConsumptionCost.addProperty(hasvalue, vResourceConsumptionCost);
						mainobjinst.addProperty(hasUtilityCost, ResourceConsumptionCost);
						vResourceConsumptionCost.addProperty(hasunit, $);

						if (resource == 1) {
							ResourceConsumptionCost.addProperty(inContextOf, water);
							vResourceConsumptionCost.setPropertyValue(numval,
									jenaOwlModel.createTypedLiteral(new Double(0)));
							vResourceConsumption.setPropertyValue(numval,
									jenaOwlModel.createTypedLiteral(new Double(0)));
						} else if (resource == 2) {
							ResourceConsumptionCost.addProperty(inContextOf, energy);
							vResourceConsumptionCost.setPropertyValue(numval,
									jenaOwlModel.createTypedLiteral(new Double(0.1)));
							vResourceConsumption.setPropertyValue(numval,
									jenaOwlModel.createTypedLiteral(new Double(70)));
						}

					} else if (tech == 2) {
						tech2.addProperty(requiredConsumption, ResourceConsumption);
						if (resource == 2) {
							vResourceConsumption.setPropertyValue(numval,
									jenaOwlModel.createTypedLiteral(new Double(62)));
						} else {
							vResourceConsumption.setPropertyValue(numval,
									jenaOwlModel.createTypedLiteral(new Double(0)));
						}

					} else if (tech == 3) {
						tech3.addProperty(requiredConsumption, ResourceConsumption);
						if (resource == 2) {
							vResourceConsumption.setPropertyValue(numval,
									jenaOwlModel.createTypedLiteral(new Double(57)));
						} else {
							vResourceConsumption.setPropertyValue(numval,
									jenaOwlModel.createTypedLiteral(new Double(0)));
						}

					}

					if (resource == 1) {
						ResourceConsumption.addProperty(inContextOf, water);
						vResourceConsumption.addProperty(hasunit, ton);

					} else {
						ResourceConsumption.addProperty(inContextOf, energy);
						vResourceConsumption.addProperty(hasunit, kWh);

					}
				}
				
				
				//resource recovery
				for (int recoveredsource = 1; recoveredsource <= 3; recoveredsource++) {
					Individual TransferRate = transferrateclass.createIndividual(Prefix + mainobjectname
							+ ".owl#TransferRate_Tech" + tech + "_" + recoveredsource + "Of" + mainobjectname);

					Individual vTransferRate = scalarvalueclass.createIndividual(Prefix + mainobjectname
							+ ".owl#V_TransferRate_Tech" + tech + "_" + recoveredsource + "Of" + mainobjectname);
					TransferRate.addProperty(hasvalue, vTransferRate);

					if (tech == 1) { // add property REVENUE to main object also
						tech1.addProperty(hasTransferRate, TransferRate);
						Individual Revenue = revenueclass.createIndividual(
								Prefix + mainobjectname + ".owl#Revenue" + recoveredsource + "Of" + mainobjectname);
						mainobjinst.addProperty(hasRevenue, Revenue);

						Individual vRevenue = scalarvalueclass.createIndividual(
								Prefix + mainobjectname + ".owl#V_Revenue" + recoveredsource + "Of" + mainobjectname);
						Revenue.addProperty(hasvalue, vRevenue);
						vRevenue.addProperty(hasunit, $);
						if (recoveredsource == 1) {
							Revenue.addProperty(obtainedFrom, recoveredwaste_a);
							vRevenue.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
							vTransferRate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));

						} else if (recoveredsource == 2) {
							Revenue.addProperty(obtainedFrom, recoveredwaste_b);
							vRevenue.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
							vTransferRate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));

						} else if (recoveredsource == 3) {
							Revenue.addProperty(obtainedFrom, electricity);
							vRevenue.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0.1)));
							vTransferRate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(290)));
						}
					} else if (tech == 2) {
						tech2.addProperty(hasTransferRate, TransferRate);
						if (recoveredsource == 3) {
							vTransferRate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(460)));
						} else {
							vTransferRate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
						}
					} else if (tech == 3) {
						tech3.addProperty(hasTransferRate, TransferRate);
						if (recoveredsource == 3) {
							vTransferRate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(288)));
						} else {
							vTransferRate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
						}
					}
					if (recoveredsource == 1) {
						TransferRate.addProperty(obtainedFrom, recoveredwaste_a);
						vTransferRate.addProperty(hasunit, ton);
					} else if (recoveredsource == 2) {
						TransferRate.addProperty(obtainedFrom, recoveredwaste_b);
						vTransferRate.addProperty(hasunit, ton);
					} else if (recoveredsource == 3) {
						TransferRate.addProperty(obtainedFrom, electricity);
						vTransferRate.addProperty(hasunit, kWh);
					}
				}
			
		}
		
		
		
		//unit land cost
		Individual LandCost = landcostclass.createIndividual(Prefix + mainobjectname + ".owl#LandCostOf" + mainobjectname);
		Individual VLandCost = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_LandCostOf" + mainobjectname);
		LandCost.addProperty(hasvalue, VLandCost);
		VLandCost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(300)));
		VLandCost.addProperty(hasunit, $);
		mainobjinst.addProperty(hasCost, LandCost);
		
		
		
		//x,y
		Individual wtfcoordinate = coordinatesystemclass.createIndividual(Prefix+mainobjectname+".owl#CoordinateSystemOf"+mainobjectname);
		Individual xcoordinate = coordinateclass.createIndividual(Prefix+mainobjectname+".owl#xCoordinateOf"+mainobjectname);
		Individual ycoordinate = coordinateclass.createIndividual(Prefix+mainobjectname+".owl#yCoordinateOf"+mainobjectname);
		Individual xcoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname+".owl#V_xCoordinateOf"+mainobjectname);
		Individual ycoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname+".owl#V_yCoordinateOf"+mainobjectname);
		wtfcoordinate.addProperty(hasx,xcoordinate);
		wtfcoordinate.addProperty(hasy,ycoordinate);
		xcoordinate.addProperty(hasvalue,xcoordinatevalue);
		ycoordinate.addProperty(hasvalue,ycoordinatevalue);
		xcoordinatevalue.addProperty(hasunit, degree);
		ycoordinatevalue.addProperty(hasunit, degree);
		mainobjinst.addProperty(hascoordinatesystem, wtfcoordinate);
		if(mainobjectname.contains("-1")) {
			xcoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral("103.797798"));
			ycoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral("1.463083"));
		}else if(mainobjectname.contains("-2")) {
			xcoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral("103.626523"));
			ycoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral("1.277098"));
		} else if(mainobjectname.contains("-3")) {
			xcoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral("103.943946"));
			ycoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral("1.337292"));
		} else if(mainobjectname.contains("-4")) {
			xcoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral("103.620614"));
			ycoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral("1.29591"));
		}
		

		
		return Prefix+mainobjectname+".owl#"+mainobjectname;
	}
	
	public String doConversionOnSite(OntModel jenaOwlModel,String Prefix, String mainobjectname,String[] inputdata,String[] outputdata,List<String[]>propertydata) {
		Individual mainobjinst = objectwtfon.createIndividual(Prefix+mainobjectname+".owl#"+mainobjectname);
		
		//coordinate
		Individual wtfcoordinate = coordinatesystemclass.createIndividual(Prefix+mainobjectname+".owl#CoordinateSystemOf"+mainobjectname);
		Individual xcoordinate = coordinateclass.createIndividual(Prefix+mainobjectname+".owl#xCoordinateOf"+mainobjectname);
		Individual ycoordinate = coordinateclass.createIndividual(Prefix+mainobjectname+".owl#yCoordinateOf"+mainobjectname);
		Individual xcoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname+".owl#V_xCoordinateOf"+mainobjectname);
		Individual ycoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname+".owl#V_yCoordinateOf"+mainobjectname);
		wtfcoordinate.addProperty(hasx,xcoordinate);
		wtfcoordinate.addProperty(hasy,ycoordinate);
		xcoordinate.addProperty(hasvalue,xcoordinatevalue);
		ycoordinate.addProperty(hasvalue,ycoordinatevalue);
		xcoordinatevalue.addProperty(hasunit, degree);
		xcoordinatevalue.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(inputdata[1])));
		ycoordinatevalue.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(inputdata[2])));
		ycoordinatevalue.addProperty(hasunit, degree);
		mainobjinst.addProperty(hascoordinatesystem, wtfcoordinate);
		
		//unit land cost
		Individual LandCost = landcostclass.createIndividual(Prefix + mainobjectname + ".owl#LandCostOf" + mainobjectname);
		Individual VLandCost = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_LandCostOf" + mainobjectname);
		LandCost.addProperty(hasvalue, VLandCost);
		VLandCost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(500)));
		VLandCost.addProperty(hasunit, $);
		mainobjinst.addProperty(hasCost, LandCost);
		
		Individual tech1 = tech4class
				.createIndividual(Prefix + mainobjectname + ".owl#OnSiteTech1Of" + mainobjectname);
		mainobjinst.addProperty(useTechnology, tech1);
		
		Individual tech2 = tech5class
				.createIndividual(Prefix + mainobjectname + ".owl#OnSiteDigesterOf" + mainobjectname);
		mainobjinst.addProperty(useTechnology, tech2);

		Individual tech3 = tech6class
				.createIndividual(Prefix + mainobjectname + ".owl#OnSiteTech3Of" + mainobjectname);
		mainobjinst.addProperty(useTechnology, tech3);
		
			mainobjinst.setPropertyValue(uppboundtech4, jenaOwlModel.createTypedLiteral(new Integer(0)));
			mainobjinst.setPropertyValue(uppboundtech5, jenaOwlModel.createTypedLiteral(new Integer(10)));
			mainobjinst.setPropertyValue(uppboundtech6, jenaOwlModel.createTypedLiteral(new Integer(0)));

			for (int tech=1;tech<=3;tech++) {
				Individual eos = EOSclass.createIndividual(Prefix + mainobjectname + ".owl#EOSOfTech"+tech+"Of" + mainobjectname);
				Individual Veos = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_EOSOfTech"+tech+"Of" + mainobjectname);
				eos.addProperty(hasvalue, Veos);
				Veos.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(1)));
				
				Individual techcap = techcapclass.createIndividual(Prefix + mainobjectname + ".owl#TechnologyCapacity"+tech+"Of" + mainobjectname);
				Individual Vtechcap = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_TechnologyCapacity"+tech+"Of" + mainobjectname);
				techcap.addProperty(hasvalue, Vtechcap);
				
				 Vtechcap.addProperty(hasunit, ton_per_day);
				
				Individual installcost = installationcostclass.createIndividual(Prefix + mainobjectname + ".owl#InstallationCost"+tech+"Of" + mainobjectname);
				Individual Vinstallcost = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_InstallationCost"+tech+"Of" + mainobjectname);
				installcost.addProperty(hasvalue, Vinstallcost);
				
				Vinstallcost.addProperty(hasunit, $);
				
				Individual operationcost = operationcostclass.createIndividual(Prefix + mainobjectname + ".owl#OperationalCost"+tech+"Of" + mainobjectname);
				Individual Voperationcost = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_OperationalCost"+tech+"Of" + mainobjectname);
				operationcost.addProperty(hasvalue, Voperationcost);
				
				Voperationcost.addProperty(hasunit, $);
				
				Individual manpowercost = manpowercostclass.createIndividual(Prefix + mainobjectname + ".owl#ManPowerCost"+tech+"Of" + mainobjectname);
				Individual Vmanpowercost = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_ManPowerCost"+tech+"Of" + mainobjectname);
				manpowercost.addProperty(hasvalue, Vmanpowercost);
				Vmanpowercost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
				Vmanpowercost.addProperty(hasunit, $);
				
				Individual polltreatmenttax = treatmenttaxclass.createIndividual(Prefix + mainobjectname + ".owl#PollutionTreatmentTax"+tech+"Of" + mainobjectname);
				Individual Vpolltreatmenttax = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_PollutionTreatmentTax"+tech+"Of" + mainobjectname);
				polltreatmenttax.addProperty(hasvalue, Vpolltreatmenttax);
				
				Vpolltreatmenttax.addProperty(hasunit, $);
				
				if(tech==1) {
					tech1.addProperty(hasProperty, eos);
					tech1.addProperty(hastechcap, techcap);
					tech1.addProperty(hasInstallationCost, installcost);
					tech1.addProperty(hasCost, operationcost);
					tech1.addProperty(hasLaborCost, manpowercost);
					tech1.addProperty(hasTax, polltreatmenttax);
					Vtechcap.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
					Vinstallcost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
					Voperationcost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
					Vpolltreatmenttax.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
				}
				else if (tech==2) {
					tech2.addProperty(hasProperty, eos);
					tech2.addProperty(hastechcap, techcap);
					tech2.addProperty(hasInstallationCost, installcost);
					tech2.addProperty(hasCost, operationcost);
					tech2.addProperty(hasLaborCost, manpowercost);
					tech2.addProperty(hasTax, polltreatmenttax);
					for(int t=1;t<=1;t++) {
						Individual unitdevice = WasteTreatmentDeviceclass.createIndividual(Prefix + mainobjectname + ".owl#UnitDeviceOf" + mainobjectname+"_"+t);
						tech2.addProperty(realizedByDevice, unitdevice);
						unitdevice.setPropertyValue(usedInYear, jenaOwlModel.createTypedLiteral(new Integer(t))); //the amount of unit still not be possible to be included in owl files
						unitdevice.setPropertyValue(amountOfUnit, jenaOwlModel.createTypedLiteral(Math.round(new Double(outputdata[0]))));
					}
					Vtechcap.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(propertydata.get(0)[1])));
					Vinstallcost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(propertydata.get(0)[2])));
					Voperationcost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(propertydata.get(0)[3])));
					Vpolltreatmenttax.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(propertydata.get(0)[0])));
				}
				else if (tech==3) {
					tech3.addProperty(hasProperty, eos);
					tech3.addProperty(hastechcap, techcap);
					tech3.addProperty(hasInstallationCost, installcost);
					tech3.addProperty(hasCost, operationcost);
					tech3.addProperty(hasLaborCost, manpowercost);
					tech3.addProperty(hasTax, polltreatmenttax);
					Vtechcap.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
					Vinstallcost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
					Voperationcost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
					Vpolltreatmenttax.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
				}
				
				//resource consumption
				for (int resource = 1; resource <= 2; resource++) {
					Individual ResourceConsumption = resourceconsumptionclass.createIndividual(Prefix + mainobjectname
							+ ".owl#ResourceConsumption_Tech" + tech + "_" + resource + "Of" + mainobjectname);

					Individual vResourceConsumption = scalarvalueclass.createIndividual(Prefix + mainobjectname
							+ ".owl#V_ResourceConsumption_Tech" + tech + "_" + resource + "Of" + mainobjectname);
					ResourceConsumption.addProperty(hasvalue, vResourceConsumption);

					if (tech == 1) { // add property revenue to main object also
						tech1.addProperty(requiredConsumption, ResourceConsumption);
						Individual ResourceConsumptionCost = resourceconsumptioncostclass.createIndividual(Prefix
								+ mainobjectname + ".owl#ResourceConsumptionCost" + resource + "Of" + mainobjectname);

						Individual vResourceConsumptionCost = scalarvalueclass.createIndividual(Prefix + mainobjectname
								+ ".owl#V_ResourceConsumptionCost" + resource + "Of" + mainobjectname);
						ResourceConsumptionCost.addProperty(hasvalue, vResourceConsumptionCost);
						mainobjinst.addProperty(hasUtilityCost, ResourceConsumptionCost);
						vResourceConsumptionCost.addProperty(hasunit, $);

						if (resource == 1) {
							ResourceConsumptionCost.addProperty(inContextOf, water);
							vResourceConsumptionCost.setPropertyValue(numval,
									jenaOwlModel.createTypedLiteral(new Double(0)));
							vResourceConsumption.setPropertyValue(numval,
									jenaOwlModel.createTypedLiteral(new Double(0)));
						} else if (resource == 2) {
							ResourceConsumptionCost.addProperty(inContextOf, energy);
							vResourceConsumptionCost.setPropertyValue(numval,
									jenaOwlModel.createTypedLiteral(new Double(0.1)));
							vResourceConsumption.setPropertyValue(numval,
									jenaOwlModel.createTypedLiteral(new Double(0)));
						}

					} else if (tech == 2) {
						tech2.addProperty(requiredConsumption, ResourceConsumption);
						if (resource == 2) {
							vResourceConsumption.setPropertyValue(numval,
									jenaOwlModel.createTypedLiteral(new Double(propertydata.get(0)[5])));
						} else {
							vResourceConsumption.setPropertyValue(numval,
									jenaOwlModel.createTypedLiteral(new Double(0)));
						}

					} else if (tech == 3) {
						tech3.addProperty(requiredConsumption, ResourceConsumption);
						if (resource == 2) {
							vResourceConsumption.setPropertyValue(numval,
									jenaOwlModel.createTypedLiteral(new Double(0)));
						} else {
							vResourceConsumption.setPropertyValue(numval,
									jenaOwlModel.createTypedLiteral(new Double(0)));
						}

					}

					if (resource == 1) {
						ResourceConsumption.addProperty(inContextOf, water);
						vResourceConsumption.addProperty(hasunit, ton);

					} else {
						ResourceConsumption.addProperty(inContextOf, energy);
						vResourceConsumption.addProperty(hasunit, kWh);

					}
				}
				
				
				//resource recovery
				for (int recoveredsource = 1; recoveredsource <= 3; recoveredsource++) {
					Individual TransferRate = transferrateclass.createIndividual(Prefix + mainobjectname
							+ ".owl#TransferRate_Tech" + tech + "_" + recoveredsource + "Of" + mainobjectname);

					Individual vTransferRate = scalarvalueclass.createIndividual(Prefix + mainobjectname
							+ ".owl#V_TransferRate_Tech" + tech + "_" + recoveredsource + "Of" + mainobjectname);
					TransferRate.addProperty(hasvalue, vTransferRate);

					if (tech == 1) { // add property REVENUE to main object also
						tech1.addProperty(hasTransferRate, TransferRate);
						Individual Revenue = revenueclass.createIndividual(
								Prefix + mainobjectname + ".owl#Revenue" + recoveredsource + "Of" + mainobjectname);
						mainobjinst.addProperty(hasRevenue, Revenue);

						Individual vRevenue = scalarvalueclass.createIndividual(
								Prefix + mainobjectname + ".owl#V_Revenue" + recoveredsource + "Of" + mainobjectname);
						Revenue.addProperty(hasvalue, vRevenue);
						vRevenue.addProperty(hasunit, $);
						if (recoveredsource == 1) {
							Revenue.addProperty(obtainedFrom, recoveredwaste_a);
							vRevenue.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
							vTransferRate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));

						} else if (recoveredsource == 2) {
							Revenue.addProperty(obtainedFrom, recoveredwaste_b);
							vRevenue.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
							vTransferRate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));

						} else if (recoveredsource == 3) {
							Revenue.addProperty(obtainedFrom, electricity);
							vRevenue.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0.1)));
							vTransferRate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
						}
					} else if (tech == 2) {
						tech2.addProperty(hasTransferRate, TransferRate);
						if (recoveredsource == 3) {
							vTransferRate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(propertydata.get(0)[4])));
						} else {
							vTransferRate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
						}
					} else if (tech == 3) {
						tech3.addProperty(hasTransferRate, TransferRate);
						if (recoveredsource == 3) {
							vTransferRate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
						} else {
							vTransferRate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
						}
					}
					if (recoveredsource == 1) {
						TransferRate.addProperty(obtainedFrom, recoveredwaste_a);
						vTransferRate.addProperty(hasunit, ton);
					} else if (recoveredsource == 2) {
						TransferRate.addProperty(obtainedFrom, recoveredwaste_b);
						vTransferRate.addProperty(hasunit, ton);
					} else if (recoveredsource == 3) {
						TransferRate.addProperty(obtainedFrom, electricity);
						vTransferRate.addProperty(hasunit, kWh);
					}
				}
			
		}
		
		return Prefix+mainobjectname+".owl#"+mainobjectname;
	}
	
	public void doConversionWasteSystem(OntModel jenaOwlModel,String Prefix, String mainobjectname, String transportiri, List<String>foodcourt, List<String>wtf) {
		
		Individual mainobjinst = compositeclass.createIndividual(Prefix+mainobjectname+".owl#"+mainobjectname);
		
		System.out.println("it goes through here");
		for (int d = 1; d <= 7; d++) {
			Individual benchmark = benchmarkclass.createIndividual(Prefix + mainobjectname + ".owl#Benchmark" + d + "Of" + mainobjectname);
			Individual vbenchmark = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_Benchmark" + d + "Of" + mainobjectname);
			benchmark.addProperty(hasvalue, vbenchmark);
			
			if(d==1) {
				benchmark.addOntClass(landcostclass);
				vbenchmark.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(2700000)));
				vbenchmark.addProperty(hasunit, $);
			}
			else if(d==2) {
				benchmark.addOntClass(operationcostclass);
				vbenchmark.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(56540000)));
				vbenchmark.addProperty(hasunit, $);
			}
			else if(d==3) {
				benchmark.addOntClass(manpowercostclass);
				vbenchmark.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
				vbenchmark.addProperty(hasunit, $);
			}
			else if(d==4) {
				benchmark.addOntClass(installationcostclass);
				vbenchmark.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(890000000)));
				vbenchmark.addProperty(hasunit, $);
			}
			else if(d==5) {
				benchmark.addOntClass(lifecycleclass);
				vbenchmark.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Integer(16)));
			}
			else if(d==6) {//energy
				benchmark.addOntClass(resourceconsumptionclass);
				vbenchmark.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(70)));
				benchmark.addProperty(inContextOf, energy);
				 vbenchmark.addProperty(hasunit, kWh);
			}
			else if(d==7) {//water
				benchmark.addOntClass(resourceconsumptionclass);
				vbenchmark.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
				benchmark.addProperty(inContextOf, water);
				 vbenchmark.addProperty(hasunit, ton);
			}
			mainobjinst.addProperty(hasBenchmark, benchmark);
		}
		System.out.println("it goes through here2");
		Individual discountrate = discountrateclass.createIndividual(Prefix+mainobjectname+".owl#DiscountRateOf"+mainobjectname);
		Individual vdiscountrate = scalarvalueclass.createIndividual(Prefix+mainobjectname+".owl#V_DiscountRateOf"+mainobjectname);
		discountrate.addProperty(hasvalue, vdiscountrate);
		vdiscountrate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (0.01)));
		mainobjinst.addProperty(hasDiscountRate, discountrate);
		
		Individual lifecycle = lifecycleclass.createIndividual(Prefix+mainobjectname+".owl#LifeCycleOf"+mainobjectname);
		Individual vlifecycle = scalarvalueclass.createIndividual(Prefix+mainobjectname+".owl#V_LifeCycleOf"+mainobjectname);
		lifecycle.addProperty(hasvalue, vlifecycle);
		vlifecycle.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Integer (15)));
		mainobjinst.addProperty(hasLifeCycle, lifecycle);
		
		Individual disposalfee = disposalfeeclass.createIndividual(Prefix+mainobjectname+".owl#DisposalFeeOf"+mainobjectname);
		Individual vdisposalfee = scalarvalueclass.createIndividual(Prefix+mainobjectname+".owl#V_DisposalFeeOf"+mainobjectname);
		disposalfee.addProperty(hasvalue, vdisposalfee);
		vdisposalfee.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (70)));
		vdisposalfee.addProperty(hasunit, $perton);
		mainobjinst.addProperty(hasUtilityCost, disposalfee);
		
		//==============================================newly added based on output 5 mar 2020
		Individual installcost = installationcostclass.createIndividual(Prefix + mainobjectname + ".owl#InstallationCostOf" + mainobjectname);
		Individual Vinstallcost = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_InstallationCostOf" + mainobjectname);
		installcost.addProperty(hasvalue, Vinstallcost);
		
		Vinstallcost.addProperty(hasunit, $);
		
		Individual operationcost = operationcostclass.createIndividual(Prefix + mainobjectname + ".owl#OperationalCostOf" + mainobjectname);
		Individual Voperationcost = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_OperationalCostOf" + mainobjectname);
		operationcost.addProperty(hasvalue, Voperationcost);
		
		Voperationcost.addProperty(hasunit, $);
		
		Individual manpowercost = manpowercostclass.createIndividual(Prefix + mainobjectname + ".owl#ManPowerCostOf" + mainobjectname);
		Individual Vmanpowercost = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_ManPowerCostOf" + mainobjectname);
		manpowercost.addProperty(hasvalue, Vmanpowercost);
		Vmanpowercost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(0)));
		Vmanpowercost.addProperty(hasunit, $);
		
		//unit land cost
		Individual LandCost = landcostclass.createIndividual(Prefix + mainobjectname + ".owl#LandCostOf" + mainobjectname);
		Individual VLandCost = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_LandCostOf" + mainobjectname);
		LandCost.addProperty(hasvalue, VLandCost);
		VLandCost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(300)));
		VLandCost.addProperty(hasunit, $);
		
		Individual transportcost = transportcostclass.createIndividual(Prefix+mainobjectname+".owl#TransportCostOf"+mainobjectname);
		Individual vtransportcost = scalarvalueclass.createIndividual(Prefix+mainobjectname+".owl#V_TransportCostOf"+mainobjectname);
		transportcost.addProperty(hasvalue, vtransportcost);
		vtransportcost.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(3)));
		vtransportcost.addProperty(hasunit, $);
		
		Individual polltreatmenttax = treatmenttaxclass.createIndividual(Prefix + mainobjectname + ".owl#PollutionTreatmentTaxOf" + mainobjectname);
		Individual Vpolltreatmenttax = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_PollutionTreatmentTaxOf" + mainobjectname);
		polltreatmenttax.addProperty(hasvalue, Vpolltreatmenttax);
		Vpolltreatmenttax.addProperty(hasunit, $);
		
		Individual Revenue = revenueclass.createIndividual(Prefix + mainobjectname + ".owl#TotalRevenueOf" + mainobjectname);
		Individual vRevenue = scalarvalueclass.createIndividual(Prefix + mainobjectname + ".owl#V_TotalRevenueOf" + mainobjectname);
		Revenue.addProperty(hasvalue, vRevenue);
		vRevenue.addProperty(hasunit, $);
		
		Individual ResourceConsumptionCost = resourceconsumptioncostclass.createIndividual(Prefix+mainobjectname + ".owl#ResourceConsumptionCostOf" + mainobjectname);
		Individual vResourceConsumptionCost = scalarvalueclass.createIndividual(Prefix + mainobjectname+ ".owl#V_ResourceConsumptionCostOf" + mainobjectname);
		ResourceConsumptionCost.addProperty(hasvalue, vResourceConsumptionCost);
		vResourceConsumptionCost.addProperty(hasunit, $);
		
		mainobjinst.addProperty(hasUtilityCost, ResourceConsumptionCost);
		mainobjinst.addProperty(hasRevenue, Revenue);
		mainobjinst.addProperty(hasCost, LandCost);
		mainobjinst.addProperty(hasInstallationCost, installcost);
		mainobjinst.addProperty(hasCost, operationcost);
		mainobjinst.addProperty(hasTax, polltreatmenttax);
		mainobjinst.addProperty(hasLaborCost, manpowercost);
		mainobjinst.addProperty(hasTransportationcost, transportcost);
		
		//================================================================
		
		Resource mainobjtrans = jenaOwlModel.createResource(transportiri);
		mainobjinst.addProperty(hasSubsystem, mainobjtrans);

		for (int el=0;el<foodcourt.size();el++) {
			Resource mainobjfc = jenaOwlModel.createResource(foodcourt.get(el));
			mainobjinst.addProperty(hasSubsystem, mainobjfc);
			System.out.println(foodcourt.get(el));
		}
		for (int el2=0;el2<wtf.size();el2++) {
			Resource mainobjwtf = jenaOwlModel.createResource(wtf.get(el2));
			mainobjinst.addProperty(hasSubsystem, mainobjwtf);
			System.out.println(wtf.get(el2));
		}
		
		
		
	}
	
	public void startConversion(String flag,List<String[]> inputdata,List<String[]> outputdata,List<String[]> propertydata) throws Exception {
    	
    	//String filePath = baseURL2 + "wastetemplate.owl"; // the empty owl file
    	String filePath=AgentLocator.getPathToWorkingDir(this)+"/wastetemplate.owl";

			FileInputStream inFile = new FileInputStream(filePath);
			Reader in = new InputStreamReader(inFile, "UTF-8");
			String Prefix="http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/";

			String mainobjectname="TransportSystem-001"; //still hard-coded for the sample
			
			if(flag.contains("transport")) {
				OntModel jenaOwlModel = ModelFactory.createOntologyModel();
				jenaOwlModel.read(in, null);
				initOWLClasses(jenaOwlModel);
				transportiri=doConversionTransport(jenaOwlModel,Prefix, mainobjectname);
				
				
				String content = JenaHelper.writeToString(jenaOwlModel);
				new QueryBroker().putOld(Prefix+mainobjectname+".owl", content);
				System.out.println("it is processed= " + mainobjectname);
			}
			else if(flag.contains("onsitewtf")) {
				for(int d=1;d<=outputdata.size();d++) {
					String wtfname="OnSiteWasteTreatment-"+String.format("%03d", d); ; 
					if(Double.parseDouble(outputdata.get(d-1)[0])!=0.0) {
						inFile = new FileInputStream(filePath);
						in = new InputStreamReader(inFile, "UTF-8");
						OntModel jenaOwlModel = ModelFactory.createOntologyModel();
						jenaOwlModel.read(in, null);
						initOWLClasses(jenaOwlModel);
						String iriofwtf=doConversionOnSite(jenaOwlModel,Prefix, wtfname,inputdata.get(d-1),outputdata.get(d-1),propertydata);
						//wtf.add(iriofwtf); should the onsite attached to the waste system??
						String content = JenaHelper.writeToString(jenaOwlModel);
						new QueryBroker().putOld(Prefix+wtfname+".owl", content);
						onsiteiri.add(Prefix+wtfname+".owl#"+wtfname);
					}
				}

//				for(int d=1;d<=4;d++) { //incinerator not to be touched again which is d=4
//					inFile = new FileInputStream(filePath);
//					in = new InputStreamReader(inFile, "UTF-8");
//					OntModel jenaOwlModel = ModelFactory.createOntologyModel();
//					jenaOwlModel.read(in, null);
//					initOWLClasses(jenaOwlModel);
//					String wtfname="WasteTreatment-"+d; 
//					String iriofwtf=doConversionWTF(jenaOwlModel,Prefix, wtfname);
//					wtf.add(iriofwtf);
//					String content = JenaHelper.writeToString(jenaOwlModel);
//					new QueryBroker().putOld(Prefix+wtfname+".owl", content);
//				}
			 
			}
			else if(flag.contains("foodcourt")) {
				
				String csv = new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/fcdetails.csv");
				List<String[]> readingFromCSV = MatrixConverter.fromCsvToArray(csv);
				int size=readingFromCSV.size();

				for(int d=1;d<size;d++) {
					String[]data=readingFromCSV.get(d);
					inFile = new FileInputStream(filePath);
					in = new InputStreamReader(inFile, "UTF-8");
					OntModel jenaOwlModel2 = ModelFactory.createOntologyModel();
					jenaOwlModel2.read(in, null);
					initOWLClasses(jenaOwlModel2);

					String fcname="FoodCourt-"+String.format("%03d", d); 
				String irioffc=doConversionFC(jenaOwlModel2,Prefix, fcname,data);
				foodcourt.add(irioffc);
				String content = JenaHelper.writeToString(jenaOwlModel2);
				new QueryBroker().putOld(Prefix+fcname+".owl", content);
				}
				System.out.println("it is processed= " + flag);
			}
			else if(flag.contains("wtf")) {

				for(int d=1;d<=3;d++) { //incinerator not to be touched again which is d=4
					inFile = new FileInputStream(filePath);
					in = new InputStreamReader(inFile, "UTF-8");
					OntModel jenaOwlModel = ModelFactory.createOntologyModel();
					jenaOwlModel.read(in, null);
					initOWLClasses(jenaOwlModel);
					String wtfname="OffsiteWasteTreatment-"+d; 
					String iriofwtf=doConversionWTF(jenaOwlModel,Prefix, wtfname);
					wtf.add(iriofwtf);
					String content = JenaHelper.writeToString(jenaOwlModel);
					new QueryBroker().putOld(Prefix+wtfname+".owl", content);
				}
				System.out.println("it is processed= " + flag);
			 
			}

			else if(flag.contains("system")) { //for the waste treatment system
				inFile = new FileInputStream(filePath);
				in = new InputStreamReader(inFile, "UTF-8");
				System.out.println("it goes to system");
				OntModel jenaOwlModel = ModelFactory.createOntologyModel();
				jenaOwlModel.read(in, null);
				initOWLClasses(jenaOwlModel);
				String sysname="SingaporeWasteSystem"; 
				doConversionWasteSystem(jenaOwlModel,Prefix, sysname,transportiri, foodcourt,wtf);
				String content = JenaHelper.writeToString(jenaOwlModel);
				new QueryBroker().putOld(Prefix+sysname+".owl", content);
				
			}


		//}  	

}
		
	public void executeConversion() throws Exception { //not valid for onsite wtf, need provide additional data first
		System.out.println("Starting Process");
		WTEKBCreator converter = new WTEKBCreator();
		converter.startConversion("foodcourt",null,null,null);
		converter.startConversion("transport",null,null,null);
		converter.startConversion("wtf",null,null,null);
		transportiri="http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/TransportSystem-001.owl#TransportSystem-001";
		converter.startConversion("system",null,null,null); //it is completed no need to be rerun again
		
	}
	
	public static void main(String[] args) throws Exception {
		
		
		System.out.println("Starting Process");
		WTEKBCreator converter = new WTEKBCreator();
		converter.executeConversion();
		
		/**list of uncomplete features:
		 * put coordinates in onsite wtf
		 * start conversion of onsite wtf
		
		*/

	}


}
