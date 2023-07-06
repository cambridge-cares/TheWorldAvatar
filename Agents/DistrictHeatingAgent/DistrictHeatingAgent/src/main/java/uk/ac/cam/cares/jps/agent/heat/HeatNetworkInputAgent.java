package uk.ac.cam.cares.jps.agent.heat;

import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.update.UpdateRequest;
import org.jooq.exception.IOException;
import org.apache.jena.graph.NodeFactory;
import org.apache.logging.log4j.Logger;
import org.jooq.exception.DataAccessException;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;
import org.apache.logging.log4j.LogManager;

public class HeatNetworkInputAgent {

    // Common Base URLs
    public static String RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    public static String RDFS = "http://www.w3.org/2000/01/rdf-schema#";
    public static String XSD = "http://www.w3.org/2001/XMLSchema#";
    public static String OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    public static String OHN = "https://www.theworldavatar.com/kg/ontoheatnetwork/";
    public static String EMS = "https://www.theworldavatar.com/kg/ontoems/";
    public static String TS = "https://www.theworldavatar.com/kg/ontotimeseries/";
    public static String KB = "https://www.theworldavatar.com/kb/ontoheatnetwork/";

    // IRIs for OntoHeatNet and others
    public static String OC_HAS_PART = "http://www.theworldavatar.com/ontology/meta_model/mereology/mereology.owl#hasPart";
    public static String OC_isOwnerOf = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isOwnerOf";
    public static String OC_hasRevenue = "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_performance/economic_performance.owl#hasRevenue";
    public static String OCP_hasCO2emission = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasIndividualCO2Emission";
    public static String OCP_hasFuelType = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasFuelType";
    public static String OCP_FuelType = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#FuelType";

    // OntoTimeSeries
    public static String TS_TIMESERIES = TS + "TimeSeries";
    public static String TS_HAS_TIMESERIES = TS + "hasTimeSeries";
    public static String TS_HAS_TIME_UNIT = TS + "hasTimeUnit";
    public static String TS_HAS_RDB = TS + "hasRDB";

    // For units of measure
    public static String OM_QUANTITY = OM + "Quantity";
    public static String OM_MEASURE = OM + "Measure";
    public static String OM_UNIT = OM + "Unit";
    public static String OM_HAS_VALUE = OM + "hasValue";
    public static String OM_HAS_UNIT = OM + "hasUnit";
    public static String OM_SYMBOL = OM + "symbol";
    public static String OM_Energy = OM + "Energy";
    public static String OM_POWER = OM + "Power";
    public static String OM_DURATION = OM + "Duration";
    public static String OM_VFR = OM + "VolumetricFlowRate";
    public static String OM_Has_NUMERICAL_VALUE = OM + "hasNumericalValue";
    public static String OM_DENSITY = OM + "Density";
    public static String OM_TEMPERATURE = OM + "Temperature";
    public static String OM_PRESSURE = OM + "Pressure";
    public static String OM_SHC = OM + "SpecificHeatCapacity";

    // Data types
    public static String RDF_TYPE = RDF + "type";
    public static String RDFS_COMMENT = RDFS + "comment";
    public static String RDFS_LABEL = RDFS + "label";
    public static String XSD_STRING = XSD + "string";
    public static String XSD_FLOAT = XSD + "float";
    public static String XSD_DATE = XSD + "date";
    public static String XSD_BOOLEAN = XSD + "boolean";

    // The time series client to interact with the knowledge graph and data storage
    private TimeSeriesClient<OffsetDateTime> tsClient;

    // A list of mappings between JSON keys and the corresponding IRI, contains one mapping per time series
    private static List<JSONKeyToIRIMapper> mappings;

    // The JSON key for the timestamp
    public static final String timestampKey = "obsTimeUtc";

    // The prefix to use when no IRI exists for a JSON key originally
    public static final String generatedIRIPrefix = "http://www.theworldavatar.com/kb/ontotimeseries/" + "heat";

    // The time unit used for all time series maintained by the heating network input agent
    public static final String timeUnit = OffsetDateTime.class.getSimpleName();

    // Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(HeatNetworkInputAgentLauncher.class);
    private static String sparqlendpoint;
    
    
    // For TS IRI
    public static String WaermeleistungKessel4 = "";
    public static String WaermeleistungKessel5= "";
    public static String WaermeleistungKessel6="";
    public static String CO2Preis="";
    public static String GTWaermeleistung="";
    public static String GaspreisGT="";
    public static String WaermemengeInnenstadt="";
    public static String TempRuecklauf="";
    public static String MHKWTempVorlauf="";
    public static String TempVorlauf="";
    public static String Spotpreis="";
    public static String WaermemengeMHKW="";
    public static String GaspreisKessel="";
    public static String Aussentemperatur="";
    public static String MHKWTempRuecklauf="";
    public static String GasverbrauchGT="";
    public static String GasbezugKessel4="";
    public static String GasbezugKessel5="";
    public static String GasbezugKessel6="";
    public static String GTActive="";
    public static String Kessel4Active="";
    public static String Kessel5Active="";
    public static String Kessel6Active="";
    public static String CHPBonus="";
    public static String Gridcharges="";
    public static String HourlyLabourCostBoiler="";
    public static String HourlyLabourCostGT="";
    public static String HourlyWearCostBoiler="";
    public static String HourlyWearCostGT="";
    public static String DemandDrivenWearCostBoiler="";
    public static String DemandDrivenWearCostGT="";
    public static String PressureRuecklauf="";
    public static String MHKWPressureV="";
    public static String PressureVorlauf="";
    public static String MHKWPressureRuecklauf="";
    public static String MinHourlySupply="";
    public static String MaxHourlySupply="";
    public static String AvaiIn="";
    public static String AvaiGT="";
    public static String AvaiKessel4="";
    public static String AvaiKessel5="";
    public static String AvaiKessel6="";
    public static String IsHoliday="";
    public static String IsVacation="";
    public static String GTWirkleistung="";
    
    
    

    // The instantiation of static data
    public void dataInstantiation(String endpoint) {

    	sparqlendpoint = endpoint;
        String staticData = System.getenv("CSVPATH");
        String[] numericValue = ReadColInput(1,staticData,",");

        // static data
        float Value_VolumetricFlowRate = Float.parseFloat(numericValue[0]);
        float Value_RatedElectricalPower = Float.parseFloat(numericValue[1]);
        float Value_MinimumThermalLoad = Float.parseFloat(numericValue[2]);
        float Value_MinimumIdleTime = Float.parseFloat(numericValue[3]);
        float Value_MinPurchase = Float.parseFloat(numericValue[4]);
        float Value_MaxPurchase = Float.parseFloat(numericValue[5]);
        float Value_TierOneCap = Float.parseFloat(numericValue[6]);
        float Value_TierTwoCap = Float.parseFloat(numericValue[7]);
        float Value_TierOneUnitPrice = Float.parseFloat(numericValue[8]);
        float Value_TierTwoUnitPrice = Float.parseFloat(numericValue[9]);
        float Value_ThermalLoad_GT = Float.parseFloat(numericValue[10]);
        float Value_HCV_GT = Float.parseFloat(numericValue[11]);
        float Value_LCV_GT = Float.parseFloat(numericValue[12]);
        float Value_CO2Factor_GT = Float.parseFloat(numericValue[13]);
        float Value_ThermalLoad_HB4 = Float.parseFloat(numericValue[14]);
        float Value_HCV_HB4 = Float.parseFloat(numericValue[15]);
        float Value_LCV_HB4 = Float.parseFloat(numericValue[16]);
        float Value_CO2Factor_HB4 = Float.parseFloat(numericValue[17]);
        float Value_ThermalLoad_HB5 = Float.parseFloat(numericValue[18]);
        float Value_HCV_HB5 = Float.parseFloat(numericValue[19]);
        float Value_LCV_HB5 = Float.parseFloat(numericValue[20]);
        float Value_CO2Factor_HB5 = Float.parseFloat(numericValue[21]);
        float Value_ThermalLoad_HB6 = Float.parseFloat(numericValue[22]);
        float Value_HCV_HB6 = Float.parseFloat(numericValue[23]);
        float Value_LCV_HB6 = Float.parseFloat(numericValue[24]);
        float Value_CO2Factor_HB6 = Float.parseFloat(numericValue[25]);


        // For the Consumer part
        UpdateBuilder Consumer_ub =
                new UpdateBuilder()
                        .addInsert(NodeFactory.createURI(KB + "Consumer"), NodeFactory.createURI(OHN + "hasHeatDemand"), NodeFactory.createURI(KB + "HeatDemand"))
                        .addInsert(NodeFactory.createURI(KB + "Consumer"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "Consumer"))
                        .addInsert(NodeFactory.createURI(KB + "Consumer"), NodeFactory.createURI(RDFS_LABEL), "Consumer")
                        .addInsert(NodeFactory.createURI(KB + "HeatDemand"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "HeatDemand"));
        UpdateRequest Consumer_ur = Consumer_ub.buildRequest();
        AccessAgentCaller.updateStore(sparqlendpoint, Consumer_ur.toString());
       

        // For the Heating Network part
        UpdateBuilder HeatingNetwork_ub =
                new UpdateBuilder()
                        .addInsert(NodeFactory.createURI(KB + "HeatingNetwork"), NodeFactory.createURI(OHN + "suppliesHeatTo"), NodeFactory.createURI(KB + "Consumer"))
                        .addInsert(NodeFactory.createURI(KB + "HeatingNetwork"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "HeatingNetwork"))
                        .addInsert(NodeFactory.createURI(KB + "HeatingNetwork"), NodeFactory.createURI(RDFS_LABEL), "HeatingNetwork")
                        .addInsert(NodeFactory.createURI(KB + "HeatingNetwork"), NodeFactory.createURI(OC_HAS_PART), NodeFactory.createURI(KB + "GridConnectionUPMunicipal"))
                        .addInsert(NodeFactory.createURI(KB + "HeatingNetwork"), NodeFactory.createURI(OC_HAS_PART), NodeFactory.createURI(KB + "GridConnectionDownMunicipal"))
                        .addInsert(NodeFactory.createURI(KB + "HeatingNetwork"), NodeFactory.createURI(OC_HAS_PART), NodeFactory.createURI(KB + "GridConnectionUPEfW"))
                        .addInsert(NodeFactory.createURI(KB + "HeatingNetwork"), NodeFactory.createURI(OC_HAS_PART), NodeFactory.createURI(KB + "GridConnectionDownEfW"))
                        .addInsert(NodeFactory.createURI(KB + "GridConnectionUPMunicipal"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "GridConnection"))
                        .addInsert(NodeFactory.createURI(KB + "GridConnectionDownMunicipal"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "GridConnection"))
                        .addInsert(NodeFactory.createURI(KB + "GridConnectionUPEfW"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "GridConnection"))
                        .addInsert(NodeFactory.createURI(KB + "GridConnectionDownEfW"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "GridConnection"))
                        .addInsert(NodeFactory.createURI(KB + "HeatingNetwork"), NodeFactory.createURI(OHN + "hasMinFlowRate"), NodeFactory.createURI(KB + "VolumetricFlowRate"))
                        .addInsert(NodeFactory.createURI(KB + "VolumetricFlowRate"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_VFR));
        UpdateRequest HeatingNetwork_ur = HeatingNetwork_ub.buildRequest();
        AccessAgentCaller.updateStore(sparqlendpoint, HeatingNetwork_ur.toString());
        omHasValueNonTS("VolumetricFlowRate", "cubicMetrePerHour", Value_VolumetricFlowRate);


        // For GridConnection part
        UpdateBuilder GridConnection_ub =
                new UpdateBuilder()
                        .addInsert(NodeFactory.createURI(KB + "GridConnectionUPMunicipal"), NodeFactory.createURI(OHN + "hasObservableProperty"), NodeFactory.createURI(KB + "UPMunicipal" + "Pressure"))
                        .addInsert(NodeFactory.createURI(KB + "GridConnectionDownMunicipal"), NodeFactory.createURI(OHN + "hasObservableProperty"), NodeFactory.createURI(KB + "DownMunicipal" + "Pressure"))
                        .addInsert(NodeFactory.createURI(KB + "GridConnectionUPEfW"), NodeFactory.createURI(OHN + "hasObservableProperty"), NodeFactory.createURI(KB + "UPEfW" + "Pressure"))
                        .addInsert(NodeFactory.createURI(KB + "GridConnectionDownEfW"), NodeFactory.createURI(OHN + "hasObservableProperty"), NodeFactory.createURI(KB + "DownEfW" + "Pressure"))
                        .addInsert(NodeFactory.createURI(KB + "UPMunicipal" + "Pressure"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_PRESSURE))
                        .addInsert(NodeFactory.createURI(KB + "DownMunicipal" + "Pressure"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_PRESSURE))
                        .addInsert(NodeFactory.createURI(KB + "UPEfW" + "Pressure"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_PRESSURE))
                        .addInsert(NodeFactory.createURI(KB + "DownEfW" + "Pressure"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_PRESSURE))
                        .addInsert(NodeFactory.createURI(KB + "GridConnectionUPMunicipal"), NodeFactory.createURI(OHN + "hasObservableProperty"), NodeFactory.createURI(KB + "UPMunicipal" + "Temperature"))
                        .addInsert(NodeFactory.createURI(KB + "GridConnectionDownMunicipal"), NodeFactory.createURI(OHN + "hasObservableProperty"), NodeFactory.createURI(KB + "DownMunicipal" + "Temperature"))
                        .addInsert(NodeFactory.createURI(KB + "GridConnectionUPEfW"), NodeFactory.createURI(OHN + "hasObservableProperty"), NodeFactory.createURI(KB + "UPEfW" + "Temperature"))
                        .addInsert(NodeFactory.createURI(KB + "GridConnectionDownEfW"), NodeFactory.createURI(OHN + "hasObservableProperty"), NodeFactory.createURI(KB + "DownEfW" + "Temperature"))
                        .addInsert(NodeFactory.createURI(KB + "UPMunicipal" + "Temperature"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_TEMPERATURE))
                        .addInsert(NodeFactory.createURI(KB + "DownMunicipal" + "Temperature"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_TEMPERATURE))
                        .addInsert(NodeFactory.createURI(KB + "UPEfW" + "Temperature"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_TEMPERATURE))
                        .addInsert(NodeFactory.createURI(KB + "DownEfW" + "Temperature"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_TEMPERATURE));
        UpdateRequest GridConnection_ur = GridConnection_ub.buildRequest();
        AccessAgentCaller.updateStore(sparqlendpoint, GridConnection_ur.toString());


        // For the HeatProvider part
        UpdateBuilder HeatProvider_ub =
                new UpdateBuilder()
                        .addInsert(NodeFactory.createURI(KB + "MunicipalUtility"), NodeFactory.createURI(OHN + "providesHeatTo"), NodeFactory.createURI(KB + "HeatingNetwork"))
                        .addInsert(NodeFactory.createURI(KB + "MunicipalUtility"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "MunicipalUtility"))
                        .addInsert(NodeFactory.createURI(KB + "IncinerationPlant"), NodeFactory.createURI(OHN + "providesHeatTo"), NodeFactory.createURI(KB + "HeatingNetwork"))
                        .addInsert(NodeFactory.createURI(KB + "IncinerationPlant"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "IncinerationPlant"))
                        .addInsert(NodeFactory.createURI(KB + "IncinerationPlant"), NodeFactory.createURI(RDFS_LABEL), "IncinerationPlant")
                        .addInsert(NodeFactory.createURI(KB + "MunicipalUtility"), NodeFactory.createURI(OHN + "hasUpstreamGridConnection"), NodeFactory.createURI(KB + "GridConnectionUPMunicipal"))
                        .addInsert(NodeFactory.createURI(KB + "IncinerationPlant"), NodeFactory.createURI(OHN + "hasUpstreamGridConnection"), NodeFactory.createURI(KB + "GridConnectionUPEfW"))
                        .addInsert(NodeFactory.createURI(KB + "MunicipalUtility"), NodeFactory.createURI(OHN + "hasDownstreamGridConnection"), NodeFactory.createURI(KB + "GridConnectionDownMunicipal"))
                        .addInsert(NodeFactory.createURI(KB + "IncinerationPlant"), NodeFactory.createURI(OHN + "hasDownstreamGridConnection"), NodeFactory.createURI(KB + "GridConnectionDownEfW"));
        UpdateRequest HeatProvider_ur = HeatProvider_ub.buildRequest();
        AccessAgentCaller.updateStore(sparqlendpoint, HeatProvider_ur.toString());


        // For the MunicipalUtility part
        UpdateBuilder MunicipalUtility_ub =
                new UpdateBuilder()
                        .addInsert(NodeFactory.createURI(KB + "MunicipalUtility"), NodeFactory.createURI(OHN + "operates"), NodeFactory.createURI(KB + "HeatingNetwork"))
                        .addInsert(NodeFactory.createURI(KB + "MunicipalUtility"), NodeFactory.createURI(RDFS_LABEL), "MunicipalUtility")
                        .addInsert(NodeFactory.createURI(KB + "MunicipalUtility"), NodeFactory.createURI(OC_isOwnerOf), NodeFactory.createURI(KB + "ThermalEnergyStorage"))
                        .addInsert(NodeFactory.createURI(KB + "ThermalEnergyStorage"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#ThermalEnergyStorage"))
                        .addInsert(NodeFactory.createURI(KB + "MunicipalUtility"), NodeFactory.createURI(OHN + "hasPurchaseAgreement"), NodeFactory.createURI(KB + "Contract"))
                        .addInsert(NodeFactory.createURI(KB + "Contract"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI("https://spec.edmcouncil.org/fibo/ontology/FND/Agreements/Contracts/Contract"))
                        .addInsert(NodeFactory.createURI(KB + "MunicipalUtility"), NodeFactory.createURI(OC_isOwnerOf), NodeFactory.createURI(KB + "HeatGeneratorGT"))
                        .addInsert(NodeFactory.createURI(KB + "HeatGeneratorGT"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "GasTurbine"))
                        .addInsert(NodeFactory.createURI(KB + "HeatGeneratorGT"), NodeFactory.createURI(RDFS_LABEL), "GasTurbine")
                        .addInsert(NodeFactory.createURI(KB + "MunicipalUtility"), NodeFactory.createURI(OC_isOwnerOf), NodeFactory.createURI(KB + "HeatGeneratorBoil4"))
                        .addInsert(NodeFactory.createURI(KB + "HeatGeneratorBoil4"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "HeatBoiler"))
                        .addInsert(NodeFactory.createURI(KB + "HeatGeneratorBoil4"), NodeFactory.createURI(RDFS_LABEL), "Boil4")
                        .addInsert(NodeFactory.createURI(KB + "MunicipalUtility"), NodeFactory.createURI(OC_isOwnerOf), NodeFactory.createURI(KB + "HeatGeneratorBoil5"))
                        .addInsert(NodeFactory.createURI(KB + "HeatGeneratorBoil5"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "HeatBoiler"))
                        .addInsert(NodeFactory.createURI(KB + "HeatGeneratorBoil5"), NodeFactory.createURI(RDFS_LABEL), "Boil5")
                        .addInsert(NodeFactory.createURI(KB + "MunicipalUtility"), NodeFactory.createURI(OC_isOwnerOf), NodeFactory.createURI(KB + "HeatGeneratorBoil6"))
                        .addInsert(NodeFactory.createURI(KB + "HeatGeneratorBoil6"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "HeatBoiler"))
                        .addInsert(NodeFactory.createURI(KB + "HeatGeneratorBoil6"), NodeFactory.createURI(RDFS_LABEL), "Boil6");
        UpdateRequest MunicipalUtility_ur = MunicipalUtility_ub.buildRequest();
        AccessAgentCaller.updateStore(sparqlendpoint, MunicipalUtility_ur.toString());


        // For HeatGenerator Part
        HeatGeneratorUpdate("HeatGeneratorGT", Value_ThermalLoad_GT, Value_HCV_GT, Value_LCV_GT, Value_CO2Factor_GT);
        HeatGeneratorUpdate("HeatGeneratorBoil4", Value_ThermalLoad_HB4, Value_HCV_HB4, Value_LCV_HB4, Value_CO2Factor_HB4);
        HeatGeneratorUpdate("HeatGeneratorBoil5", Value_ThermalLoad_HB5, Value_HCV_HB5, Value_LCV_HB5, Value_CO2Factor_HB5);
        HeatGeneratorUpdate("HeatGeneratorBoil6", Value_ThermalLoad_HB6, Value_HCV_HB6, Value_LCV_HB6, Value_CO2Factor_HB6);


        // For the IncinerationPlant part
        UpdateBuilder IncinerationPlant_ub =
                new UpdateBuilder()    
                        .addInsert(NodeFactory.createURI(KB + "IncinerationPlant"), NodeFactory.createURI(OHN + "hasProvidedHeatAmount"), NodeFactory.createURI(KB + "ProvidedHeatAmount"))
                        .addInsert(NodeFactory.createURI(KB + "ProvidedHeatAmount"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "ProvidedHeatAmount"))
                        .addInsert(NodeFactory.createURI(KB + "IncinerationPlant"), NodeFactory.createURI(OHN + "hasMaxHourlySupply"), NodeFactory.createURI(KB + "MaxPower" + "IncinerationPlant"))
                        .addInsert(NodeFactory.createURI(KB + "IncinerationPlant"), NodeFactory.createURI(OHN + "hasMinHourlySupply"), NodeFactory.createURI(KB + "MinPower" + "IncinerationPlant"))
                        .addInsert(NodeFactory.createURI(KB + "MaxPower" + "IncinerationPlant"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_POWER))
                        .addInsert(NodeFactory.createURI(KB + "MinPower" + "IncinerationPlant"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_POWER));
        UpdateRequest IncinerationPlant_ur = IncinerationPlant_ub.buildRequest();
        AccessAgentCaller.updateStore(sparqlendpoint, IncinerationPlant_ur.toString());


        // For GasTurbine part
        UpdateBuilder GasTurbine_ub =
                new UpdateBuilder()
                        .addInsert(NodeFactory.createURI(KB + "HeatGeneratorGT"), NodeFactory.createURI(OHN + "hasRatedElectricalPower"), NodeFactory.createURI(KB + "ElectricalPower" + "HeatGeneratorGT"))
                        .addInsert(NodeFactory.createURI(KB + "HeatGeneratorGT"), NodeFactory.createURI(OHN + "hasMinimumThermalLoad"), NodeFactory.createURI(KB + "MinThermalLoad" + "HeatGeneratorGT"))
                        .addInsert(NodeFactory.createURI(KB + "ElectricalPower" + "HeatGeneratorGT"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_POWER))
                        .addInsert(NodeFactory.createURI(KB + "MinThermalLoad" + "HeatGeneratorGT"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_POWER))
                        .addInsert(NodeFactory.createURI(KB + "HeatGeneratorGT"), NodeFactory.createURI(OHN + "hasMinimumIdleTime"), NodeFactory.createURI(KB + "IdleTime" + "HeatGeneratorGT"))
                        .addInsert(NodeFactory.createURI(KB + "IdleTime" + "HeatGeneratorGT"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_DURATION))
                        .addInsert(NodeFactory.createURI(KB + "HeatGeneratorGT"), NodeFactory.createURI(OHN + "hasCoGenElectricityAmount"), NodeFactory.createURI(KB + "CoGenElectricityAmount" + "HeatGeneratorGT"))
                        .addInsert(NodeFactory.createURI(KB + "CoGenElectricityAmount" + "HeatGeneratorGT"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "CoGenElectricityAmount"));
        UpdateRequest GasTurbine_ur = GasTurbine_ub.buildRequest();
        AccessAgentCaller.updateStore(sparqlendpoint, GasTurbine_ur.toString());
        omHasValueNonTS("ElectricalPower" + "HeatGeneratorGT", "megawatt", Value_RatedElectricalPower);
        omHasValueNonTS("MinThermalLoad" + "HeatGeneratorGT", "megawatt", Value_MinimumThermalLoad);
        omHasValueNonTS("IdleTime" + "HeatGeneratorGT", "hour-Sidereal", Value_MinimumIdleTime);


        // For Contract part
        UpdateBuilder Contract_ub =
                new UpdateBuilder()
                        .addInsert(NodeFactory.createURI(KB + "Contract"), NodeFactory.createURI(RDFS_LABEL), "Contract")
                        .addInsert(NodeFactory.createURI(KB + "Contract"), NodeFactory.createURI(OHN + "isFulfilledBy"), NodeFactory.createURI(KB + "IncinerationPlant"))
                        .addInsert(NodeFactory.createURI(KB + "Contract"), NodeFactory.createURI(OHN + "hasMinAnnualPurchaseVolume"), NodeFactory.createURI(KB + "MinPurchase"))
                        .addInsert(NodeFactory.createURI(KB + "Contract"), NodeFactory.createURI(OHN + "hasMaxAnnualPurchaseVolume"), NodeFactory.createURI(KB + "MaxPurchase"))
                        .addInsert(NodeFactory.createURI(KB + "MinPurchase"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_Energy))
                        .addInsert(NodeFactory.createURI(KB + "MaxPurchase"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_Energy))
                        .addInsert(NodeFactory.createURI(KB + "Contract"), NodeFactory.createURI(OHN + "hasTieredUnitPrice"), NodeFactory.createURI(KB + "TieredUnitPrice"))
                        .addInsert(NodeFactory.createURI(KB + "TieredUnitPrice"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "TieredUnitPrice"))
                        .addInsert(NodeFactory.createURI(KB + "TieredUnitPrice"), NodeFactory.createURI(OHN + "hasTier"), NodeFactory.createURI(KB + "Tier_1"))
                        .addInsert(NodeFactory.createURI(KB + "Tier_1"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "Tier"))
                        .addInsert(NodeFactory.createURI(KB + "TieredUnitPrice"), NodeFactory.createURI(OHN + "hasTier"), NodeFactory.createURI(KB + "Tier_2"))
                        .addInsert(NodeFactory.createURI(KB + "Tier_2"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "Tier"))
                        .addInsert(NodeFactory.createURI(KB + "Contract"), NodeFactory.createURI(OHN + "hasCurrentUnitPrice"), NodeFactory.createURI(KB + "UnitPrice_1"))
                        .addInsert(NodeFactory.createURI(KB + "UnitPrice_1"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "UnitPrice"))
                        .addInsert(NodeFactory.createURI(KB + "UnitPrice_2"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "UnitPrice"))
                        .addInsert(NodeFactory.createURI(KB + "Tier_1"), NodeFactory.createURI(OHN + "hasUnitPrice"), NodeFactory.createURI(KB + "UnitPrice_1"))
                        .addInsert(NodeFactory.createURI(KB + "Tier_2"), NodeFactory.createURI(OHN + "hasUnitPrice"), NodeFactory.createURI(KB + "UnitPrice_2"))
                        .addInsert(NodeFactory.createURI(KB + "Tier_1"), NodeFactory.createURI(OHN + "hasCumulativeEnergyCap"), NodeFactory.createURI(KB + "CumulativeEnergyCap_1"))
                        .addInsert(NodeFactory.createURI(KB + "Tier_2"), NodeFactory.createURI(OHN + "hasCumulativeEnergyCap"), NodeFactory.createURI(KB + "CumulativeEnergyCap_2"))
                        .addInsert(NodeFactory.createURI(KB + "CumulativeEnergyCap_1"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_Energy))
                        .addInsert(NodeFactory.createURI(KB + "CumulativeEnergyCap_2"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_Energy));
        UpdateRequest Contract_ur = Contract_ub.buildRequest();
        AccessAgentCaller.updateStore(sparqlendpoint, Contract_ur.toString());
        omHasValueNonTS("CumulativeEnergyCap_1", "megawattHour", Value_TierOneCap);
        omHasValueNonTS("CumulativeEnergyCap_2", "megawattHour", Value_TierTwoCap);
        omHasValueNonTS("UnitPrice_1", "euroPerMegawattHour", Value_TierOneUnitPrice);
        omHasValueNonTS("UnitPrice_2", "euroPerMegawattHour", Value_TierTwoUnitPrice);
        omHasValueNonTS("MinPurchase", "megawattHour", Value_MinPurchase);
        omHasValueNonTS("MaxPurchase", "megawattHour", Value_MaxPurchase);


        // For the UnitRate part
        UpdateBuilder UnitRate_ub =
                new UpdateBuilder()
                        .addInsert(NodeFactory.createURI(KB + "ElectricitySpotPrice"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "ElectricitySpotPrice"))
                        .addInsert(NodeFactory.createURI(KB + "CO2CertificatePrice"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "CO2CertificatePrice"))
                        .addInsert(NodeFactory.createURI(KB + "GasUnitCostGT"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "GasUnitCost"))
                        .addInsert(NodeFactory.createURI(KB + "GasUnitCostKessel"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "GasUnitCost"));
        UpdateRequest UnitRate_ur = UnitRate_ub.buildRequest();
        AccessAgentCaller.updateStore(sparqlendpoint, UnitRate_ur.toString());

        String mappingFolder = System.getenv("MAPPINGSPATH");
        try {
            readMappings(mappingFolder);
        } catch (java.io.IOException e) {
            e.printStackTrace();
        }

        for (JSONKeyToIRIMapper mapping : mappings) {
        		WaermeleistungKessel4=mapping.getIRI("WaermeleistungKessel4");		
        		WaermeleistungKessel5=mapping.getIRI("WaermeleistungKessel5");	
        		WaermeleistungKessel6=mapping.getIRI("WaermeleistungKessel6");				
        		CO2Preis=mapping.getIRI("CO2Preis");		
        		GTWaermeleistung=mapping.getIRI("GTWaermeleistung");	
        		GaspreisGT=mapping.getIRI("GaspreisGT");	
        		WaermemengeInnenstadt=mapping.getIRI("WaermemengeInnenstadt");	
        		TempRuecklauf=mapping.getIRI("TempRuecklauf");	
        		MHKWTempVorlauf=mapping.getIRI("MHKWTempVorlauf");		
        		TempVorlauf=mapping.getIRI("TempVorlauf");			
        		Spotpreis=mapping.getIRI("Spotpreis");		
        		WaermemengeMHKW=mapping.getIRI("WaermemengeMHKW");		
        		GaspreisKessel=mapping.getIRI("GaspreisKessel");	
        		Aussentemperatur=mapping.getIRI("Aussentemperatur");			
        		MHKWTempRuecklauf=mapping.getIRI("MHKWTempRuecklauf");
        		GasverbrauchGT=mapping.getIRI("GasverbrauchGT");
        		GasbezugKessel4=mapping.getIRI("GasbezugKessel4");
        		GasbezugKessel5=mapping.getIRI("GasbezugKessel5");
        		GasbezugKessel6=mapping.getIRI("GasbezugKessel6");
        		GTActive=mapping.getIRI("GTActive");
        		Kessel4Active=mapping.getIRI("Kessel4Active");
        		Kessel5Active=mapping.getIRI("Kessel5Active");
        		Kessel6Active=mapping.getIRI("Kessel6Active");
        		CHPBonus=mapping.getIRI("CHPBonus");
        		Gridcharges=mapping.getIRI("Gridcharges");
        		HourlyLabourCostBoiler=mapping.getIRI("HourlyLabourCostBoiler");
        		HourlyLabourCostGT=mapping.getIRI("HourlyLabourCostGT");
        		HourlyWearCostBoiler=mapping.getIRI("HourlyWearCostBoiler");
        		HourlyWearCostGT=mapping.getIRI("HourlyWearCostGT");
        		DemandDrivenWearCostBoiler=mapping.getIRI("DemandDrivenWearCostBoiler");
        		DemandDrivenWearCostGT=mapping.getIRI("DemandDrivenWearCostGT");
        		PressureRuecklauf=mapping.getIRI("PressureRuecklauf");
        		MHKWPressureV=mapping.getIRI("MHKWPressureV");
        		PressureVorlauf=mapping.getIRI("PressureVorlauf");
        		MHKWPressureRuecklauf=mapping.getIRI("MHKWPressureRuecklauf");
        		MinHourlySupply=mapping.getIRI("MinHourlySupply");
        		MaxHourlySupply=mapping.getIRI("MaxHourlySupply");
        		AvaiIn=mapping.getIRI("AvaiIn");
        		AvaiGT=mapping.getIRI("AvaiGT");
        		AvaiKessel4=mapping.getIRI("AvaiKessel4");
        		AvaiKessel5=mapping.getIRI("AvaiKessel5");
        		AvaiKessel6=mapping.getIRI("AvaiKessel6");
        		IsHoliday=mapping.getIRI("IsHoliday");
        		IsVacation=mapping.getIRI("IsVacation");
        		GTWirkleistung=mapping.getIRI("GTWirkleistung");
        }

        UpdateBuilder TSIRI_ub =
        		new UpdateBuilder()
        		.addInsert(NodeFactory.createURI(KB + "EnergyInTimeIntervalHA" +"HeatGeneratorBoil4"), NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(WaermeleistungKessel4))
        		.addInsert(NodeFactory.createURI(KB + "EnergyInTimeIntervalHA" +"HeatGeneratorBoil5"), NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(WaermeleistungKessel5))
        		.addInsert(NodeFactory.createURI(KB + "EnergyInTimeIntervalHA" +"HeatGeneratorBoil6"), NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(WaermeleistungKessel6))
        		.addInsert(NodeFactory.createURI(KB + "CO2CertificatePrice"), NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(CO2Preis))
        		.addInsert(NodeFactory.createURI(KB + "EnergyInTimeIntervalHA" +"HeatGeneratorGT"), NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(GTWaermeleistung))
        		.addInsert(NodeFactory.createURI(KB + "GasUnitCostGT"), NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(GaspreisGT))
        		.addInsert(NodeFactory.createURI(KB + "HeatDemand"), NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(WaermemengeInnenstadt))
        		.addInsert(NodeFactory.createURI(KB + "UPMunicipal" + "Temperature"), NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(TempRuecklauf))
        		.addInsert(NodeFactory.createURI(KB + "DownEfW" + "Temperature"), NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(MHKWTempVorlauf))
        		.addInsert(NodeFactory.createURI(KB + "DownMunicipal" + "Temperature"), NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(TempVorlauf))
        		.addInsert(NodeFactory.createURI(KB + "ElectricitySpotPrice"), NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(Spotpreis))
        		.addInsert(NodeFactory.createURI(KB + "ProvidedHeatAmount"), NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(WaermemengeMHKW))
        		.addInsert(NodeFactory.createURI(KB + "GasUnitCostKessel"), NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(GaspreisKessel))
        		.addInsert(NodeFactory.createURI(KB + "AirTemperature"), NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(Aussentemperatur))
        		.addInsert(NodeFactory.createURI(KB + "UPEfW" + "Temperature"), NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(MHKWTempRuecklauf))
        		.addInsert(NodeFactory.createURI(KB + "EnergyInTimeIntervalGA" + "HeatGeneratorGT"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(GasverbrauchGT))
                .addInsert(NodeFactory.createURI(KB + "EnergyInTimeIntervalGA" + "HeatGeneratorBoil4"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(GasbezugKessel4))
                .addInsert(NodeFactory.createURI(KB + "EnergyInTimeIntervalGA" + "HeatGeneratorBoil5"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(GasbezugKessel5))
                .addInsert(NodeFactory.createURI(KB + "EnergyInTimeIntervalGA" + "HeatGeneratorBoil6"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(GasbezugKessel6))
                .addInsert(NodeFactory.createURI(KB + "UnitRate" + "CHPBonus"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(CHPBonus))
                .addInsert(NodeFactory.createURI(KB + "UnitRate" + "CHPBonus"),NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "CHPBonus"))
                .addInsert(NodeFactory.createURI(KB + "UnitRate" + "Gridcharges"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(Gridcharges))
                .addInsert(NodeFactory.createURI(KB + "UnitRate" + "Gridcharges"),NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "GridCharges"))    
                .addInsert(NodeFactory.createURI(KB + "UnitRate" + "HourlyLabourCostBoiler"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(HourlyLabourCostBoiler))
                .addInsert(NodeFactory.createURI(KB + "UnitRate" + "HourlyLabourCostBoiler"),NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "HourlyLabourCost"))   
                .addInsert(NodeFactory.createURI(KB + "UnitRate" + "HourlyLabourCostGT"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(HourlyLabourCostGT))
                .addInsert(NodeFactory.createURI(KB + "UnitRate" + "HourlyLabourCostGT"),NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "HourlyLabourCost" ))
                .addInsert(NodeFactory.createURI(KB + "UnitRate" + "HourlyWearCostBoiler"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(HourlyWearCostBoiler))
                .addInsert(NodeFactory.createURI(KB + "UnitRate" + "HourlyWearCostBoiler"),NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "HourlyWearCost"))
                .addInsert(NodeFactory.createURI(KB + "UnitRate" + "HourlyWearCostGT"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(HourlyWearCostGT))
                .addInsert(NodeFactory.createURI(KB + "UnitRate" + "HourlyWearCostGT"),NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "HourlyWearCost"))
                .addInsert(NodeFactory.createURI(KB + "UnitRate" + "DemandDrivenWearCostBoiler"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(DemandDrivenWearCostBoiler))
                .addInsert(NodeFactory.createURI(KB + "UnitRate" + "DemandDrivenWearCostBoiler"),NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "DemandDrivenWearCost"))
                .addInsert(NodeFactory.createURI(KB + "UnitRate" + "DemandDrivenWearCostGT"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(DemandDrivenWearCostGT))
                .addInsert(NodeFactory.createURI(KB + "UnitRate" + "DemandDrivenWearCostGT"),NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "DemandDrivenWearCost"))  
                .addInsert(NodeFactory.createURI(KB + "UPMunicipal" + "Pressure"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(PressureRuecklauf))
                .addInsert(NodeFactory.createURI(KB + "DownEfW" + "Pressure"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(MHKWPressureV))
                .addInsert(NodeFactory.createURI(KB + "DownMunicipal" + "Pressure"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(PressureVorlauf))
                .addInsert(NodeFactory.createURI(KB + "UPEfW" + "Pressure"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(MHKWPressureRuecklauf))   
                .addInsert(NodeFactory.createURI(KB + "MinPower" + "IncinerationPlant"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(MinHourlySupply))
                .addInsert(NodeFactory.createURI(KB + "MaxPower" + "IncinerationPlant"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(MaxHourlySupply))
        		.addInsert(NodeFactory.createURI(KB + "DurationInTimeInterval" + "HeatGeneratorGT"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(GTActive))
                .addInsert(NodeFactory.createURI(KB + "DurationInTimeInterval" + "HeatGeneratorBoil4"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(Kessel4Active))
                .addInsert(NodeFactory.createURI(KB + "DurationInTimeInterval" + "HeatGeneratorBoil5"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI( Kessel5Active))
                .addInsert(NodeFactory.createURI(KB + "DurationInTimeInterval" + "HeatGeneratorBoil6"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(Kessel6Active))    
                .addInsert(NodeFactory.createURI(KB + "CoGenElectricityAmount" + "HeatGeneratorGT"),NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(GTWirkleistung))
                .addInsert(NodeFactory.createURI(KB + "IncinerationPlant"), NodeFactory.createURI(OHN + "hasOperatingAvailability"), NodeFactory.createURI(AvaiIn))
                .addInsert(NodeFactory.createURI(KB + "HeatGeneratorGT"), NodeFactory.createURI(OHN + "hasOperatingAvailability"), NodeFactory.createURI(AvaiGT))
                .addInsert(NodeFactory.createURI(KB + "HeatGeneratorBoil4"), NodeFactory.createURI(OHN + "hasOperatingAvailability"), NodeFactory.createURI(AvaiKessel4))
                .addInsert(NodeFactory.createURI(KB + "HeatGeneratorBoil5"), NodeFactory.createURI(OHN + "hasOperatingAvailability"), NodeFactory.createURI(AvaiKessel5))
                .addInsert(NodeFactory.createURI(KB + "HeatGeneratorBoil6"), NodeFactory.createURI(OHN + "hasOperatingAvailability"), NodeFactory.createURI(AvaiKessel6))
                .addInsert(NodeFactory.createURI(AvaiIn), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "Availability"))
                .addInsert(NodeFactory.createURI(AvaiGT), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "Availability"))
                .addInsert(NodeFactory.createURI(AvaiKessel4), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "Availability"))
                .addInsert(NodeFactory.createURI(AvaiKessel5), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "Availability"))
                .addInsert(NodeFactory.createURI(AvaiKessel6), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "Availability"));
        UpdateRequest TSIRI_ur = TSIRI_ub.buildRequest();
        AccessAgentCaller.updateStore(sparqlendpoint, TSIRI_ur.toString());
        
        
        omHasValueTS("megawattHour", WaermeleistungKessel4);
        omHasValueTS("megawattHour", WaermeleistungKessel5);
        omHasValueTS("megawattHour", WaermeleistungKessel6);
        omHasValueTS("euroPerTonne", CO2Preis);
        omHasValueTS("megawattHour", GTWaermeleistung);
        omHasValueTS("euroPerMegawattHour", GaspreisGT);
        omHasValueTS("megawattHour",WaermemengeInnenstadt);
        omHasValueTS("degreeCelsius", TempRuecklauf);
        omHasValueTS("degreeCelsius", MHKWTempVorlauf);
        omHasValueTS("degreeCelsius", TempVorlauf);
        omHasValueTS("euroPerMegawattHour", Spotpreis);
        omHasValueTS("megawattHour", WaermemengeMHKW);
        omHasValueTS("euroPerMegawattHour", GaspreisKessel); 
        omHasValueTS("degreeCelsius", Aussentemperatur);
        omHasValueTS("degreeCelsius", MHKWTempRuecklauf);    
        omHasValueTS("megawattHour", GasverbrauchGT); 
        omHasValueTS("megawattHour", GasbezugKessel4);   
        omHasValueTS("megawattHour", GasbezugKessel5);   
        omHasValueTS("megawattHour", GasbezugKessel6);   
        omHasValueTS("euroPerMegawattHour", CHPBonus);
        omHasValueTS("euroPerMegawattHour", Gridcharges);
        omHasValueTS("euroPerHour", HourlyLabourCostBoiler);
        omHasValueTS("euroPerHour", HourlyLabourCostGT);
        omHasValueTS("euroPerHour", HourlyWearCostBoiler);
        omHasValueTS("euroPerHour", HourlyWearCostGT);
        omHasValueTS("euroPerMegawattHour", DemandDrivenWearCostBoiler);
        omHasValueTS("euroPerMegawattHour", DemandDrivenWearCostGT);
        omHasValueTS("bar", PressureRuecklauf);
        omHasValueTS("bar", MHKWPressureV);
        omHasValueTS("bar", PressureVorlauf);
        omHasValueTS("bar", MHKWPressureRuecklauf);
        omHasValueTS("megawatt", MinHourlySupply);
        omHasValueTS("megawatt", MaxHourlySupply);
        omHasValueTS("megawattHour", GTWirkleistung);
        omHasValueTS("hour-Sidereal", GTActive); 
        omHasValueTS("hour-Sidereal", Kessel4Active);
        omHasValueTS("hour-Sidereal", Kessel5Active);
        omHasValueTS("hour-Sidereal", Kessel6Active);	
        
        
        // For the CalendarEffect part
        UpdateBuilder CalendarEffect_ub =
                new UpdateBuilder() 
                        .addInsert(NodeFactory.createURI(IsHoliday), NodeFactory.createURI(OHN +"applicableLocation"), NodeFactory.createURI(KB + "isPublicHoliday"+"Location_1"))
                        .addInsert(NodeFactory.createURI(IsVacation), NodeFactory.createURI(OHN +"applicableLocation"), NodeFactory.createURI(KB + "isSchoolVacation"+"Location_1"))   
                        .addInsert(NodeFactory.createURI(KB + "isPublicHoliday"+"Location_1"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI("http://purl.org/dc/terms/Location"))
                        .addInsert(NodeFactory.createURI(KB + "isPublicHoliday"+"Location_1"), NodeFactory.createURI(RDFS_LABEL), "Rheinland-Pfalz")   
                        .addInsert(NodeFactory.createURI(IsHoliday), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "isPublicHoliday"))
                        .addInsert(NodeFactory.createURI(IsVacation), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "isSchoolVacation")) 
                        .addInsert(NodeFactory.createURI(IsHoliday), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(XSD_BOOLEAN))
                        .addInsert(NodeFactory.createURI(IsVacation), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(XSD_BOOLEAN))     
                        .addInsert(NodeFactory.createURI(KB + "AirTemperature"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(EMS + "AirTemperature"));
        UpdateRequest CalendarEffect_ur = CalendarEffect_ub.buildRequest();
        AccessAgentCaller.updateStore(sparqlendpoint, CalendarEffect_ur.toString());
    }    

    // For the instance of HeatGenerator part
    public void HeatGeneratorUpdate(String HeatGenerator_instance, float Value_ThermalLoad, float Value_HCV, float Value_LCV, float Value_CO2Factor) {
        UpdateBuilder HeatGenerator_instance_ub =
                new UpdateBuilder()
                        .addInsert(NodeFactory.createURI(KB + HeatGenerator_instance), NodeFactory.createURI(OHN + "hasGeneratedHeatAmount"), NodeFactory.createURI(KB + "EnergyInTimeIntervalHA" + HeatGenerator_instance))
                        .addInsert(NodeFactory.createURI(KB + "EnergyInTimeIntervalHA" + HeatGenerator_instance), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "GeneratedHeatAmount"))
                        .addInsert(NodeFactory.createURI(KB + HeatGenerator_instance), NodeFactory.createURI(OHN + "hasConsumedGasAmount"), NodeFactory.createURI(KB + "EnergyInTimeIntervalGA" + HeatGenerator_instance)) 
                        .addInsert(NodeFactory.createURI(KB + "EnergyInTimeIntervalGA" + HeatGenerator_instance), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "ConsumedGasAmount"))
                        .addInsert(NodeFactory.createURI(KB + HeatGenerator_instance), NodeFactory.createURI(OHN + "hasOperatingTime"), NodeFactory.createURI(KB + "DurationInTimeInterval" + HeatGenerator_instance))
                        .addInsert(NodeFactory.createURI(KB + "DurationInTimeInterval" + HeatGenerator_instance), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "DurationInTimeInterval"))
                        .addInsert(NodeFactory.createURI(KB + HeatGenerator_instance), NodeFactory.createURI(OHN + "hasRatedThermalPower"), NodeFactory.createURI(KB + "ThermalLoad" + HeatGenerator_instance))
                        .addInsert(NodeFactory.createURI(KB + "ThermalLoad" + HeatGenerator_instance), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_POWER))         
                        .addInsert(NodeFactory.createURI(KB + HeatGenerator_instance), NodeFactory.createURI(OCP_hasFuelType), NodeFactory.createURI(KB + "NaturalGas"))
                        .addInsert(NodeFactory.createURI(KB + "NaturalGas"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "NaturalGas"))
                        .addInsert(NodeFactory.createURI(KB + "NaturalGas"), NodeFactory.createURI(OHN + "hasHigherCalorificValue"), NodeFactory.createURI(KB + "HigherCalorificValue" ))
                        .addInsert(NodeFactory.createURI(KB + "HigherCalorificValue"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "HigherCalorificValue"))
                        .addInsert(NodeFactory.createURI(KB + "NaturalGas"), NodeFactory.createURI(OHN + "hasLowerCalorificValue"), NodeFactory.createURI(KB + "LowerCalorificValue"))
                        .addInsert(NodeFactory.createURI(KB + "LowerCalorificValue"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "LowerCalorificValue"))
                        .addInsert(NodeFactory.createURI(KB + "NaturalGas"), NodeFactory.createURI(OHN + "hasCO2Factor"), NodeFactory.createURI(KB + "NaturalGasCO2Factor"))
                        .addInsert(NodeFactory.createURI(KB + "NaturalGasCO2Factor"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OHN + "CO2Factor"));
        UpdateRequest HeatGenerator_instance_ur = HeatGenerator_instance_ub.buildRequest();
        AccessAgentCaller.updateStore(sparqlendpoint, HeatGenerator_instance_ur.toString());
        omHasValueNonTS("ThermalLoad" + HeatGenerator_instance, "megawatt", Value_ThermalLoad);
        omHasValueNonTS("HigherCalorificValue", "kilowattHourPerCubicMetre", Value_HCV);
        omHasValueNonTS("LowerCalorificValue", "kilowattHourPerCubicMetre", Value_LCV);
        omHasValueNonTS("NaturalGasCO2Factor", "tonnePerMegawattHour", Value_CO2Factor);
    }


    // Update the non time-series triples part
    public void omHasValueNonTS(String Instance, String Unit, float NumericalValue) {
        UpdateBuilder omHasValueNonTS_ub =
                new UpdateBuilder()
                        .addInsert(NodeFactory.createURI(KB + Instance), NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(KB + "Measure" + Instance))
                        .addInsert(NodeFactory.createURI(KB + "Measure" + Instance), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_MEASURE))
                        .addInsert(NodeFactory.createURI(KB + "Measure" + Instance), NodeFactory.createURI(OM_HAS_UNIT), NodeFactory.createURI(OM + Unit))
    //                  .addInsert(NodeFactory.createURI(KB + Unit), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_UNIT))
                        .addInsert(NodeFactory.createURI(KB + "Measure" + Instance), NodeFactory.createURI(OM_Has_NUMERICAL_VALUE), NumericalValue);
        UpdateRequest omHasValueNonTS_ur = omHasValueNonTS_ub.buildRequest();
        AccessAgentCaller.updateStore(sparqlendpoint, omHasValueNonTS_ur.toString());
    }


    // Update the time-series triples part
    public void omHasValueTS(String Unit, String TSIRI) {
        UpdateBuilder omHasValueTS_ub =
                new UpdateBuilder()
                        .addInsert(NodeFactory.createURI(TSIRI), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_MEASURE))
                        .addInsert(NodeFactory.createURI(TSIRI), NodeFactory.createURI(OM_HAS_UNIT), NodeFactory.createURI(OM + Unit));
    //                    .addInsert(NodeFactory.createURI(KB + Unit), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_UNIT))
        UpdateRequest omHasValueTS_ur = omHasValueTS_ub.buildRequest();
        AccessAgentCaller.updateStore(sparqlendpoint, omHasValueTS_ur.toString());
    }

   // Setter for the time series client.
   // @param tsClient The time series client to use.
    public void setTsClient(TimeSeriesClient<OffsetDateTime> tsClient) {
        this.tsClient = tsClient;
    }

    // Update the TS database with new readings.
    public void updateTSData(Map<String, List<?>> HeatingNetworkReadings) throws IllegalArgumentException {
        if (!HeatingNetworkReadings.isEmpty()) {
            List<TimeSeries<OffsetDateTime>> timeSeries;
            try {
                timeSeries = convertReadingsToTimeSeries(HeatingNetworkReadings);
            } catch (NoSuchElementException e) {
                throw new IllegalArgumentException("Readings can not be converted to proper time series!", e);
            }
            for (TimeSeries<OffsetDateTime> ts : timeSeries) {
                if (!ts.getTimes().isEmpty()) {
                    try {
                        tsClient.addTimeSeriesData(ts);
                    } catch (Exception e) {
                        e.getMessage();
                       throw new JPSRuntimeException("Cannot not add timeseries!");
                    } 
                }
            }
        } else {
            throw new IllegalArgumentException("Readings can not be empty!");
        }
    }


    // Converts the readings in form of maps to time series' using the mappings to IRI.
    // @return A list of time series objects (one per mapping) that can be used with the time series client.
    private List<TimeSeries<OffsetDateTime>> convertReadingsToTimeSeries(Map<String, List<?>> HeatingNetworkReadings)
            throws NoSuchElementException, IOException {
        List<OffsetDateTime> HeatingNetworkTimestamps = HeatingNetworkReadings.get(HeatNetworkInputAgent.timestampKey).stream()
                .map(timestamp -> (convertStringToOffsetDateTime(timestamp.toString()))).collect(Collectors.toList());
        List<TimeSeries<OffsetDateTime>> timeSeries = new ArrayList<>();
        String mappingFolder = System.getenv("MAPPINGSPATH");
        try {
            readMappings(mappingFolder);
        } catch (java.io.IOException e) {
            e.printStackTrace();
        }

        for (JSONKeyToIRIMapper mapping : mappings) {
            List<String> iris = new ArrayList<>();
            List<List<?>> values = new ArrayList<>();
            for (String key : mapping.getAllJSONKeys()) {
                iris.add(mapping.getIRI(key));
                if (HeatingNetworkReadings.containsKey(key)) {
                    values.add(HeatingNetworkReadings.get(key));
                } else {
                    throw new NoSuchElementException("The key " + key + " is not contained in the readings!");
                }
            }
            List<OffsetDateTime> times = HeatingNetworkTimestamps;
            TimeSeries<OffsetDateTime> currentTimeSeries = new TimeSeries<>(times, iris, values);
            timeSeries.add(currentTimeSeries);
        }
        return timeSeries;
    }



    // Converts a string into a datetime object with zone information using the zone globally define for the agent.
    // The format should be equal to 2008-12-03T10:15:30.
    // Return The resulting datetime object.
    private OffsetDateTime convertStringToOffsetDateTime(String timestamp) {
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss");
        LocalDateTime localTime = LocalDateTime.parse(timestamp, dtf);
        return OffsetDateTime.of(localTime, ZoneOffset.UTC);
    }


    // Reads the JSON key to IRI mappings from files in the provided folder
    // MappingFolder The path to the folder in which the mapping files are located
    private static void readMappings(String mappingFolder) throws IOException, java.io.IOException {
        mappings = new ArrayList<>();
        File folder = new File(mappingFolder);
        File[] mappingFiles = folder.listFiles();
        if (mappingFiles == null) {
            throw new IOException("Folder does not exist: " + mappingFolder, null);
        }
        if (mappingFiles.length == 0) {
            throw new IOException("No files in the folder: " + mappingFolder, null);
        } else {
            for (File mappingFile : mappingFiles) {
                JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(HeatNetworkInputAgent.generatedIRIPrefix, mappingFile.getAbsolutePath());
                mappings.add(mapper);
                mapper.saveToFile(mappingFile.getAbsolutePath());
            }
        }
    }


    // Initializes all time series maintained by the agent (represented by the key to IRI mappings) if they do no exist
    // using the time series client.
    public void initializeTimeSeriesIfNotExist() {
        //String mappingFolder = "/Users/HXUE01/Documents/GitHub/TheWorldAvatar/Agents/DistrictHeatingNetworkAgent/config/mappings";
        String mappingFolder = System.getenv("MAPPINGSPATH");
        try {
            readMappings(mappingFolder);
        } catch (java.io.IOException e) {
            e.printStackTrace();
        }

        for (JSONKeyToIRIMapper mapping : mappings) {
            List<String> iris = mapping.getAllIRIs();
            if (!timeSeriesExist(iris)) {
                List<Class<?>> classes = iris.stream().map(this::getClassFromJSONKey).collect(Collectors.toList());
                try {
                    tsClient.initTimeSeries(iris, classes, timeUnit);
                    LOGGER.info(String.format("Initialized time series with the following IRIs: %s", String.join(", ", iris)));
                } catch (Exception e) {
                    throw new JPSRuntimeException("Could not initialize timeseries!");
                } 
            }
        }
    }


    // Checks whether a time series exists by checking whether any of the IRIs that should be attached to
    // the time series is not initialised in the central RDB lookup table using the time series client
    // The IRIs that should be attached to the same time series provided as list of strings
    // Return true if all IRIs have a time series attached, false otherwise
    private boolean timeSeriesExist(List<String> iris) {
        for (String iri : iris) {
            try {
                if (!tsClient.checkDataHasTimeSeries(iri)) {
                    return false;
                }
            } catch (DataAccessException e) {
                if (e.getMessage().contains("ERROR: relation \"dbTable\" does not exist")) {
                    return false;
                } else {
                    throw e;
                }
            } 
        }
        return true;
    }


    // Returns the class (datatype) corresponding to a JSON key
    // Return The corresponding class as Class<?> object.
    private Class<?> getClassFromJSONKey(String jsonKey) {
        if (jsonKey.contains("sample1") || jsonKey.contains("sample2") || jsonKey.contains("sample3")) {
            return Double.class;
        } else if (jsonKey.contains(timestampKey)) {
            return String.class;
        } else {
            return Double.class;
        }
    }

    // Extract the column from an input csv file
    public static String[] ReadColInput(int col, String filepath, String delimiter) {
        String currentLine;
        String[] data;
        ArrayList<String> colData = new ArrayList<String>();

        try {
            FileReader fr = new FileReader(filepath);
            BufferedReader br = new BufferedReader(fr);
            while ((currentLine = br.readLine()) != null) {
                data = currentLine.split(delimiter);
                colData.add(data[col]);
            }
        } catch (Exception e) {
            System.out.println(e);
            return null;
        }
        return colData.toArray(new String[0]);
    }
}