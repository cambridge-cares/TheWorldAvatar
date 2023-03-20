package uk.ac.cam.cares.jps.agent.historicalntuenergy;
import com.hp.hpl.jena.graph.Triple;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Iterator;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;


import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import java.util.List;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;


public class HistoricalQueryBuilder {

    private static final Logger LOGGER = LogManager.getLogger(HistoricalNTUEnergyAgentLauncher.class);

    /**
     * Prefixes
     */
    public static final String PowsysPrefix = "https://www.theworldavatar.com/kg/ontopowsys/";
    public static final String OmPrefix = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    public static final String generatedIRIPrefix = OmPrefix + "measure";
    public String queryEndpoint;
    public String updateEndpoint;

    /**
     * Classes
     */
    public static final String powerSystem = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PowerSystem";
    public static final String powerSystemModel = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#PowerSystemModel";
    public static final String busNode = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#BusNode";
    public static final String powerGenerator = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PowerGenerator";
    public static final String branch = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#Branch";
    public static final String absorbedActivePower = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedActivePower";
    public static final String absorbedReactivePower = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedReactivePower";


    /**
     * Model variables
     */
    public static final String PowerSystemModelPrefix = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#";



    // Busnode model variables
    public static final String BusNumber = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusNumber";
    public static final String BusType = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusType";
    public static final String Area = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Area";
    public static final String Bs = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Bs";
    public static final String Gs = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Gs";
    public static final String BaseKV = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#baseKV";
    public static final String VmMin = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#VmMin";
    public static final String VmMax = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#VmMax";
    public static final String Zone = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Zone";
    public static final String Vm = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Vm";
    public static final String Va = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Va";

    //PowerGenerator model variables
    public static final String QC2Max = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QC2Max";
    public static final String genCostn = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#genCostn";
    public static final String QC1Max = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QC1Max";
    public static final String genCostc0 = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#genCostc0";
    public static final String QMax = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QMax";
    public static final String StopCost = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#StopCost";
    public static final String Pc2 = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Pc2";
    public static final String QMin = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QMin";
    public static final String Status = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Status";
    public static final String PMin = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#PMin";
    public static final String CostModel = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#CostModel";
    public static final String PMax = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#PMax";
    public static final String Rampagc = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Rampagc";
    public static final String mBase = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#mBase";
    public static final String StartCost = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#StartCost";
    public static final String genCostcn_1 = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#genCostcn-1";
    public static final String GeneratedActivePower = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#GeneratedActivePower";
    public static final String GeneratedReactivePower = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#GeneratedReactivePower";
    public static final String Ramp10 = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Ramp10";
    public static final String Pg = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Pg";
    public static final String Qg = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Qg";
    public static final String QC1Min = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QC1Min";

    // public static final String BusNumber = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusNumber";

    public static final String Vg = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Vg";
    public static final String Ramp30 = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Ramp30";
    public static final String APF = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#APF";
    public static final String genCostcn_2 = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#genCostcn-2";
    public static final String Pc1 = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Pc1";
    public static final String QC2Min = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QC2Min";



    public static final String modelVariableSpecification = "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#ModelVariableSpecification";
    public static final String Measure = "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure";



    /**
     * Relationships
     */
    private static final String OMHasValue = OmPrefix + "hasValue";
    private static final String OntoCapeHasValue = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue";
    private static final String hasActivePowerAbsorbed = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasActivePowerAbsorbed";
    private static final String hasReactivePowerAbsorbed = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasReactivePowerAbsorbed";

    private static final String hasUnit = OmPrefix + "hasUnit";
    private static final String hasSubSystem = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem";
    private static final String isModeledBy = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isModeledBy";
    private static final String hasModelVariable = "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable";
    private static final String numericalValue = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue";
    private static final String hasUnitOfMeasure = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure";

    /**
     * Individuals
     */
    private static final String kilowatt = OmPrefix + "kilowatt";
    private static final String kilovoltamperereactive = OmPrefix + "kilovoltamperereactive";
    private static final String OntoCapekV = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kV";
    private static final String OntoCapeDegree = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree";


    /**
     * NTU building to Bus number mapping
     */
    HashMap<String, Integer> NTUBuildingToBusNum = new HashMap<>();

    /**
     * A List of HashMap for branches
     */
    HashMap<String, List<String>> branchParams = new HashMap<String, List<String>>();

    /**
     * A List of HashMap for buses
     */
    HashMap<String, List<String>> busParams = new HashMap<String, List<String>>();

    /**
     * HashMaps for model variables
     */
    HashMap<String, String> BusNodeModelVariables = new HashMap<>();
    HashMap<String, String> BranchModelVariables = new HashMap<>();
    HashMap<String, String> GeneratorModelVariables = new HashMap<>();


    RemoteStoreClient kbClient;
    public String agentProperties;
    public String clientProperties;
    public JSONObject readings;

    private List<JSONKeyToIRIMapper> mappings;

    public HistoricalQueryBuilder(String agentProp, String clientProp) throws IOException
    {
        agentProperties = agentProp;
        clientProperties = clientProp;

        loadconfigs(clientProperties);
        //readings endpoints from client.properties

        loadproperties(agentProperties);

        NTUBuildingToBusNum.put("None", 1);
        NTUBuildingToBusNum.put("N1_3", 2);
        NTUBuildingToBusNum.put("NYA", 3);
        NTUBuildingToBusNum.put("CANTEEN_2", 4);
        NTUBuildingToBusNum.put("None", 5);
        NTUBuildingToBusNum.put("SBS", 6);
        NTUBuildingToBusNum.put("EMB", 7);
        NTUBuildingToBusNum.put("RTP", 8);
        NTUBuildingToBusNum.put("N_2", 9);
        NTUBuildingToBusNum.put("N_2_1", 10);
        NTUBuildingToBusNum.put("SPMS", 11);
        NTUBuildingToBusNum.put("HALL_4", 12);
        NTUBuildingToBusNum.put("PIONEER_HALL", 13);
        NTUBuildingToBusNum.put("NEC", 14);
        NTUBuildingToBusNum.put("THE_WAVE", 15);

        //BusNode model variables
        BusNodeModelVariables.put("BusNumber", PowerSystemModelPrefix + "BusNumber");
        BusNodeModelVariables.put("BusType", PowerSystemModelPrefix + "BusType");
        BusNodeModelVariables.put("Area", PowerSystemModelPrefix + "Area");
        BusNodeModelVariables.put("Bs", PowerSystemModelPrefix + "Bs");
        BusNodeModelVariables.put("Gs", PowerSystemModelPrefix + "Gs");
        BusNodeModelVariables.put("BaseKV", PowerSystemModelPrefix + "BaseKV");
        BusNodeModelVariables.put("VmMin", PowerSystemModelPrefix + "VmMin");
        BusNodeModelVariables.put("VmMax", PowerSystemModelPrefix + "VmMax");
        BusNodeModelVariables.put("Zone", PowerSystemModelPrefix + "Zone");
        BusNodeModelVariables.put("Vm", PowerSystemModelPrefix + "Vm");
        BusNodeModelVariables.put("Va", PowerSystemModelPrefix + "Va");
        //PowerGenerator model variables
        GeneratorModelVariables.put("BusNumber", PowerSystemModelPrefix + "BusNumber");
        GeneratorModelVariables.put("QC2Max", PowerSystemModelPrefix + "QC2Max");
        GeneratorModelVariables.put("genCostn", PowerSystemModelPrefix + "genCostn");
        GeneratorModelVariables.put("QC1Max", PowerSystemModelPrefix + "QC1Max");
        GeneratorModelVariables.put("genCostc0", PowerSystemModelPrefix + "genCostc0");
        GeneratorModelVariables.put("QMax", PowerSystemModelPrefix + "QMax");
        GeneratorModelVariables.put("StopCost", PowerSystemModelPrefix + "StopCost");
        GeneratorModelVariables.put("Pc2", PowerSystemModelPrefix + "Pc2");
        GeneratorModelVariables.put("QMin", PowerSystemModelPrefix + "QMin");
        GeneratorModelVariables.put("Status", PowerSystemModelPrefix + "Status");
        GeneratorModelVariables.put("PMin", PowerSystemModelPrefix + "PMin");
        GeneratorModelVariables.put("CostModel", PowerSystemModelPrefix + "CostModel");
        GeneratorModelVariables.put("PMax", PowerSystemModelPrefix + "PMax");
        GeneratorModelVariables.put("Rampagc", PowerSystemModelPrefix + "Rampagc");
        GeneratorModelVariables.put("mBase", PowerSystemModelPrefix + "mBase");
        GeneratorModelVariables.put("StartCost", PowerSystemModelPrefix + "StartCost");
        GeneratorModelVariables.put("GeneratedActivePower", PowerSystemModelPrefix + "GeneratedActivePower");
        GeneratorModelVariables.put("GeneratedReactivePower", PowerSystemModelPrefix + "GeneratedReactivePower");
        GeneratorModelVariables.put("Ramp10", PowerSystemModelPrefix + "Ramp10");
        GeneratorModelVariables.put("Pg", PowerSystemModelPrefix + "Pg");
        GeneratorModelVariables.put("Qg", PowerSystemModelPrefix + "Qg");
        GeneratorModelVariables.put("Vg", PowerSystemModelPrefix + "Vg");
        GeneratorModelVariables.put("Ramp30", PowerSystemModelPrefix + "Ramp30");
        GeneratorModelVariables.put("QC1Min", PowerSystemModelPrefix + "Ramp30");
        GeneratorModelVariables.put("APF", PowerSystemModelPrefix + "APF");
        GeneratorModelVariables.put("genCostcn-2", PowerSystemModelPrefix + "genCostcn-2");
        GeneratorModelVariables.put("QC2Min", PowerSystemModelPrefix + "QC2Min");
        //Branch model variables
        BranchModelVariables.put("QAverage", PowerSystemModelPrefix + "QAverage");
        BranchModelVariables.put("RateB", PowerSystemModelPrefix + "RateB");
        BranchModelVariables.put("RateC", PowerSystemModelPrefix + "RateC");
        BranchModelVariables.put("RatioCoefficient", PowerSystemModelPrefix + "RatioCoefficient");
        BranchModelVariables.put("AngleMax", PowerSystemModelPrefix + "AngleMax");
        BranchModelVariables.put("PAverage", PowerSystemModelPrefix + "PAverage");
        BranchModelVariables.put("BusFrom", PowerSystemModelPrefix + "BusFrom");
        BranchModelVariables.put("X", PowerSystemModelPrefix + "X");
        BranchModelVariables.put("B", PowerSystemModelPrefix + "B");
        BranchModelVariables.put("RateA", PowerSystemModelPrefix + "RateA");
        BranchModelVariables.put("Angle", PowerSystemModelPrefix + "Angle");
        BranchModelVariables.put("AngleMin", PowerSystemModelPrefix + "AngleMin");
        BranchModelVariables.put("QLoss", PowerSystemModelPrefix + "QLoss");
        BranchModelVariables.put("SAverage", PowerSystemModelPrefix + "SAverage");
        BranchModelVariables.put("R", PowerSystemModelPrefix + "R");
        BranchModelVariables.put("BusTo", PowerSystemModelPrefix + "BusTo");
        BranchModelVariables.put("PLoss", PowerSystemModelPrefix + "PLoss");
        BranchModelVariables.put("BranchStatus", PowerSystemModelPrefix + "BranchStatus");


        branchParams.put("fbus", Arrays.asList("1", "2", "3", "4", "2", "9", "2", "6", "6", "3", "11", "12", "4", "4"));
        branchParams.put("tbus", Arrays.asList("2", "3", "4", "5", "9", "10", "6", "7", "8", "11", "12", "13", "14", "15"));
        branchParams.put("r", Arrays.asList("1.35309", "1.17024", "0.84111", "1.52348", "2.01317", "1.68671", "2.55727", "1.0882", "1.25143", "1.79553", "2.44845", "2.01317", "2.23081", "1.19702"));
        branchParams.put("x", Arrays.asList("1.32349", "1.14464", "0.82271", "1.0276", "1.3579", "1.1377", "1.7249", "0.734", "0.8441", "1.2111", "1.6515", "1.3579", "1.5047", "0.8074"));
        branchParams.put("b", Arrays.asList("0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"));
        branchParams.put("rateA", Arrays.asList("0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"));
        branchParams.put("rateB", Arrays.asList("0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"));
        branchParams.put("rateC", Arrays.asList("0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"));
        branchParams.put("ratio", Arrays.asList("0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"));
        branchParams.put("angle", Arrays.asList("0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"));
        branchParams.put("status", Arrays.asList("1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"));
        branchParams.put("angmin", Arrays.asList("-360", "-360", "-360", "-360", "-360", "-360", "-360", "-360", "-360", "-360", "-360", "-360", "-360", "-360"));
        branchParams.put("angmax", Arrays.asList("360", "360", "360", "360", "360", "360", "360", "360", "360", "360", "360", "360", "360", "360"));


        busParams.put("bus_i", Arrays.asList("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"));
        busParams.put("type", Arrays.asList("3", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"));
        busParams.put("Gs", Arrays.asList("0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"));
        busParams.put("Bs", Arrays.asList("0", "0", "1", "0", "0", "0", "0", "1", "1", "0", "0", "1", "0", "1", "0"));
        busParams.put("area", Arrays.asList("1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"));
        busParams.put("Vm", Arrays.asList("1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"));
        busParams.put("Va", Arrays.asList("0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"));
        busParams.put("baseKV", Arrays.asList("11", "11", "11", "11", "11", "11", "11", "11", "11", "11", "11", "11", "11", "11", "11"));
        busParams.put("Zone", Arrays.asList("1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"));
        busParams.put("Vmax", Arrays.asList("1", "1.1", "1.1", "1.1", "1.1", "1.1", "1.1", "1.1", "1.1", "1.1", "1.1", "1.1", "1.1", "1.1", "1.1"));
        busParams.put("Vmin", Arrays.asList("1", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9"));

        kbClient = new RemoteStoreClient();
        kbClient.setUpdateEndpoint(updateEndpoint);
        kbClient.setQueryEndpoint(queryEndpoint);
    }

    public void loadconfigs(String filepath) throws IOException
    {
        File file = new File(filepath);
        if(!file.exists())
        {
            throw new FileNotFoundException("There was no file found in the path");
        }

        try(InputStream input = new FileInputStream(file))
        {
            Properties prop = new Properties();
            prop.load(input);

            if(prop.containsKey("sparql.query.endpoint"))
            {
                queryEndpoint = prop.getProperty("sparql.query.endpoint");
            }
            else
            {
                throw new IOException("The file is missing: \"sparql.query.endpoint=<queryEndpoint>\"");
            }

            if(prop.containsKey("sparql.update.endpoint"))
            {
                updateEndpoint = prop.getProperty("sparql.update.endpoint");
            }
            else
            {
                throw new IOException("The file is missing: \"sparql.update.endpoint=<updateEndpoint>\"");
            }
        }
    }

    public void loadproperties(String propfile) throws IOException
    {
        try(InputStream input = new FileInputStream(propfile))
        {
            Properties prop = new Properties();
            prop.load(input);
            String mappingfolder;
            try
            {
                mappingfolder = System.getenv(prop.getProperty("ntuenergy.mappingfolder"));
            }
            catch(NullPointerException e)
            {
                throw new IOException("The key ntuenergy.mappingfolder cannot be found");
            }
            if(mappingfolder == null)
            {
                throw new InvalidPropertiesFormatException("The properties file does not contain the key ntuenergy.mappingfolder with a path to the folder containing the required JSON key to IRI Mappings");
            }
            mappings = new ArrayList<>();
            File folder = new File(mappingfolder);
            File[] mappingFiles = folder.listFiles();

            if(mappingFiles.length == 0)
            {
                throw new IOException("No files in folder");
            }
            else
            {
                for(File mappingFile : mappingFiles)
                {
                    JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(generatedIRIPrefix, mappingFile.getAbsolutePath());
                    mappings.add(mapper);
                    mapper.saveToFile(mappingFile.getAbsolutePath());
                }
            }
        }
    }

    /*
    JSONKeyToIRIMapper IRIGenerator = new JSONKeyToIRIMapper();
    String generatedIRI = IRIGenerator.generateIRI(generatedIRIPrefix, "123");
    */

    public String getBuildingNameFromIRI(String IRI){
        int firstUnderscoreIndex = IRI.indexOf('_');
        if (firstUnderscoreIndex != -1) {
            int endIndex = IRI.indexOf("_Q_KVAR", firstUnderscoreIndex + 1);
            if (endIndex == -1) {
                endIndex = IRI.indexOf("_P_KW", firstUnderscoreIndex + 1);
            }
            if (endIndex != -1) {
                return IRI.substring(firstUnderscoreIndex + 1, endIndex);
            }
        }
        return null;
    }

    public void instantiateTriples(){

        String powerSystemIRI = PowsysPrefix + "PowerSystem_NTU";
        TriplePattern powerSysisType = iri(powerSystemIRI).isA(iri(powerSystem));
        InsertDataQuery powersysInsertion = Queries.INSERT_DATA(powerSysisType);
        kbClient.executeUpdate(powersysInsertion.getQueryString());

        /**
         * Instantiate Generator-related triples
         */
        String powerGeneratorIRI = PowsysPrefix + "NTU_PowerGenerator";
        String GeneratorModelIRI = PowsysPrefix + "NTU_PowerGenerator_Model";
        TriplePattern PSHasGenerator = iri(powerSystemIRI).has(iri(hasSubSystem), iri(powerGeneratorIRI));
        TriplePattern GeneratorisType = iri(powerGeneratorIRI).isA(iri(powerGenerator));
        TriplePattern GeneratorIsModeledByModel = iri(powerGeneratorIRI).has(iri(isModeledBy), iri(GeneratorModelIRI));
        TriplePattern GeneratorModelisType = iri(GeneratorModelIRI).isA(iri(powerSystemModel));

        InsertDataQuery generatorInsertion1 = Queries.INSERT_DATA(PSHasGenerator, GeneratorisType, GeneratorIsModeledByModel, GeneratorModelisType);
        kbClient.executeUpdate(generatorInsertion1.getQueryString());

        for (Map.Entry<String, String> set : GeneratorModelVariables.entrySet()) {
            String GeneratorModelVariable = set.getValue();
            String GeneratorModelName = set.getKey();
            String GeneratorModelVariableIRI = PowsysPrefix + set.getKey();
            //PowerSystemModelIRI hasModelVariable GeneratorModelVariableIRI
            TriplePattern ModelHasVariable = iri(GeneratorModelIRI).has(iri(hasModelVariable), iri(GeneratorModelVariableIRI));
            TriplePattern VariableisTypeVariable = iri(GeneratorModelVariableIRI).isA(iri(GeneratorModelVariable));

            //GeneratorModelVariableIRI hasValue ModelVariableSpecificationIRI
            String ModelVariableSpecificationIRI = PowsysPrefix + "_ModelVariable_" + GeneratorModelName;
            TriplePattern GeneratorModelVariableHasValueSpec = iri(GeneratorModelVariableIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationIRI));
            TriplePattern ModelVariableSpecificationisType = iri(ModelVariableSpecificationIRI).isA(iri(modelVariableSpecification));

            InsertDataQuery GeneratorInsertion2 = Queries.INSERT_DATA(ModelHasVariable, VariableisTypeVariable, GeneratorModelVariableHasValueSpec, ModelVariableSpecificationisType);
            kbClient.executeUpdate(GeneratorInsertion2.getQueryString());
        }

        /**
         * Instantiate Branch-related triples
         */
        String branchIRI = PowsysPrefix + "NTU_Branch";
        String branchModelIRI = PowsysPrefix + "NTU_Branch_Model";
        TriplePattern PSHasBranch = iri(powerSystemIRI).has(iri(hasSubSystem), iri(branchIRI));
        TriplePattern branchisType = iri(branchIRI).isA(iri(branch));
        TriplePattern branchIsModeledByModel = iri(branchIRI).has(iri(isModeledBy), iri(branchModelIRI));
        TriplePattern branchModelisType = iri(branchModelIRI).isA(iri(powerSystemModel));

        InsertDataQuery branchInsertion1 = Queries.INSERT_DATA(PSHasBranch, branchisType, branchIsModeledByModel, branchModelisType);
        kbClient.executeUpdate(branchInsertion1.getQueryString());

        for (Map.Entry<String, String> set : BranchModelVariables.entrySet()) {
            String BranchModelVariable = set.getValue();
            String BranchModelName = set.getKey();
            String BranchModelVariableIRI = PowsysPrefix + set.getKey();
            //PowerSystemModelIRI hasModelVariable BranchModelVariableIRI
            TriplePattern ModelHasVariable = iri(branchModelIRI).has(iri(hasModelVariable), iri(BranchModelVariableIRI));
            TriplePattern VariableisTypeVariable = iri(BranchModelVariableIRI).isA(iri(BranchModelVariable));

            //BranchModelVariableIRI hasValue ModelVariableSpecificationIRI
            String ModelVariableSpecificationIRI = PowsysPrefix + "_ModelVariable_" + BranchModelName;
            TriplePattern BranchModelVariableHasValueSpec = iri(BranchModelVariableIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationIRI));
            TriplePattern ModelVariableSpecificationisType = iri(ModelVariableSpecificationIRI).isA(iri(modelVariableSpecification));

            InsertDataQuery BranchInsertion2 = Queries.INSERT_DATA(ModelHasVariable, VariableisTypeVariable, BranchModelVariableHasValueSpec, ModelVariableSpecificationisType);
            kbClient.executeUpdate(BranchInsertion2.getQueryString());
        }


        /**
         * Instantiate Bus Node-related triples
         */
        String busNodeIRI = PowsysPrefix + "NTU_BusNode";
        String busNodeModelIRI = PowsysPrefix + "NTU_BusNode_Model";
        TriplePattern PSHasBusNode = iri(powerSystemIRI).has(iri(hasSubSystem), iri(busNodeIRI));
        TriplePattern BusNodeIsType = iri(busNodeIRI).isA(iri(busNode));
        TriplePattern BusNodeIsModeledByModel = iri(busNodeIRI).has(iri(isModeledBy), iri(busNodeModelIRI));
        TriplePattern BusNodeModelisType = iri(busNodeModelIRI).isA(iri(powerSystemModel));

        InsertDataQuery busNodeInsertion1 = Queries.INSERT_DATA(PSHasBusNode, BusNodeIsType, BusNodeIsModeledByModel, BusNodeModelisType);
        kbClient.executeUpdate(busNodeInsertion1.getQueryString());

        for (Map.Entry<String, String> set : BusNodeModelVariables.entrySet()) {
            String BusNodeModelVariable = set.getValue();
            String BusNodeModelName = set.getKey();
            String BusNodeModelVariableIRI = PowsysPrefix + set.getKey();
            //PowerSystemModelIRI hasModelVariable BusNodeModelVariableIRI
            TriplePattern ModelHasVariable = iri(busNodeModelIRI).has(iri(hasModelVariable), iri(BusNodeModelVariableIRI));
            TriplePattern VariableisTypeVariable = iri(BusNodeModelVariableIRI).isA(iri(BusNodeModelVariable));

            //BusNodeModelVariableIRI hasValue ModelVariableSpecificationIRI
            String ModelVariableSpecificationIRI = PowsysPrefix + "_ModelVariable_" + BusNodeModelName;
            TriplePattern BusNodeModelVariableHasValueSpec = iri(BusNodeModelVariableIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationIRI));
            TriplePattern ModelVariableSpecificationisType = iri(ModelVariableSpecificationIRI).isA(iri(modelVariableSpecification));

            InsertDataQuery BusNodeInsertion2 = Queries.INSERT_DATA(ModelHasVariable, VariableisTypeVariable, BusNodeModelVariableHasValueSpec, ModelVariableSpecificationisType);
            kbClient.executeUpdate(BusNodeInsertion2.getQueryString());
        }



        InsertDataQuery insertion;

        for (JSONKeyToIRIMapper mapping : mappings){
            List<String> iris = mapping.getAllIRIs();
            for (String iri:iris){
                String buildingName = getBuildingNameFromIRI(iri);

                //IRI isType OM:measure
                TriplePattern isTypeMeasure = iri(iri).isA(iri(Measure));
                insertion = Queries.INSERT_DATA(isTypeMeasure);
                kbClient.executeUpdate(insertion.getQueryString());

                if(iri.contains("KW")){

                    //IRI hasUnit OM:kilowatt
                    TriplePattern hasUnitKW = iri(iri).has(iri(hasUnit), iri(kilowatt));

                    //activePowerIRI hasValue IRI
                    String absorbedActivePowerIRI = PowsysPrefix +buildingName + "_AbsorbedActivePower";
                    TriplePattern APHasValue = iri(absorbedActivePowerIRI).has(iri(OMHasValue), iri(iri));

                    //activePowerIRI isType AbsorbedActivePower
                    TriplePattern typeAP = iri(absorbedActivePowerIRI).isA(iri(absorbedActivePower));

                    //busNodeIRI hasActivePowerAbsorbed activePowerIRI
                    String busNodeIRI = PowsysPrefix + buildingName + "_BusNode";
                    TriplePattern BNHasActivePowerAbsorbed = iri(busNodeIRI).has(iri(hasActivePowerAbsorbed), iri(absorbedActivePowerIRI));

                    //busNodeIRI isType BusNode
                    TriplePattern typeBN = iri(busNodeIRI).isA(iri(busNode));

                    //BusNodeIRI isModeledBy PowerSystemModelIRI
                    String PowerSystemModelIRI = PowsysPrefix + buildingName + "_PowerSystemModel";
                    TriplePattern BNHasPowerSystemModel = iri(busNodeIRI).has(iri(isModeledBy), iri(PowerSystemModelIRI));

                    //PowerSystemModelIRI isType PowerSystemModell
                    TriplePattern typePSM = iri(PowerSystemModelIRI).isA(iri(powerSystemModel));

                    //PowerSystemModelIRI hasModelVariable BusNumebrIRI ---
                    String BusNumberIRI = PowsysPrefix + buildingName + "_BusNumber";
                    TriplePattern PSMHasModelVariableBN = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(BusNumberIRI));

                    //BusNumebr hasValue ModelVariableSpecificationBNIRI
                    String ModelVariableSpecificationBNIRI = PowsysPrefix + buildingName + "_ModelVariable_BusNumber";
                    TriplePattern BNHasModelVariableSpecBN = iri(BusNumberIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationBNIRI));

                    //ModelVariableSpecificationBNIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecBN = iri(ModelVariableSpecificationBNIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationBNIRI numericalValue (Int)
                    LOGGER.info(buildingName);
                    int busNumber = NTUBuildingToBusNum.get(buildingName);
                    LOGGER.info(busNumber);
                    TriplePattern BNNumerical = iri(ModelVariableSpecificationBNIRI).has(iri(numericalValue), busNumber);

                    //PowerSystemModelIRI hasModelVariable Area ---
                    String AreaIRI = PowsysPrefix + buildingName + "_Area";
                    TriplePattern PSMHasModelVariableA = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(AreaIRI));

                    //Area hasValue ModelVariableSpecificationAIRI
                    String ModelVariableSpecificationAIRI = PowsysPrefix + buildingName + "_ModelVariable_Area";
                    TriplePattern AHasModelVariableSpecA = iri(AreaIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationAIRI));

                    //ModelVariableSpecificationAIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecA = iri(ModelVariableSpecificationAIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationAIRI numericalValue (Int)
                    TriplePattern ANumerical = iri(ModelVariableSpecificationAIRI).has(iri(numericalValue), 1);

                    //PowerSystemModelIRI hasModelVariable Bs ---
                    String BsIRI = PowsysPrefix + buildingName + "_Bs";
                    TriplePattern PSMHasModelVariableB = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(BsIRI));

                    //Bs hasValue ModelVariableSpecificationBIRI
                    String ModelVariableSpecificationBIRI = PowsysPrefix + buildingName + "_ModelVariable_Bs";
                    TriplePattern BHasModelVariableSpecB = iri(BsIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationBIRI));

                    //ModelVariableSpecificationBIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecB = iri(ModelVariableSpecificationBIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationBIRI numericalValue (Int)
                    TriplePattern BNumerical = iri(ModelVariableSpecificationBIRI).has(iri(numericalValue), 0);

                    //PowerSystemModelIRI hasModelVariable Gs ---
                    String GsIRI = PowsysPrefix + buildingName + "_Gs";
                    TriplePattern PSMHasModelVariableG = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(GsIRI));


                    //Gs hasValue ModelVariableSpecificationGIRI
                    String ModelVariableSpecificationGIRI = PowsysPrefix + buildingName + "_ModelVariable_Gs";
                    TriplePattern GHasModelVariableSpecG = iri(GsIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationGIRI));

                    //ModelVariableSpecificationGIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecG = iri(ModelVariableSpecificationGIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationGIRI numericalValue (Int)
                    TriplePattern GNumerical = iri(ModelVariableSpecificationGIRI).has(iri(numericalValue), 0);

                    //PowerSystemModelIRI hasModelVariable baseKV ---
                    String baseKVIRI = PowsysPrefix + buildingName + "_baseKV";
                    TriplePattern PSMHasModelVariablebKV = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(baseKVIRI));

                    //baseKV hasValue ModelVariableSpecificationbKVIRI
                    String ModelVariableSpecificationbKVIRI = PowsysPrefix + buildingName + "_ModelVariable_baseKV";
                    TriplePattern bKVHasModelVariableSpecbKV = iri(baseKVIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationbKVIRI));

                    //ModelVariableSpecificationbKVIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecbKV = iri(ModelVariableSpecificationbKVIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationbKVIRI numericalValue (Int)
                    TriplePattern bKVNumerical = iri(ModelVariableSpecificationbKVIRI).has(iri(numericalValue), 11);

                    // ModelVariableSpecificationbKVIRI hasUnitOfMeasure OntoCapeKV
                    TriplePattern bKVMeasure = iri(ModelVariableSpecificationbKVIRI).has(iri(hasUnitOfMeasure), iri(OntoCapekV));

                    //PowerSystemModelIRI hasModelVariable VmMin ---
                    String VmMinIRI = PowsysPrefix + buildingName +  "_VmMin";
                    TriplePattern PSMHasModelVariableVmMin = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(VmMinIRI));

                    //VmMin hasValue ModelVariableSpecificationVmMinIRI
                    String ModelVariableSpecificationVmMinIRI = PowsysPrefix + buildingName + "_ModelVariable_VmMin";
                    TriplePattern VmMinHasModelVariableSpecVmMin = iri(VmMinIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationVmMinIRI));

                    //ModelVariableSpecificationVmMinIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecVmMin = iri(ModelVariableSpecificationVmMinIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationVmMinIRI numericalValue (Double)
                    TriplePattern VmMinNumerical = iri(ModelVariableSpecificationVmMinIRI).has(iri(numericalValue), 0.9);

                    // ModelVariableSpecificationVmMinIRI hasUnitOfMeasure OntoCapeKV
                    TriplePattern VmMinMeasure = iri(ModelVariableSpecificationVmMinIRI).has(iri(hasUnitOfMeasure), iri(OntoCapekV));

                    //PowerSystemModelIRI hasModelVariable VmMax ---
                    String VmMaxIRI = PowsysPrefix + buildingName + "_VmMax";
                    TriplePattern PSMHasModelVariableVmMax = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(VmMaxIRI));

                    //VmMax hasValue ModelVariableSpecificationVmMaxIRI
                    String ModelVariableSpecificationVmMaxIRI = PowsysPrefix + buildingName + "_ModelVariable_VmMax";
                    TriplePattern VmMaxHasModelVariableSpecVmMax = iri(VmMaxIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationVmMaxIRI));

                    //ModelVariableSpecificationVmMaxIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecVmMax = iri(ModelVariableSpecificationVmMaxIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationVmMaxIRI numericalValue (Double)
                    TriplePattern VmMaxNumerical = iri(ModelVariableSpecificationVmMaxIRI).has(iri(numericalValue), 1.1);

                    // ModelVariableSpecificationVmMaxIRI hasUnitOfMeasure OntoCapeKV
                    TriplePattern VmMaxMeasure = iri(ModelVariableSpecificationVmMaxIRI).has(iri(hasUnitOfMeasure), iri(OntoCapekV));

                    //PowerSystemModelIRI hasModelVariable Zone ---
                    String ZoneIRI = PowsysPrefix + buildingName + "_Zone";
                    TriplePattern PSMHasModelVariableZone = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(ZoneIRI));

                    //Zone hasValue ModelVariableSpecificationZoneIRI
                    String ModelVariableSpecificationZoneIRI = PowsysPrefix + buildingName + "_ModelVariable_Zone";
                    TriplePattern ZoneHasModelVariableSpecZone = iri(ZoneIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationZoneIRI));

                    //ModelVariableSpecificationZoneIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecZone = iri(ModelVariableSpecificationZoneIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationZoneIRI numericalValue (Int)
                    TriplePattern ZoneNumerical = iri(ModelVariableSpecificationZoneIRI).has(iri(numericalValue), 1);

                    //PowerSystemModelIRI hasModelVariable Vm ---
                    String VmIRI = PowsysPrefix + buildingName + "_Vm";
                    TriplePattern PSMHasModelVariableVm = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(VmIRI));

                    //Vm hasValue ModelVariableSpecificationVmIRI
                    String ModelVariableSpecificationVmIRI = PowsysPrefix + buildingName + "_ModelVariable_Vm";
                    TriplePattern VmHasModelVariableSpecVm = iri(VmIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationVmIRI));

                    //ModelVariableSpecificationVmIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecVm = iri(ModelVariableSpecificationVmIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationVmIRI numericalValue (Double)
                    TriplePattern VmNumerical = iri(ModelVariableSpecificationVmIRI).has(iri(numericalValue), 1);

                    // ModelVariableSpecificationVmIRI hasUnitOfMeasure OntoCapeKV
                    TriplePattern VmMeasure = iri(ModelVariableSpecificationVmIRI).has(iri(hasUnitOfMeasure), iri(OntoCapekV));

                    // PowerSystemModelIRI hasModelVariable Va ---
                    String VaIRI = PowsysPrefix + buildingName + "_Va";
                    TriplePattern PSMHasModelVariableVa = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(VaIRI));

                    // Va hasValue ModelVariableSpecificationVaIRI
                    String ModelVariableSpecificationVaIRI = PowsysPrefix + buildingName + "_ModelVariable_Va";
                    TriplePattern VaHasModelVariableSpecVa = iri(VaIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationVaIRI));

                    // ModelVariableSpecificationVaIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecVa = iri(ModelVariableSpecificationVaIRI).isA(iri(modelVariableSpecification));

                    // ModelVariableSpecificationVaIRI numericalValue (Double)
                    TriplePattern VaNumerical = iri(ModelVariableSpecificationVaIRI).has(iri(numericalValue), 0);

                    // ModelVariableSpecificationVaIRI hasUnitOfMeasure OntoCapeKV
                    TriplePattern VaMeasure = iri(ModelVariableSpecificationVaIRI).has(iri(hasUnitOfMeasure), iri(OntoCapeDegree));

                    // PowerSystemModelIRI hasModelVariable BusType ---
                    String busTypeIRI = PowsysPrefix + buildingName + "_BusType";
                    TriplePattern PSMHasModelVariableBusType = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(busTypeIRI));

                    // BusType hasValue ModelVariableSpecificationBusTypeIRI
                    String ModelVariableSpecificationBusTypeIRI = PowsysPrefix + buildingName + "_ModelVariable_BusType";
                    TriplePattern busTypeHasModelVariableSpecBusType = iri(busTypeIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationBusTypeIRI));

                    // ModelVariableSpecificationBusTypeIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecBusType = iri(ModelVariableSpecificationBusTypeIRI).isA(iri(modelVariableSpecification));

                    // ModelVariableSpecificationBusTypeIRI nominalValue (String)
                    TriplePattern busTypeNominal = iri(ModelVariableSpecificationBusTypeIRI).has(iri(numericalValue), 1);

                    //PowerSystemIRI hasSubSystem busNodeIRI
                    String powerSystemIRI = PowsysPrefix + "PowerSystem_" + buildingName;
                    TriplePattern PSHasSubsystem = iri(powerSystemIRI).has(iri(hasSubSystem), iri(busNodeIRI));

                    //PowerSystemIRI isType PowerSystem
                    TriplePattern typePS = iri(powerSystemIRI).isA(iri(powerSystem));


                    //Assign rdf:type to all modelvariables
                    TriplePattern typeBusNumber = iri(BusNumberIRI).isA(iri(BusNumber));
                    TriplePattern typeArea = iri(AreaIRI).isA(iri(Area));
                    TriplePattern typeBs = iri(BsIRI).isA(iri(Bs));
                    TriplePattern typeGs = iri(GsIRI).isA(iri(Gs));
                    TriplePattern typeBaseKV = iri(baseKVIRI).isA(iri(BaseKV));
                    TriplePattern typeVmMin = iri(VmMinIRI).isA(iri(VmMin));
                    TriplePattern typeVmMax = iri(VmMaxIRI).isA(iri(VmMax));
                    TriplePattern typeZone = iri(ZoneIRI).isA(iri(Zone));
                    TriplePattern typeVm = iri(VmIRI).isA(iri(Vm));
                    TriplePattern typeVa = iri(VaIRI).isA(iri(Va));
                    TriplePattern typeBt = iri(busTypeIRI).isA(iri(BusType));


                    insertion = Queries.INSERT_DATA(hasUnitKW, APHasValue, typeAP,
                            BNHasActivePowerAbsorbed, typeBN, PSHasSubsystem, typePS, BNHasPowerSystemModel, typePSM,
                            PSMHasModelVariableB, BNumerical, typeModelVariableSpecB, BHasModelVariableSpecB,
                            PSMHasModelVariableG, GHasModelVariableSpecG,typeModelVariableSpecG, GNumerical,  typeModelVariableSpecG,
                            PSMHasModelVariableA, typeModelVariableSpecA, AHasModelVariableSpecA, ANumerical,
                            PSMHasModelVariablebKV,bKVHasModelVariableSpecbKV, typeModelVariableSpecbKV, bKVNumerical, bKVMeasure,
                            PSMHasModelVariableVmMin, VmMinHasModelVariableSpecVmMin, typeModelVariableSpecVmMin, VmMinNumerical, VmMinMeasure,
                            PSMHasModelVariableVmMax, VmMaxHasModelVariableSpecVmMax, VmMaxNumerical, typeModelVariableSpecVmMax, VmMaxMeasure,
                            PSMHasModelVariableZone, ZoneHasModelVariableSpecZone, typeModelVariableSpecZone, ZoneNumerical,
                            PSMHasModelVariableVm, VmHasModelVariableSpecVm, typeModelVariableSpecVm, VmNumerical, VmMeasure,
                            PSMHasModelVariableVa, VaHasModelVariableSpecVa, typeModelVariableSpecVa, VaNumerical,VaMeasure,
                            PSMHasModelVariableBusType, busTypeHasModelVariableSpecBusType, typeModelVariableSpecBusType, busTypeNominal,
                            PSMHasModelVariableBN, BNHasModelVariableSpecBN, typeModelVariableSpecBN, BNNumerical,
                            typeBusNumber, typeArea, typeBs, typeGs, typeBaseKV, typeVmMin, typeVmMax, typeZone, typeVm, typeVa, typeBt
                            );
                    kbClient.executeUpdate(insertion.getQueryString());

                }
                else if(iri.contains("KVAR")){


                    TriplePattern omHasUnit = iri(iri).has(iri(hasUnit), iri(kilovoltamperereactive));
                    //ReactivePowerIRI hasValue IRI
                    String absorbedReactivePowerIRI = PowsysPrefix + buildingName + "_AbsorbedReactivePower";
                    TriplePattern RAPHasValue = iri(absorbedReactivePowerIRI).has(iri(OMHasValue), iri(iri));

                    //activePowerIRI isType AbsorbedReactivePower
                    TriplePattern typeRAP = iri(absorbedReactivePowerIRI).isA(iri(absorbedReactivePower));

                    //busNodeIRI hasReactivePowerAbsorbed activePowerIRI
                    String busNodeIRI = PowsysPrefix + buildingName + "_BusNode";
                    TriplePattern BNHasReactivePowerAbsorbed = iri(busNodeIRI).has(iri(hasReactivePowerAbsorbed), iri(absorbedReactivePowerIRI));

                    InsertDataQuery insert = Queries.INSERT_DATA(omHasUnit, RAPHasValue, typeRAP, BNHasReactivePowerAbsorbed);

                    kbClient.executeUpdate(insert.getQueryString());

                }
            }
        }

    }



}
