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
    public static final String branch = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#UndergroundCable";
    public static final String absorbedActivePower = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedActivePower";
    public static final String absorbedReactivePower = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedReactivePower";
    public static final String voltageAngle = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#VoltageAngle";
    public static final String voltageMagnitude = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#VoltageMagnitude";


    /**
     * Model variables
     */
    public static final String PowerSystemModelPrefix = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#";


    public static final String modelVariableSpecification = "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#ModelVariableSpecification";
    public static final String Measure = "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure";



    /**
     * Relationships
     */
    private static final String OMHasValue = OmPrefix + "hasValue";
    private static final String OntoCapeHasValue = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue";
    private static final String hasActivePowerAbsorbed = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasActivePowerAbsorbed";
    private static final String hasReactivePowerAbsorbed = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasReactivePowerAbsorbed";
    private static final String hasVoltageAngle = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasVoltageAngle";
    //TODO: Add hasVoltageMagnitude to OntoPowsys
    private static final String hasVoltageMagnitude = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasVoltageMagnitude";

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
    private static final String degree = OmPrefix + "degree";
    private static final String kilovolt = OmPrefix + "kilovolt";
    private static final String OntoCapekV = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kV";
    private static final String OntoCapeDegree = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree";


    /**
     * NTU building to Bus number mapping
     */
    HashMap<String, Integer> NTUBuildingToBusNum = new HashMap<>();

    /**
     * Branch-related parameters stored in a hashmap
     */
    HashMap<String, List<String>> branchParams = new HashMap<String, List<String>>();
    /**
     * Generator-related parameters stored in a hashmap
     */
    HashMap<String, List<String>> generatorParams = new HashMap<String, List<String>>();

    /**
     * Bus-related parameters stored in a hashmap
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

        NTUBuildingToBusNum.put("GENERATOR_NODE", 1);
        NTUBuildingToBusNum.put("N1_3", 2);
        NTUBuildingToBusNum.put("NYA", 3);
        NTUBuildingToBusNum.put("CANTEEN_2", 4);
        NTUBuildingToBusNum.put("CONNECTION_NODE", 5);
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
        BusNodeModelVariables.put("bus_i", PowerSystemModelPrefix + "BusNumber");
        BusNodeModelVariables.put("type", PowerSystemModelPrefix + "BusType");
        BusNodeModelVariables.put("area", PowerSystemModelPrefix + "Area");
        BusNodeModelVariables.put("Bs", PowerSystemModelPrefix + "Bs");
        BusNodeModelVariables.put("Gs", PowerSystemModelPrefix + "Gs");
        BusNodeModelVariables.put("baseKV", PowerSystemModelPrefix + "baseKV");
        BusNodeModelVariables.put("Vmin", PowerSystemModelPrefix + "VmMin");
        BusNodeModelVariables.put("Vmax", PowerSystemModelPrefix + "VmMax");
        BusNodeModelVariables.put("Zone", PowerSystemModelPrefix + "Zone");
        BusNodeModelVariables.put("Vm", PowerSystemModelPrefix + "Vm");
        BusNodeModelVariables.put("Va", PowerSystemModelPrefix + "Va");

        //BusNode Parameters
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
        busParams.put("Vmin", Arrays.asList("1", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9"));

        //PowerGenerator model variables
        GeneratorModelVariables.put("bus", PowerSystemModelPrefix + "BusNumber");
        GeneratorModelVariables.put("Pg", PowerSystemModelPrefix + "Pg");
        GeneratorModelVariables.put("Qg", PowerSystemModelPrefix + "Qg");
        GeneratorModelVariables.put("Qmax", PowerSystemModelPrefix + "QMax");
        GeneratorModelVariables.put("Qmin", PowerSystemModelPrefix + "QMin");
        GeneratorModelVariables.put("Vg", PowerSystemModelPrefix + "Vg");
        GeneratorModelVariables.put("mBase", PowerSystemModelPrefix + "mBase");
        GeneratorModelVariables.put("status", PowerSystemModelPrefix + "Status");
        GeneratorModelVariables.put("Pmax", PowerSystemModelPrefix + "PMax");
        GeneratorModelVariables.put("Pmin", PowerSystemModelPrefix + "PMin");
        GeneratorModelVariables.put("Pc1", PowerSystemModelPrefix + "Pc1");
        GeneratorModelVariables.put("Pc2", PowerSystemModelPrefix + "Pc2");
        GeneratorModelVariables.put("Qc1min", PowerSystemModelPrefix + "QC1Min");
        GeneratorModelVariables.put("Qc1max", PowerSystemModelPrefix + "QC1Max");
        GeneratorModelVariables.put("Qc2min", PowerSystemModelPrefix + "QC2Min");
        GeneratorModelVariables.put("Qc2max", PowerSystemModelPrefix + "QC2Max");
        GeneratorModelVariables.put("ramp_agc", PowerSystemModelPrefix + "Rampagc");
        GeneratorModelVariables.put("ramp_10", PowerSystemModelPrefix + "Ramp10");
        GeneratorModelVariables.put("ramp_30", PowerSystemModelPrefix + "Ramp30");
        GeneratorModelVariables.put("ramp_q", PowerSystemModelPrefix + "Rampq");
        GeneratorModelVariables.put("apf", PowerSystemModelPrefix + "APF");
        GeneratorModelVariables.put("CostModel", PowerSystemModelPrefix + "CostModel");
        GeneratorModelVariables.put("StartCost", PowerSystemModelPrefix + "StartCost");
        GeneratorModelVariables.put("StopCost", PowerSystemModelPrefix + "StopCost");
        GeneratorModelVariables.put("genCostn", PowerSystemModelPrefix + "genCostn");
        GeneratorModelVariables.put("genCostcn-1", PowerSystemModelPrefix + "genCostcn-1");
        GeneratorModelVariables.put("genCostcn-2", PowerSystemModelPrefix + "genCostcn-2");
        GeneratorModelVariables.put("genCostc0", PowerSystemModelPrefix + "genCostc0");

        //GeneratorModelVariables.put("GeneratedActivePower", PowerSystemModelPrefix + "GeneratedActivePower");
        //GeneratorModelVariables.put("GeneratedReactivePower", PowerSystemModelPrefix + "GeneratedReactivePower");

        //PowerGenerator model variables
        generatorParams.put("bus", Arrays.asList("1"));
        generatorParams.put("Pg", Arrays.asList("0"));
        generatorParams.put("Qg", Arrays.asList("0"));
        generatorParams.put("Qmax", Arrays.asList("10"));
        generatorParams.put("Qmin", Arrays.asList("-10"));
        generatorParams.put("Vg", Arrays.asList("1"));
        generatorParams.put("mBase", Arrays.asList("100"));
        generatorParams.put("status", Arrays.asList("1"));
        generatorParams.put("Pmax", Arrays.asList("10"));
        generatorParams.put("Pmin", Arrays.asList("0"));
        generatorParams.put("Pc1", Arrays.asList("0"));
        generatorParams.put("Pc2", Arrays.asList("0"));
        generatorParams.put("Qc1min", Arrays.asList("0"));
        generatorParams.put("Qc1max", Arrays.asList("0"));
        generatorParams.put("Qc2min", Arrays.asList("0"));
        generatorParams.put("Qc2max", Arrays.asList("0"));
        generatorParams.put("ramp_agc", Arrays.asList("0"));
        generatorParams.put("ramp_10", Arrays.asList("0"));
        generatorParams.put("ramp_30", Arrays.asList("0"));
        generatorParams.put("ramp_q", Arrays.asList("0"));
        generatorParams.put("apf", Arrays.asList("0"));
        //PowerGenerator model variables (cost data)
        generatorParams.put("CostModel", Arrays.asList("2"));
        generatorParams.put("StartCost", Arrays.asList("0"));
        generatorParams.put("StopCost", Arrays.asList("0"));
        generatorParams.put("genCostn", Arrays.asList("3"));
        generatorParams.put("genCostcn-1", Arrays.asList("0"));
        generatorParams.put("genCostcn-2", Arrays.asList("20"));
        generatorParams.put("genCostc0", Arrays.asList("0"));

        //Branch model variables
        //BranchModelVariables.put("QAverage", PowerSystemModelPrefix + "QAverage");
        BranchModelVariables.put("rateB", PowerSystemModelPrefix + "RateB");
        BranchModelVariables.put("rateC", PowerSystemModelPrefix + "RateC");
        BranchModelVariables.put("ratio", PowerSystemModelPrefix + "RatioCoefficient");
        BranchModelVariables.put("angmax", PowerSystemModelPrefix + "AngleMax");
        //BranchModelVariables.put("PAverage", PowerSystemModelPrefix + "PAverage");
        BranchModelVariables.put("fbus", PowerSystemModelPrefix + "BusFrom");
        BranchModelVariables.put("x", PowerSystemModelPrefix + "X");
        BranchModelVariables.put("b", PowerSystemModelPrefix + "B");
        BranchModelVariables.put("rateA", PowerSystemModelPrefix + "RateA");
        BranchModelVariables.put("angle", PowerSystemModelPrefix + "Angle");
        BranchModelVariables.put("angmin", PowerSystemModelPrefix + "AngleMin");
        //BranchModelVariables.put("QLoss", PowerSystemModelPrefix + "QLoss");
        //BranchModelVariables.put("SAverage", PowerSystemModelPrefix + "SAverage");
        BranchModelVariables.put("r", PowerSystemModelPrefix + "R");
        BranchModelVariables.put("tbus", PowerSystemModelPrefix + "BusTo");
        //BranchModelVariables.put("PLoss", PowerSystemModelPrefix + "PLoss");
        BranchModelVariables.put("status", PowerSystemModelPrefix + "BranchStatus");

        //Branch model parameters
        branchParams.put("fbus", Arrays.asList("1", "2", "3", "4", "2", "9", "2", "6", "6", "3", "11", "12", "4", "4"));
        branchParams.put("tbus", Arrays.asList("2", "3", "4", "5", "9", "10", "6", "7", "8", "11", "12", "13", "14", "15"));
        branchParams.put("r", Arrays.asList("0.011182562", "0.009671405", "0.006951322", "0.012590744", "0.016637769", "0.013939752", "0.021134463", "0.008993388", "0.010342397", "0.014839091", "0.020235124", "0.016637769", "0.018436446", "0.009892727"));
        branchParams.put("x", Arrays.asList("0.010937934", "0.009459835", "0.006799256", "0.008492562", "0.011222314", "0.009402479", "0.014255372", "0.006066116", "0.006976033", "0.010009091", "0.01364876", "0.011222314", "0.012435537", "0.006672727"));
        branchParams.put("b", Arrays.asList("0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"));
        branchParams.put("rateA", Arrays.asList("9900", "9900", "9900", "9900", "9900", "9900", "9900", "9900", "9900", "9900", "9900", "9900", "9900", "9900"));
        branchParams.put("rateB", Arrays.asList("0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"));
        branchParams.put("rateC", Arrays.asList("0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"));
        branchParams.put("ratio", Arrays.asList("0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"));
        branchParams.put("angle", Arrays.asList("0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"));
        branchParams.put("status", Arrays.asList("1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"));
        branchParams.put("angmin", Arrays.asList("-360", "-360", "-360", "-360", "-360", "-360", "-360", "-360", "-360", "-360", "-360", "-360", "-360", "-360"));
        branchParams.put("angmax", Arrays.asList("360", "360", "360", "360", "360", "360", "360", "360", "360", "360", "360", "360", "360", "360"));


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
            if (endIndex == -1) {
                endIndex = IRI.indexOf("_VA_DEGREE", firstUnderscoreIndex + 1);
            }
            if (endIndex == -1) {
                endIndex = IRI.indexOf("_VM_KV", firstUnderscoreIndex + 1);
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

        for (int entry=0; entry < 1; entry++){

            String powerGeneratorIRI = PowsysPrefix + "NTU_PowerGenerator_" + String.valueOf(entry);
            String GeneratorModelIRI = PowsysPrefix + "NTU_PowerGenerator_" +  String.valueOf(entry) + "_Model";
            TriplePattern PSHasGenerator = iri(powerSystemIRI).has(iri(hasSubSystem), iri(powerGeneratorIRI));
            TriplePattern GeneratorisType = iri(powerGeneratorIRI).isA(iri(powerGenerator));
            TriplePattern GeneratorIsModeledByModel = iri(powerGeneratorIRI).has(iri(isModeledBy), iri(GeneratorModelIRI));
            TriplePattern GeneratorModelisType = iri(GeneratorModelIRI).isA(iri(powerSystemModel));

            InsertDataQuery generatorInsertion1 = Queries.INSERT_DATA(PSHasGenerator, GeneratorisType, GeneratorIsModeledByModel, GeneratorModelisType);
            kbClient.executeUpdate(generatorInsertion1.getQueryString());


            for (Map.Entry<String, String> set : GeneratorModelVariables.entrySet()) {
                String GeneratorModelVariable = set.getValue();
                String GeneratorModelName = set.getKey();
                String GeneratorModelVariableIRI = PowsysPrefix + "PowerGenerator_" + String.valueOf(entry) + "_ModelVariable_" + GeneratorModelName;
                //PowerSystemModelIRI hasModelVariable GeneratorModelVariableIRI
                TriplePattern ModelHasVariable = iri(GeneratorModelIRI).has(iri(hasModelVariable), iri(GeneratorModelVariableIRI));
                TriplePattern VariableisTypeVariable = iri(GeneratorModelVariableIRI).isA(iri(GeneratorModelVariable));

                //GeneratorModelVariableIRI hasValue ModelVariableSpecificationIRI
                String ModelVariableSpecificationIRI = PowsysPrefix + "PowerGenerator_" + String.valueOf(entry) + "_ModelVariableSpec_" + GeneratorModelName;
                TriplePattern GeneratorModelVariableHasValueSpec = iri(GeneratorModelVariableIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationIRI));
                TriplePattern ModelVariableSpecificationisType = iri(ModelVariableSpecificationIRI).isA(iri(modelVariableSpecification));

                //ModelVariableSpecificationIRI numericalValue
                LOGGER.info(GeneratorModelName);
                String numValue = generatorParams.get(GeneratorModelName).get(entry);
                TriplePattern SpecNumericalValue = iri(ModelVariableSpecificationIRI).has(iri(numericalValue), numValue);

                InsertDataQuery GeneratorInsertion2 = Queries.INSERT_DATA(ModelHasVariable, VariableisTypeVariable,
                        GeneratorModelVariableHasValueSpec, ModelVariableSpecificationisType, SpecNumericalValue);
                kbClient.executeUpdate(GeneratorInsertion2.getQueryString());
            }
        }



        /**
         * Instantiate Branch-related triples
         */

        for (int entry=0; entry < 14; entry++) {

            String branchIRI = PowsysPrefix + "NTU_Branch_" + String.valueOf(entry);
            String branchModelIRI = PowsysPrefix + "NTU_Branch_" + String.valueOf(entry) + "_Model";
            TriplePattern PSHasBranch = iri(powerSystemIRI).has(iri(hasSubSystem), iri(branchIRI));
            TriplePattern branchisType = iri(branchIRI).isA(iri(branch));
            TriplePattern branchIsModeledByModel = iri(branchIRI).has(iri(isModeledBy), iri(branchModelIRI));
            TriplePattern branchModelisType = iri(branchModelIRI).isA(iri(powerSystemModel));

            InsertDataQuery branchInsertion1 = Queries.INSERT_DATA(PSHasBranch, branchisType, branchIsModeledByModel, branchModelisType);
            kbClient.executeUpdate(branchInsertion1.getQueryString());

            for (Map.Entry<String, String> set : BranchModelVariables.entrySet()) {
                String BranchModelVariable = set.getValue();
                String BranchModelName = set.getKey();
                String BranchModelVariableIRI = PowsysPrefix + "Branch_" + String.valueOf(entry) + "_ModelVariable_" + BranchModelName;
                //PowerSystemModelIRI hasModelVariable BranchModelVariableIRI
                TriplePattern ModelHasVariable = iri(branchModelIRI).has(iri(hasModelVariable), iri(BranchModelVariableIRI));
                TriplePattern VariableisTypeVariable = iri(BranchModelVariableIRI).isA(iri(BranchModelVariable));

                //BranchModelVariableIRI hasValue ModelVariableSpecificationIRI
                String ModelVariableSpecificationIRI = PowsysPrefix + "Branch_" + String.valueOf(entry) + "_ModelVariableSpec_" + BranchModelName;
                TriplePattern BranchModelVariableHasValueSpec = iri(BranchModelVariableIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationIRI));
                TriplePattern ModelVariableSpecificationisType = iri(ModelVariableSpecificationIRI).isA(iri(modelVariableSpecification));

                //ModelVariableSpecificationIRI numericalValue
                LOGGER.info(BranchModelName);
                String numValue = branchParams.get(BranchModelName).get(entry);
                TriplePattern SpecNumericalValue = iri(ModelVariableSpecificationIRI).has(iri(numericalValue), numValue);

                InsertDataQuery BranchInsertion2 = Queries.INSERT_DATA(ModelHasVariable, VariableisTypeVariable, BranchModelVariableHasValueSpec, ModelVariableSpecificationisType, SpecNumericalValue);
                kbClient.executeUpdate(BranchInsertion2.getQueryString());
            }
        }


        /**
         * Instantiate Bus Node-related triples
         */
        for (int entry=1; entry < 16; entry++) {

            String busNodeIRI = PowsysPrefix + "NTU_BusNode_" + String.valueOf(entry);
            String busNodeModelIRI = PowsysPrefix + "NTU_BusNode_Model_" + String.valueOf(entry) + "_Model";
            TriplePattern PSHasBusNode = iri(powerSystemIRI).has(iri(hasSubSystem), iri(busNodeIRI));
            TriplePattern BusNodeIsType = iri(busNodeIRI).isA(iri(busNode));
            TriplePattern BusNodeIsModeledByModel = iri(busNodeIRI).has(iri(isModeledBy), iri(busNodeModelIRI));
            TriplePattern BusNodeModelisType = iri(busNodeModelIRI).isA(iri(powerSystemModel));

            InsertDataQuery busNodeInsertion1 = Queries.INSERT_DATA(PSHasBusNode, BusNodeIsType, BusNodeIsModeledByModel, BusNodeModelisType);
            kbClient.executeUpdate(busNodeInsertion1.getQueryString());

            for (Map.Entry<String, String> set : BusNodeModelVariables.entrySet()) {
                String BusNodeModelVariable = set.getValue();
                String BusNodeModelName = set.getKey();
                String BusNodeModelVariableIRI = PowsysPrefix + "BusNode_" + String.valueOf(entry) + "_ModelVariable_" + BusNodeModelName;
                //PowerSystemModelIRI hasModelVariable BusNodeModelVariableIRI
                TriplePattern ModelHasVariable = iri(busNodeModelIRI).has(iri(hasModelVariable), iri(BusNodeModelVariableIRI));
                TriplePattern VariableisTypeVariable = iri(BusNodeModelVariableIRI).isA(iri(BusNodeModelVariable));

                //BusNodeModelVariableIRI hasValue ModelVariableSpecificationIRI
                String ModelVariableSpecificationIRI = PowsysPrefix + "BusNode_" + String.valueOf(entry) + "_ModelVariableSpec_" + BusNodeModelName;
                TriplePattern BusNodeModelVariableHasValueSpec = iri(BusNodeModelVariableIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationIRI));
                TriplePattern ModelVariableSpecificationisType = iri(ModelVariableSpecificationIRI).isA(iri(modelVariableSpecification));

                //ModelVariableSpecificationIRI numericalValue
                LOGGER.info(BusNodeModelName);
                String numValue = busParams.get(BusNodeModelName).get(entry - 1);
                TriplePattern SpecNumericalValue = iri(ModelVariableSpecificationIRI).has(iri(numericalValue), numValue);

                InsertDataQuery BusNodeInsertion2 = Queries.INSERT_DATA(ModelHasVariable, VariableisTypeVariable,
                        BusNodeModelVariableHasValueSpec, ModelVariableSpecificationisType, SpecNumericalValue);
                kbClient.executeUpdate(BusNodeInsertion2.getQueryString());

            }
        }


        /**
         * Instantiate Timeseries-related triples
         */
        for (JSONKeyToIRIMapper mapping : mappings){
            List<String> iris = mapping.getAllIRIs();
            for (String iri:iris){

                String buildingName = getBuildingNameFromIRI(iri);
                int busNum = NTUBuildingToBusNum.get(buildingName);
                String busNodeIRI = PowsysPrefix + "NTU_BusNode_" + String.valueOf(busNum);

                //IRI isType OM:Measure
                TriplePattern isTypeMeasure = iri(iri).isA(iri(Measure));
                InsertDataQuery insertion = Queries.INSERT_DATA(isTypeMeasure);
                kbClient.executeUpdate(insertion.getQueryString());

                if(iri.contains("KW")){

                    String absorbedActivePowerIRI = PowsysPrefix + "BusNode_" + String.valueOf(busNum) + "_AbsorbedActivePower";

                    //busNodeIRI hasReactivePowerAbsorbed reactivePowerIRI
                    TriplePattern BNHasActivePowerAbsorbed = iri(busNodeIRI).has(iri(hasActivePowerAbsorbed), iri(absorbedActivePowerIRI));

                    //has unit
                    TriplePattern omHasUnit = iri(iri).has(iri(hasUnit), iri(kilowatt));

                    //ReactivePowerIRI hasValue IRI
                    TriplePattern APHasValue = iri(absorbedActivePowerIRI).has(iri(OMHasValue), iri(iri));

                    //activePowerIRI isType AbsorbedReactivePower
                    TriplePattern typeAP = iri(absorbedActivePowerIRI).isA(iri(absorbedActivePower));

                    InsertDataQuery insert = Queries.INSERT_DATA(omHasUnit, APHasValue, typeAP, BNHasActivePowerAbsorbed);

                    kbClient.executeUpdate(insert.getQueryString());


                }
                else if(iri.contains("KVAR")){

                    String absorbedReactivePowerIRI = PowsysPrefix + "BusNode_" + String.valueOf(busNum) + "_AbsorbedReactivePower";

                    //busNodeIRI hasReactivePowerAbsorbed reactivePowerIRI
                    TriplePattern BNHasReactivePowerAbsorbed = iri(busNodeIRI).has(iri(hasReactivePowerAbsorbed), iri(absorbedReactivePowerIRI));

                    //has unit
                    TriplePattern omHasUnit = iri(iri).has(iri(hasUnit), iri(kilovoltamperereactive));

                    //ReactivePowerIRI hasValue IRI
                    TriplePattern RAPHasValue = iri(absorbedReactivePowerIRI).has(iri(OMHasValue), iri(iri));

                    //activePowerIRI isType AbsorbedReactivePower
                    TriplePattern typeRAP = iri(absorbedReactivePowerIRI).isA(iri(absorbedReactivePower));

                    InsertDataQuery insert = Queries.INSERT_DATA(omHasUnit, RAPHasValue, typeRAP, BNHasReactivePowerAbsorbed);

                    kbClient.executeUpdate(insert.getQueryString());
                }
                else if(iri.contains("DEGREE")){

                    String voltageAngleIRI = PowsysPrefix + "BusNode_" + String.valueOf(busNum) + "_VoltageAngle";

                    //busNodeIRI hasVoltageAngle voltageAngleIRI
                    TriplePattern BNHasVoltageAngle = iri(busNodeIRI).has(iri(hasVoltageAngle), iri(voltageAngleIRI));

                    //has unit
                    TriplePattern omHasUnit = iri(iri).has(iri(hasUnit), iri(degree));

                    //voltageAngleIRI hasValue IRI
                    TriplePattern VAHasValue = iri(voltageAngleIRI).has(iri(OMHasValue), iri(iri));

                    //voltageAngleIRI isType VoltageAngle
                    TriplePattern typeVA = iri(voltageAngleIRI).isA(iri(voltageAngle));

                    InsertDataQuery insert = Queries.INSERT_DATA(omHasUnit, VAHasValue, typeVA, BNHasVoltageAngle);

                    kbClient.executeUpdate(insert.getQueryString());
                }

                else if(iri.contains("KV")){

                    String voltageMagnitudeIRI = PowsysPrefix + "BusNode_" + String.valueOf(busNum) + "_VoltageMagnitude";

                    //busNodeIRI hasVoltagemagnitude voltageMagnitudeIRI
                    TriplePattern BNHasVoltageMagnitude = iri(busNodeIRI).has(iri(hasVoltageMagnitude), iri(voltageMagnitudeIRI));

                    //has unit
                    TriplePattern omHasUnit = iri(iri).has(iri(hasUnit), iri(kilovolt));

                    //voltageMagnitudeIRI hasValue IRI
                    TriplePattern VMHasValue = iri(voltageMagnitudeIRI).has(iri(OMHasValue), iri(iri));

                    //voltageAngleIRI isType VoltageAngle
                    TriplePattern typeVM = iri(voltageMagnitudeIRI).isA(iri(voltageMagnitude));

                    InsertDataQuery insert = Queries.INSERT_DATA(omHasUnit, VMHasValue, typeVM, BNHasVoltageMagnitude);

                    kbClient.executeUpdate(insert.getQueryString());
                }

            }
        }


    }

}

