package com.cmclinnovations.mods.modssimpleagent.simulations;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.xml.bind.JAXBException;

import com.cmclinnovations.mods.modssimpleagent.BackendInputFile;
import com.cmclinnovations.mods.modssimpleagent.CSVDataFile;
import com.cmclinnovations.mods.modssimpleagent.CSVDataSeparateFiles;
import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackendFactory;
import com.cmclinnovations.mods.modssimpleagent.TemplateLoader;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Algorithm;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Data;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Variable;

public class Simulation {

    public static final String DATA_ALGORITHM_NAME = "Data_Algorithm";
    public static final String DEFAULT_MOO_ALGORITHM_NAME = "MOOAlg";

    public static final String DEFAULT_CASE_NAME = "Case";
    public static final String DEFAULT_CASEGROUP_NAME = "CaseGroup";

    public static final String DEFAULT_SURROGATE_MODEL_NAME = "SurrogateModel";

    public static final String INITIAL_FILE_NAME = "initialFile.csv";

    private final Request request;
    private final BackendInputFile inputFile;
    private final MoDSBackend modsBackend;

    public static Simulation createSimulation(Request request) throws JAXBException, IOException {

        String simulationType = request.getSimulationType();
        BackendInputFile inputFile = TemplateLoader.load(simulationType);
        MoDSBackend modsBackend = MoDSBackendFactory.createMoDSBackend();
        switch (simulationType) {
            case "MOO":
                return new MOO(request, inputFile, modsBackend);
            default:
                throw new IllegalArgumentException("Unknown simulation type requested '" + simulationType + "'.");
        }
    }

    public Simulation(Request request, BackendInputFile inputFile, MoDSBackend modsBackend) {
        this.request = request;
        this.inputFile = inputFile;
        this.modsBackend = modsBackend;

    }

    protected final Request getRequest() {
        return request;
    }

    protected final BackendInputFile getInputFile() {
        return inputFile;
    }

    protected final MoDSBackend getModsBackend() {
        return modsBackend;
    }

    protected Algorithm getPrimaryAlgorithm() {
        return request.getAlgorithms().get(0);
    }

    protected void populateInputFile() {
        List<Variable> variables = getPrimaryAlgorithm().getVariables();
        populateAlgorithmNodes(variables);
        populateCaseNodes();
        populateModelNodes();
        populateFileNodes();
        populateParameterNodes(variables);

    }

    protected void populateMOOAlgorithmNode(String mooAlgName, String displayName, List<Variable> variables) {
        Map<String, List<String>> partitionedSubtypes = variables.stream()
                .filter(variable -> variable.getType().equals("output"))
                .collect(Collectors.groupingBy(Variable::getObjective,
                        Collectors.mapping(Variable::getSubtype, Collectors.toList())));
        inputFile.configureMOOAlgorithm(mooAlgName, displayName, partitionedSubtypes);
    }

    protected void populateAlgorithmNodes(List<Variable> variables) {
        Map<String, List<String>> partitionedVars = variables.stream().collect(
                Collectors.groupingBy(Variable::getType,
                        Collectors.mapping(Variable::getSubtype, Collectors.toList())));
        inputFile.configureAlgorithmParams(partitionedVars.get("input"), partitionedVars.get("output"));
    }

    private void populateCaseNodes() {
        for (String caseName : getCases()) {
            inputFile.addCase(caseName, getCaseModels(caseName));
        }
    }

    private void populateModelNodes() {
        String type = "Executable";
        for (String modelName : getModels()) {
            inputFile.addModel(modelName, type);
        }
    }

    private void populateFileNodes() {
        String type = "DSV";
        String delimiter = ",";
        for (String fileName : getFiles()) {
            inputFile.addFile(fileName, type, delimiter);
        }
    }

    private List<String> getFiles() {
        return List.of(INITIAL_FILE_NAME);
    }

    private void populateParameterNodes(List<Variable> variables) {
        Data inputs = request.getInputs();
        Iterator<Double> minItr = inputs.getMinimums().getValues().iterator();
        Iterator<Double> maxItr = inputs.getMaximums().getValues().iterator();
        for (String name : inputs.getHeaders()) {
            Variable variable = variables.stream().filter(varToTest -> varToTest.getName().equals(name))
                    .findFirst().orElseThrow();
            inputFile.addParameter(name, variable.getSubtype(), variable.getType(),
                    getVaraibleCases(name), getVariableModels(name),
                    minItr.next(), maxItr.next());
        }
    }

    public String getFullCaseName(String caseGroupName, String caseName) {
        return caseGroupName + "_" + caseName;
    }

    protected List<String> getCases() {
        return List.of(getFullCaseName(DEFAULT_CASEGROUP_NAME, DEFAULT_CASE_NAME));
    }

    protected List<String> getVaraibleCases(String varName) {
        return getCases();
    }

    protected List<String> getModels() {
        return List.of(DEFAULT_SURROGATE_MODEL_NAME);
    }

    protected List<String> getCaseModels(String caseName) {
        return getModels();
    }

    protected List<String> getVariableModels(String varName) {
        return getModels();
    }

    protected void generateFiles() throws FileGenerationException {
        generateBackendInputFile();
    }

    protected final void generateBackendInputFile() throws FileGenerationException {
        inputFile.marshal(modsBackend.getWorkingDir().resolve(BackendInputFile.FILENAME));
    }

    protected final void generateInitialFile() throws FileGenerationException {
        Data inputs = getRequest().getInputs();
        new CSVDataFile(inputs.getAverages()).marshal(modsBackend.getInitialDir().resolve(INITIAL_FILE_NAME));
    }

    protected final void generateDataAlgFiles() throws FileGenerationException {
        Path dataAlgPath;
        try {
            dataAlgPath = modsBackend.createSubDir(DATA_ALGORITHM_NAME);

            new CSVDataSeparateFiles(getRequest().getInputs(),
                    DATA_ALGORITHM_NAME + "_" + Variable.SUBTYPE_PREFIX,
                    getFullCaseName(DEFAULT_CASEGROUP_NAME, DEFAULT_CASE_NAME) + "_")
                    .marshal(dataAlgPath);
        } catch (IOException ex) {
            throw new FileGenerationException(
                    "Failed to create subdirectory for algorithm '" + DATA_ALGORITHM_NAME + "'.", ex);
        }
    }

    public void run() throws IOException {
        populateInputFile();
        generateFiles();
        modsBackend.run();
    }

}
