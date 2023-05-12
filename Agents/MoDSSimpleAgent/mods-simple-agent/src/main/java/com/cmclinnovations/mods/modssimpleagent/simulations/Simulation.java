package com.cmclinnovations.mods.modssimpleagent.simulations;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import com.cmclinnovations.mods.api.MoDSAPI;
import com.cmclinnovations.mods.api.MoDSAPI.DataType;
import com.cmclinnovations.mods.api.Options;
import com.cmclinnovations.mods.modssimpleagent.BackendInputFile;
import com.cmclinnovations.mods.modssimpleagent.CSVDataFile;
import com.cmclinnovations.mods.modssimpleagent.CSVDataSeparateFiles;
import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackendFactory;
import com.cmclinnovations.mods.modssimpleagent.TemplateLoader;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Algorithm;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Data;
import com.cmclinnovations.mods.modssimpleagent.datamodels.DataColumn;
import com.cmclinnovations.mods.modssimpleagent.datamodels.InputMetaData;
import com.cmclinnovations.mods.modssimpleagent.datamodels.InputMetaDataRow;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;
import com.cmclinnovations.mods.modssimpleagent.datamodels.SensitivityLabels;
import com.cmclinnovations.mods.modssimpleagent.datamodels.SensitivityResult;
import com.cmclinnovations.mods.modssimpleagent.datamodels.SensitivityValues;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Variable;
import com.cmclinnovations.mods.modssimpleagent.utils.FileUtils;
import com.cmclinnovations.mods.modssimpleagent.utils.ListUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Streams;

public abstract class Simulation {

    private static final Logger LOGGER = LogManager.getLogger(Simulation.class);

    public static final String DATA_ALGORITHM_NAME = "Data_Algorithm";
    public static final String DEFAULT_MOO_ALGORITHM_NAME = "MOOAlg";
    public static final String DEFAULT_SURROGATE_ALGORITHM_NAME = "GenSurrogateAlg";
    public static final String DEFAULT_SAMPLING_ALGORITHM_NAME = "SamplingAlg";

    public static final String DEFAULT_CASE_NAME = "Case";
    public static final String DEFAULT_CASEGROUP_NAME = "CaseGroup";

    public static final String DEFAULT_SURROGATE_MODEL_NAME = "SurrogateModel";

    public static final String INITIAL_FILE_NAME = "initialFile.csv";
    public static final String SAMPLING_ALGORITHM_FILE_NAME = "SamplingAlg_data";

    private static final String REQUEST_FILE_NAME = "request.json";

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private final Request request;
    private final BackendInputFile inputFile;
    private final MoDSBackend modsBackend;
    private final InputMetaData inputMetaData;

    public static Simulation createSimulation(Request request) throws JAXBException, IOException {

        String simulationType = request.simulationType();
        BackendInputFile inputFile = TemplateLoader.load(simulationType);

        MoDSBackend modsBackend = MoDSBackendFactory.createMoDSBackend();

        OBJECT_MAPPER.writeValue(getRequestFilePath(modsBackend), request);

        load(request, modsBackend);

        InputMetaData inputMetaData = InputMetaData.createInputMetaData(request, modsBackend);

        return SimulationFactory.createSimulation(request, inputFile, modsBackend, inputMetaData);
    }

    public static Simulation retrieveSimulation(Request request) throws JAXBException, IOException {
        String jobID = request.jobID();

        MoDSBackend modsBackend = MoDSBackendFactory.retrieveMoDSBackend(jobID);

        Request originalRequest = OBJECT_MAPPER.readValue(getRequestFilePath(modsBackend), Request.class);
        BackendInputFile inputFile = new BackendInputFile(
                modsBackend.getWorkingDir().resolve(BackendInputFile.FILENAME));

        InputMetaData inputMetaData = InputMetaData.createInputMetaData(originalRequest, modsBackend);

        return SimulationFactory.createSimulation(request, inputFile, modsBackend, inputMetaData);
    }

    private static File getRequestFilePath(MoDSBackend modsBackend) {
        return modsBackend.getSimDir().resolve(REQUEST_FILE_NAME).toFile();
    }

    protected Simulation(Request request, BackendInputFile inputFile, MoDSBackend modsBackend,
            InputMetaData inputMetaData) {
        this.request = request;
        this.inputFile = inputFile;
        this.modsBackend = modsBackend;
        this.inputMetaData = inputMetaData;
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

    public String getJobID() {
        return modsBackend.getJobID();
    }

    protected Algorithm getPrimaryAlgorithm() {
        return request.algorithms().get(0);
    }

    protected void populateInputFile() {
        List<Variable> variables = getPrimaryAlgorithm().variables();
        populateAlgorithmNodes(variables);
        populateCaseNodes();
        populateModelNodes();
        populateFileNodes();
        populateParameterNodes(variables);

    }

    protected void populateMOOAlgorithmNode(String mooAlgName, String displayName, List<Variable> variables) {
        Map<String, List<String>> partitionedSubtypes = variables.stream()
                .filter(variable -> variable.type().equals("output")).collect(Collectors.groupingBy(Variable::objective,
                        Collectors.mapping(Variable::getSubtype, Collectors.toList())));
        inputFile.configureMOOAlgorithm(mooAlgName, displayName, partitionedSubtypes);
    }

    protected void populateAlgorithmNodes(List<Variable> variables) {
        Map<String, List<String>> partitionedVars = variables.stream().collect(
                Collectors.groupingBy(Variable::type, Collectors.mapping(Variable::getSubtype, Collectors.toList())));
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
        Iterator<Double> minItr = inputMetaData.getMinima().iterator();
        Iterator<Double> maxItr = inputMetaData.getMaxima().iterator();
        for (String name : inputMetaData.getVarNames()) {
            Variable variable = variables.stream().filter(varToTest -> varToTest.name().equals(name)).findFirst()
                    .orElseThrow();
            inputFile.addParameter(name, variable.getSubtype(), variable.type(), getVariableCases(name),
                    getVariableModels(name), minItr.next(), maxItr.next());
        }
    }

    protected Algorithm getAlgorithmOfType(String algType) {
        return getRequest().getAlgorithmOfType(algType);
    }

    public String getFullCaseName(String caseGroupName, String caseName) {
        return caseGroupName + "_" + caseName;
    }

    protected List<String> getCases() {
        return List.of(getFullCaseName(DEFAULT_CASEGROUP_NAME, DEFAULT_CASE_NAME));
    }

    protected List<String> getModels() {
        return List.of(DEFAULT_SURROGATE_MODEL_NAME);
    }

    /**
     * 
     * @param varName Name of the variable
     * @return Names of the cases associated with the specified variable
     */
    protected List<String> getVariableCases(String varName) {
        return getCases();
    }

    /**
     * 
     * @param caseName Name of the case
     * @return Names of the models associated with the specified case
     */
    protected List<String> getCaseModels(String caseName) {
        return getModels();
    }

    /**
     * 
     * @param varName Name of the variable
     * @return Names of the models associated with the specified variable
     */
    protected List<String> getVariableModels(String varName) {
        return getModels();
    }

    protected void generateFiles() throws FileGenerationException {
        generateBackendInputFile();
    }

    protected final void generateBackendInputFile() throws FileGenerationException {
        inputFile.marshal(modsBackend.getWorkingDir().resolve(BackendInputFile.FILENAME));
    }

    protected final void generateInitialFileFromInputs() throws FileGenerationException {
        Data inputs = getRequest().inputs();
        Path path = modsBackend.getInitialDir().resolve(INITIAL_FILE_NAME);
        new CSVDataFile(inputs.getAverages()).marshal(path);
    }

    protected final void generateInitialFileFromMetaData() throws FileGenerationException {
        Path path = modsBackend.getInitialDir().resolve(INITIAL_FILE_NAME);
        new CSVDataFile(inputMetaData.getMeansAsData()).marshal(path);
    }

    protected final void generateDataAlgFiles() throws FileGenerationException {
        generateAlgorithmFiles(getRequest().inputs(), DATA_ALGORITHM_NAME);
    }

    protected final void generateSamplingAlgDataFiles() throws FileGenerationException {
        generateAlgorithmFiles(getRequest().inputs(), SAMPLING_ALGORITHM_FILE_NAME);
    }

    protected final void generateMOOAlgDataFiles() throws FileGenerationException {
        generateAlgorithmFiles(getRequest().inputs(), DEFAULT_MOO_ALGORITHM_NAME);
    }

    private final void generateAlgorithmFiles(Data data, String algorithmFileName) throws FileGenerationException {
        try {
            Path algorithmPath = modsBackend.createSubDir(algorithmFileName);

            new CSVDataSeparateFiles(data, algorithmFileName + "_" + Variable.SUBTYPE_PREFIX,
                    getFullCaseName(DEFAULT_CASEGROUP_NAME, DEFAULT_CASE_NAME) + "_").marshal(algorithmPath);
        } catch (IOException ex) {
            throw new FileGenerationException(
                    "Failed to create subdirectory for algorithm '" + algorithmFileName + "'.", ex);
        }
    }

    public void run() throws IOException {
        populateInputFile();
        generateFiles();
        modsBackend.run();
        saveWhenFinished();
    }

    public void saveWhenFinished() {
        String simDir = getModsBackend().getSimDir().toString();
        String algorithmName = DEFAULT_SURROGATE_ALGORITHM_NAME;

        Thread t = new Thread(() -> {
            try {
                while (!MoDSAPI.hasAlgorithmGeneratedOutputFiles(simDir, algorithmName)) {
                    if (!modsBackend.isAlive()) {
                        throw new ResponseStatusException(
                                HttpStatus.NO_CONTENT,
                                "The job '" + getModsBackend().getJobID()
                                        + "' has not been run or has failed to run correctly so could not save.");
                    }
                    Thread.sleep(100);
                }
                save();
            } catch (InterruptedException ex) {
                Thread.currentThread().interrupt();
            }
        });

        t.start();
    }

    public void save() {
        for (Algorithm algorithm : request.algorithms()) {
            if (algorithm.saveSurrogate() != null && algorithm.saveSurrogate()) {
                Path saveDirectory = getSaveDirectory();
                Path surrogateDirectory = getSurrogateDirectory(modsBackend);

                try {
                    FileUtils.copyDirectory(surrogateDirectory, saveDirectory);
                    inputMetaData.writeToCSV(saveDirectory.resolve(InputMetaData.DEFAULT_INPUT_INFO_FILE_NAME));
                } catch (FileGenerationException ex) {
                    throw new ResponseStatusException(HttpStatus.NO_CONTENT,
                            "Job '" + getModsBackend().getJobID() + "' failed to save.", ex);
                }

                LOGGER.info("Job '{}' saved at '{}'.", getModsBackend().getJobID(), saveDirectory.toAbsolutePath());
            }
        }
    }

    public static Path getSurrogateDirectory(MoDSBackend modsBackend) {
        return modsBackend.getSimDir().resolve(DEFAULT_SURROGATE_ALGORITHM_NAME);
    }

    private Path getSaveDirectory() {
        return getSurrogateSaveDirectoryPath().resolve(modsBackend.getJobID())
                .resolve(DEFAULT_SURROGATE_ALGORITHM_NAME);
    }

    public static void load(Request request, MoDSBackend modsBackend) {
        if (request.getSurrogateToLoad() != null) {
            Path surrogateDirectory = getSurrogateDirectory(modsBackend);
            Path loadDirectory = getSurrogateSaveDirectoryPath().resolve(request.getSurrogateToLoad())
                    .resolve(DEFAULT_SURROGATE_ALGORITHM_NAME);
            try {
                if (!Files.exists(loadDirectory)) {
                    throw new IOException(
                            "File '" + loadDirectory.toAbsolutePath() + "' could not be found to load.");
                }

                FileUtils.copyDirectory(loadDirectory, surrogateDirectory);

                LOGGER.info("File '{}' loaded to '{}'.", loadDirectory.toAbsolutePath(),
                        surrogateDirectory.toAbsolutePath());

            } catch (IOException ex) {
                LOGGER.error("Failed to load '{}' to '{}'.", loadDirectory.toAbsolutePath(),
                        surrogateDirectory.toAbsolutePath(), ex);
                throw new ResponseStatusException(HttpStatus.NOT_FOUND,
                        "Job '" + modsBackend.getJobID() + "' failed to load.", ex);
            }
        }

    }

    public Request getResponse() {
        return getDefaultResponse();
    }

    public Request getDefaultResponse() {
        return new Request(getJobID(), request.simulationType());
    }

    public Request getResults() {
        return getResponse();
    }

    protected List<SensitivityResult> getSensitivity() {

        String simDir = getModsBackend().getSimDir().toString();
        String surrogateName = DEFAULT_SURROGATE_ALGORITHM_NAME;

        String algName = getPrimaryAlgorithm().type();
        List<String> xVarNames = MoDSAPI.getXVarNames(simDir, algName);
        List<String> yVarNames = MoDSAPI.getYVarNames(simDir, algName);

        List<List<List<Double>>> allSens = MoDSAPI.getHDMRSensitivities(simDir, surrogateName);

        int nX = xVarNames.size();
        int nY = allSens.size();

        // Compile the labels for each term. Only defined up to second order for now
        List<List<String>> termLabels = new ArrayList<>(2);
        termLabels.add(xVarNames);
        List<String> secondOrderTermLabels = new ArrayList<>(nX * (nX - 1) / 2);
        for (int ix1 = 0; ix1 < nX; ix1++) {
            for (int ix2 = ix1 + 1; ix2 < nX; ix2++) {
                secondOrderTermLabels.add(xVarNames.get(ix1) + " and " + xVarNames.get(ix2));
            }
        }
        termLabels.add(secondOrderTermLabels);

        /*
         * The number of orders shown is limited by both the data and the term labels
         * defined above
         */
        int nOrdersToShow = Math.min(allSens.get(0).size(), termLabels.size());

        List<SensitivityResult> sensitivities = new ArrayList<>(nY);

        for (int iy = 0; iy < nY; iy++) {
            List<SensitivityLabels> sensitivityLabelsList = new ArrayList<>(nOrdersToShow);
            List<SensitivityValues> sensitivityValuesList = new ArrayList<>(nOrdersToShow);
            for (int iOrder = 0; iOrder < nOrdersToShow; iOrder++) {
                // iOrder + 1 so ordering starts at 1 instead of 0
                sensitivityLabelsList.add(new SensitivityLabels(iOrder + 1, termLabels.get(iOrder)));
                sensitivityValuesList.add(new SensitivityValues(iOrder + 1, allSens.get(iy).get(iOrder)));
            }
            sensitivities.add(new SensitivityResult(yVarNames.get(iy), sensitivityLabelsList, sensitivityValuesList));
        }

        return sensitivities;
    }

    public InputMetaData getInputMetaData() {
        return inputMetaData;
    }

    public static Path getSurrogateSaveDirectoryPath() {
        return Path.of(System.getenv("MODS_SAVE_DIR"));
    }

    protected Request getMCDMResults() {

        String simDir = getModsBackend().getSimDir().toString();

        List<Variable> variables = getPrimaryAlgorithm().variables();

        List<String> outputVarNames = MoDSAPI.getReducedYVarIDs(simDir, DEFAULT_MOO_ALGORITHM_NAME).stream()
                .map(MoDSAPI::getVarName)
                .collect(Collectors.toList());

        List<String> inputVarNames = MoDSAPI.getReducedXVarIDs(simDir, DEFAULT_MOO_ALGORITHM_NAME).stream()
                .map(MoDSAPI::getVarName)
                .collect(Collectors.toList());

        List<String> allVarNames = Stream.concat(inputVarNames.stream(), outputVarNames.stream())
                .collect(Collectors.toList());

        List<Double> minimaFromData = ListUtils.filterAndSort(getInputMetaData().getRows(), outputVarNames,
                InputMetaDataRow::varName, InputMetaDataRow::minimum);

        List<Double> maximaFromData = ListUtils.filterAndSort(getInputMetaData().getRows(), outputVarNames,
                InputMetaDataRow::varName, InputMetaDataRow::maximum);

        List<Double> minimaFromAlg = ListUtils.filterAndSort(variables, outputVarNames, Variable::name,
                Variable::minimum);
        List<Double> maximaFromAlg = ListUtils.filterAndSort(variables, outputVarNames, Variable::name,
                Variable::maximum);
        List<Double> weightsFromAlg = ListUtils.filterAndSort(variables, outputVarNames, Variable::name,
                Variable::weight);

        int numResults = getPrimaryAlgorithm().maxNumberOfResults();

        EnumMap<DataType, List<List<Double>>> allPointsMap = MoDSAPI.getMCDMSimpleWeightedPoints(simDir,
                DEFAULT_MOO_ALGORITHM_NAME,
                ListUtils.replaceNulls(minimaFromAlg, minimaFromData),
                ListUtils.replaceNulls(maximaFromAlg, maximaFromData),
                ListUtils.replaceNulls(weightsFromAlg, Collections.nCopies(weightsFromAlg.size(), 1.0)),
                numResults, new Options().setVarIndexFirst(true));

        List<List<Double>> inputPoints = allPointsMap.get(DataType.InputVariable);
        List<List<Double>> ouputPoints = allPointsMap.get(DataType.OutputVariable);
        List<List<Double>> points = Stream.concat(inputPoints.stream(), ouputPoints.stream())
                .collect(Collectors.toList());

        Data values = new Data(
                Streams.zip(allVarNames.stream(), points.stream(), DataColumn::new)
                        .collect(Collectors.toList()));

        return getDefaultResponse().toBuilder().outputs(values).build();
    }
}
