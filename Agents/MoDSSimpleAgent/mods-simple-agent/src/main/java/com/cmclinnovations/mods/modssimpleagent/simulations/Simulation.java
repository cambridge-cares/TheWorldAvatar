package com.cmclinnovations.mods.modssimpleagent.simulations;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;

import com.cmclinnovations.mods.api.MoDSAPI;
import com.cmclinnovations.mods.modssimpleagent.BackendInputFile;
import com.cmclinnovations.mods.modssimpleagent.CSVDataFile;
import com.cmclinnovations.mods.modssimpleagent.CSVDataSeparateFiles;
import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackendFactory;
import com.cmclinnovations.mods.modssimpleagent.TemplateLoader;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Algorithm;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Data;
import com.cmclinnovations.mods.modssimpleagent.datamodels.InputMetaData;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Variable;
import com.fasterxml.jackson.databind.ObjectMapper;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

public class Simulation {

    private static final Logger LOGGER = LogManager.getLogger(Simulation.class);

    public static final String DATA_ALGORITHM_NAME = "Data_Algorithm";
    public static final String DEFAULT_MOO_ALGORITHM_NAME = "MOOAlg";
    public static final String DEFAULT_SURROGATE_ALGORITHM_NAME = "GenSurrogateAlg";
    public static final String DEFAULT_SAMPLING_ALGORITHM_NAME = "SamplingAlg";

    public static final String DEFAULT_CASE_NAME = "Case";
    public static final String DEFAULT_CASEGROUP_NAME = "CaseGroup";

    public static final String DEFAULT_SURROGATE_MODEL_NAME = "SurrogateModel";

    public static final Path DEFAULT_SURROGATE_SAVE_DIRECTORY_PATH = Path.of("savedsurrogates");

    public static final String INITIAL_FILE_NAME = "initialFile.csv";
    public static final String SAMPLING_ALGORITHM_FILE_NAME = "SamplingAlg_data";

    private static final String REQUEST_FILE_NAME = "request.json";

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private final Request request;
    private final BackendInputFile inputFile;
    private final MoDSBackend modsBackend;
    private final InputMetaData inputMetaData;

    public static Simulation createSimulation(Request request) throws JAXBException, IOException {

        String simulationType = request.getSimulationType();
        BackendInputFile inputFile = TemplateLoader.load(simulationType);

        MoDSBackend modsBackend = MoDSBackendFactory.createMoDSBackend();

        OBJECT_MAPPER.writeValue(getRequestFilePath(modsBackend), request);

        load(request, modsBackend);

        InputMetaData inputMetaData = InputMetaData.createInputMetaData(request, modsBackend, request.getAlgorithms().get(0));

        return createSimulation(request, inputFile, modsBackend, inputMetaData);
    }

    public static Simulation retrieveSimulation(Request request) throws JAXBException, IOException {
        String jobID = request.getJobID();

        MoDSBackend modsBackend = MoDSBackendFactory.retrieveMoDSBackend(jobID);

        Request originalRequest = OBJECT_MAPPER.readValue(getRequestFilePath(modsBackend), Request.class);
        BackendInputFile inputFile = new BackendInputFile(
                modsBackend.getWorkingDir().resolve(BackendInputFile.FILENAME));

        InputMetaData inputMetaData = InputMetaData.createInputMetaData(originalRequest, modsBackend,
                originalRequest.getAlgorithms().get(0));

        return createSimulation(originalRequest, inputFile, modsBackend, inputMetaData);
    }

    private static Simulation createSimulation(Request request, BackendInputFile inputFile, MoDSBackend modsBackend,
            InputMetaData inputMetaData)
            throws IOException {

        String simulationType = request.getSimulationType();
        switch (simulationType) {
            case "MOO":
                return new MOO(request, inputFile, modsBackend, inputMetaData);
            case "HDMR":
                return new HDMR(request, inputFile, modsBackend, inputMetaData);
            case "MOOonly":
                return new MOOonly(request, inputFile, modsBackend, inputMetaData);
            case "Evaluate":
                return new Evaluate(request, inputFile, modsBackend, inputMetaData);
            default:
                throw new IllegalArgumentException("Unknown simulation type requested '" + simulationType + "'.");
        }
    }

    private static File getRequestFilePath(MoDSBackend modsBackend) {
        return modsBackend.getSimDir().resolve(REQUEST_FILE_NAME).toFile();
    }

    public Simulation(Request request, BackendInputFile inputFile, MoDSBackend modsBackend, InputMetaData inputMetaData) {
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
        Iterator<Double> minItr = inputMetaData.getMinima().iterator();
        Iterator<Double> maxItr = inputMetaData.getMaxima().iterator();
        for (String name : inputMetaData.getVarNames()) {
            Variable variable = variables.stream().filter(varToTest -> varToTest.getName().equals(name))
                    .findFirst().orElseThrow();
            inputFile.addParameter(name, variable.getSubtype(), variable.getType(),
                    getVariableCases(name), getVariableModels(name),
                    minItr.next(), maxItr.next());
        }
    }

    protected Algorithm getAlgorithmOfType(String algType) {
        return getRequest().getAlgorithms().stream().filter(alg -> alg.getType().equals(algType)).findFirst()
                .orElseThrow();
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
        Data inputs = getRequest().getInputs();
        Path path = modsBackend.getInitialDir().resolve(INITIAL_FILE_NAME);
        new CSVDataFile(inputs.getAverages()).marshal(path);
    }

    protected final void generateInitialFileFromMetaData() throws FileGenerationException {
        Path path = modsBackend.getInitialDir().resolve(INITIAL_FILE_NAME);
        new CSVDataFile(inputMetaData.meansToData()).marshal(path);
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

    protected final void generateSamplingAlgDataFiles() throws FileGenerationException {
        Path samplingAlgDataPath;
        try {
            samplingAlgDataPath = modsBackend.createSubDir(SAMPLING_ALGORITHM_FILE_NAME);

            new CSVDataSeparateFiles(getRequest().getInputs(),
                    SAMPLING_ALGORITHM_FILE_NAME + "_" + Variable.SUBTYPE_PREFIX,
                    getFullCaseName(DEFAULT_CASEGROUP_NAME, DEFAULT_CASE_NAME) + "_")
                    .marshal(samplingAlgDataPath);
        } catch (IOException ex) {
            throw new FileGenerationException(
                    "Failed to create subdirectory for algorithm '" + SAMPLING_ALGORITHM_FILE_NAME + "'.", ex);
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
        request.getAlgorithms().stream()
                .filter(algorithm -> algorithm.getSaveSurrogate() != null && algorithm.getSaveSurrogate())
                .forEach(algorithm -> {
                    Path saveDirectory = getSaveDirectory();
                    Path surrogateDirectory = getSurrogateDirectory(modsBackend);

                    try {
                        copyDirectory(surrogateDirectory, saveDirectory);
                        inputMetaData.writeToCSV(saveDirectory.resolve(InputMetaData.DEFAULT_INPUT_INFO_FILE_NAME));
                    } catch (FileGenerationException ex) {
                        throw new ResponseStatusException(
                                HttpStatus.NO_CONTENT,
                                "Algorithm '" + algorithm.getName() + "' from job '" + getModsBackend().getJobID()
                                        + "' failed to save.",
                                ex);
                    }

                    LOGGER.info("Algorithm '{}' from job '{}' saved at '{}'.", algorithm.getName(),
                            getModsBackend().getJobID(), saveDirectory.toAbsolutePath());
                });
    }

    public static Path getSurrogateDirectory(MoDSBackend modsBackend) {
        return modsBackend.getSimDir().resolve(DEFAULT_SURROGATE_ALGORITHM_NAME);
    }

    private Path getSaveDirectory() {
        return DEFAULT_SURROGATE_SAVE_DIRECTORY_PATH.resolve(modsBackend.getJobID())
                .resolve(DEFAULT_SURROGATE_ALGORITHM_NAME);
    }

    public static void load(Request request, MoDSBackend modsBackend) {
        request.getAlgorithms().stream()
                .filter(algorithm -> algorithm.getSurrogateToLoad() != null)
                .forEach(algorithm -> {
                    try {
                        Path surrogateDirectory = getSurrogateDirectory(modsBackend);
                        Path loadDirectory = getLoadDirectory(algorithm);

                        if (!Files.exists(loadDirectory)) {
                            throw new IOException("File '" + loadDirectory.toAbsolutePath() + "' could not be found to load.");
                        }

                        copyDirectory(loadDirectory, surrogateDirectory);

                        LOGGER.info("File '{}' loaded to '{}'.", loadDirectory.toAbsolutePath(),
                            surrogateDirectory.toAbsolutePath());

                    } catch (IOException ex) {
                        throw new ResponseStatusException(
                                HttpStatus.NO_CONTENT,
                                "Algorithm '" + algorithm.getName() + "' from job '" + modsBackend.getJobID()
                                        + "' failed to load.",
                                ex);
                    }
                });
    }

    private static Path getLoadDirectory(Algorithm algorithm) {
        return DEFAULT_SURROGATE_SAVE_DIRECTORY_PATH
                .resolve(algorithm.getSurrogateToLoad()).resolve(DEFAULT_SURROGATE_ALGORITHM_NAME);
    }

    private static void copyDirectory(Path sourceDirectory, Path destinationDirectory) throws FileGenerationException {
        if (!Files.exists(destinationDirectory)) {
            try {
                Files.createDirectories(destinationDirectory);
            } catch (IOException ex) {
                throw new FileGenerationException(
                        "Failed to create destination directory '" + destinationDirectory.toAbsolutePath() + "'.", ex);
            }
        }

        try (Stream<Path> stream = Files.walk(sourceDirectory)) {

            stream.filter(Files::isRegularFile).forEach(source -> {
                Path destination = destinationDirectory
                        .resolve(source.toString().substring(sourceDirectory.toString().length() + 1));
                try {
                    Files.copy(source, destination);
                } catch (IOException ex) {
                    throw new ResponseStatusException(
                            HttpStatus.NO_CONTENT,
                            "Failed to copy '"
                                    + destinationDirectory + "` to `" + sourceDirectory + "'.",
                            ex);
                }
            });
        } catch (IOException ex) {
            throw new FileGenerationException("Failed to walk source directory '" + sourceDirectory + "'.", ex);
        }
    }

    public final Request getResponse() {
        Request response = new Request();
        response.setJobID(getJobID());
        response.setSimulationType(request.getSimulationType());
        return response;
    }

    public Request getResults() {
        return getResponse();
    }

    public InputMetaData getInputMetaData() {
        return inputMetaData;
    }

}
