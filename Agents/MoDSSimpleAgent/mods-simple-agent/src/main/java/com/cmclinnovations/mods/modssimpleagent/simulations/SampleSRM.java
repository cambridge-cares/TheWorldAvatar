package com.cmclinnovations.mods.modssimpleagent.simulations;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import com.cmclinnovations.mods.api.MoDSAPI;
import com.cmclinnovations.mods.api.MoDSAPI.DataType;
import com.cmclinnovations.mods.api.Options;
import com.cmclinnovations.mods.modssimpleagent.BackendInputFile;
import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Algorithm;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Data;
import com.cmclinnovations.mods.modssimpleagent.datamodels.DataColumn;
import com.cmclinnovations.mods.modssimpleagent.datamodels.InputMetaData;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Variable;
import com.cmclinnovations.mods.modssimpleagent.datamodels.ModelInput;
import com.cmclinnovations.mods.modssimpleagent.utils.SimulationLoader;
import com.cmclinnovations.mods.modssimpleagent.utils.SimulationSaver;
import com.google.common.collect.Streams;

class SampleSRM extends Simulation {

    public SampleSRM(Request request, BackendInputFile inputFile, MoDSBackend modsBackend, InputMetaData inputMetaData,
            SimulationSaver simulationSaver, SimulationLoader simulationLoader) {
        super(request, inputFile, modsBackend, inputMetaData, simulationSaver, simulationLoader);
    }

    @Override
    protected Algorithm getPrimaryAlgorithm() {
        return getAlgorithmOfType("SamplingAlg");
    }

    @Override
    protected void populateModelNodes() {
        String type = "Executable";
        for (String modelName : getModels()) {
            getInputFile().addModel(modelName, type, "bash","\"${MODS_BIN_DIR}/runKineticsSRM.sh\"");
        }
    }

    @Override
    protected void populateFileNodes() {
        super.populateFileNodes();
        getInputFile().addInputParams();
    }

    @Override
    protected void populateParameterNodes(List<Variable> variables) {
        Iterator<Double> minItr = getInputMetaData().getMinima().iterator();
        Iterator<Double> maxItr = getInputMetaData().getMaxima().iterator();
        for (String name : getInputMetaData().getVarNames()) {
            Variable variable = variables.stream().filter(varToTest -> varToTest.name().equals(name)).findFirst()
                    .orElseThrow();
            getInputFile().addParameter(name, variable.getSubtype(), variable.type(), getVariableCases(name),
                    getVariableModels(name), minItr.next(), maxItr.next(), variable.path());
        }
        for (Variable variable:variables) {
            if (variable.type().equals("output")) {
                getInputFile().addParameter(variable.name(), variable.getSubtype(), variable.type(), getVariableCases(variable.name()),
                    getVariableModels(variable.name()),variable.nParams(),variable.initialReadDetail(), variable.workingReadDetail());
            }
        }
    }

    @Override
    protected void generateFiles() throws FileGenerationException {
        generateInitialFileFromMetaData();
        generateSamplingAlgDataFiles();
        generateSpecialInputFiles();
        super.generateFiles();
    }

    @Override
    public void run() throws IOException {
        populateInputFile();
        generateFiles();
        getModsBackend().run();
    }

    private void generateSpecialInputFiles() throws FileGenerationException {
        for (ModelInput modelinput : getRequest().modelinputs()) {
            modelinput.marshal(getModsBackend().getSimDir());
        }

    }

    @Override
    public Request getResults() {

        String simDir = getModsBackend().getSimDir().toString();
        String algorithmName = Simulation.DEFAULT_SAMPLING_ALGORITHM_NAME;

        if (!MoDSAPI.hasAlgorithmGeneratedOutputFiles(simDir, algorithmName)) {
            throw new ResponseStatusException(
                    HttpStatus.NO_CONTENT,
                    "The multi-objective optimisation job with job '" + getModsBackend().getJobID()
                            + "' has not finished yet, has not been run or has failed to run correctly.");
        }

        List<String> outputVarNames = MoDSAPI.getReducedYVarIDs(simDir, algorithmName).stream()
                .map(MoDSAPI::getVarName)
                .collect(Collectors.toList());

        List<List<Double>> points = MoDSAPI.getPointsInMap(simDir, algorithmName, new Options().setVarIndexFirst(false))
                .get(DataType.OutputVariable);

        Data outputValues = new Data(
                Streams.zip(outputVarNames.stream(), points.stream(), DataColumn::new)
                        .collect(Collectors.toList()));

        return super.getResults().toBuilder().outputs(outputValues).build();
    }
}
