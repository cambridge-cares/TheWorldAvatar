package com.cmclinnovations.mods.modssimpleagent.simulations;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import com.cmclinnovations.mods.api.MoDSAPI;
import com.cmclinnovations.mods.api.Options;
import com.cmclinnovations.mods.api.MoDSAPI.DataType;
import com.cmclinnovations.mods.modssimpleagent.BackendInputFile;
import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Algorithm;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Data;
import com.cmclinnovations.mods.modssimpleagent.datamodels.DataColumn;
import com.cmclinnovations.mods.modssimpleagent.datamodels.InputMetaData;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;
import com.google.common.collect.Streams;

class Evaluate extends Simulation {

    public Evaluate(Request request, BackendInputFile inputFile, MoDSBackend modsBackend, InputMetaData inputMetaData)
            throws IOException {
        super(request, inputFile, modsBackend, inputMetaData);
    }

    @Override
    protected Algorithm getPrimaryAlgorithm() {
        return getAlgorithmOfType("SamplingAlg");
    }

    @Override
    protected void generateFiles() throws FileGenerationException {
        generateInitialFileFromMetaData();
        generateSamplingAlgDataFiles();
        super.generateFiles();
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

        List<List<Double>> points = MoDSAPI.getPointsInMap(simDir, algorithmName, new Options().setVarIndexFirst(true))
                .get(DataType.OutputVariable);

        Data outputValues = new Data(
                Streams.zip(outputVarNames.stream(), points.stream(), DataColumn::new)
                        .collect(Collectors.toList()));

        return super.getResults().toBuilder().outputs(outputValues).build();
    }
}
