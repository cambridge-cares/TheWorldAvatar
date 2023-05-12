package com.cmclinnovations.mods.modssimpleagent.simulations;

import java.io.IOException;
import java.util.List;

import com.cmclinnovations.mods.modssimpleagent.BackendInputFile;
import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.datamodels.InputMetaData;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Variable;
import com.cmclinnovations.mods.modssimpleagent.utils.SimulationSaver;

public class MCDM extends Simulation {

    public MCDM(Request request, BackendInputFile inputFile, MoDSBackend modsBackend, InputMetaData inputMetaData,
            SimulationSaver simulationSaver) {
        super(request, inputFile, modsBackend, inputMetaData, simulationSaver);
    }

    @Override
    public void run() throws IOException {
        populateInputFile();
        generateFiles();
    }

    @Override
    protected void populateAlgorithmNodes(List<Variable> variables) {
        populateMOOAlgorithmNode(Simulation.DEFAULT_MOO_ALGORITHM_NAME, getPrimaryAlgorithm().name(), variables);
        super.populateAlgorithmNodes(variables);
    }

    @Override
    protected void generateFiles() throws FileGenerationException {
        generateMOOAlgDataFiles();
        generateInitialFileFromInputs();
        super.generateFiles();
    }

    @Override
    public Request getResponse() {
        return getMCDMResults();
    }
}