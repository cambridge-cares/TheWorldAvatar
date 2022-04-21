package com.cmclinnovations.mods.modssimpleagent.simulations;

import java.io.IOException;
import java.util.List;

import com.cmclinnovations.mods.modssimpleagent.BackendInputFile;
import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Variable;

class MOO extends Simulation {

    public MOO(Request request, BackendInputFile inputFile, MoDSBackend modsBackend) throws IOException {
        super(request, inputFile, modsBackend);
    }

    @Override
    protected void populateAlgorithmNodes(List<Variable> variables) {
        populateMOOAlgorithmNode(Simulation.DEFAULT_MOO_ALGORITHM_NAME, variables);
        super.populateAlgorithmNodes(variables);
    }

    @Override
    protected void generateFiles() throws FileGenerationException {
        generateInitialFile();
        generateDataAlgFiles();
        super.generateFiles();
    }

}
