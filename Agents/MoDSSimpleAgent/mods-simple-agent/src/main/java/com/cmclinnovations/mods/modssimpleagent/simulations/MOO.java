package com.cmclinnovations.mods.modssimpleagent.simulations;

import com.cmclinnovations.mods.modssimpleagent.BackendInputFile;
import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.datamodels.InputMetaData;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;
import com.cmclinnovations.mods.modssimpleagent.utils.SimulationLoader;
import com.cmclinnovations.mods.modssimpleagent.utils.SimulationSaver;

class MOO extends MOOonly {

        public MOO(Request request, BackendInputFile inputFile, MoDSBackend modsBackend, InputMetaData inputMetaData,
                        SimulationSaver simulationSaver, SimulationLoader simulationLoader) {
                super(request, inputFile, modsBackend, inputMetaData, simulationSaver, simulationLoader);
        }

        @Override
        protected void generateFiles() throws FileGenerationException {
                generateDataAlgFiles();
                super.generateFiles();
        }
}
