package com.cmclinnovations.mods.modssimpleagent.simulations;

import java.io.IOException;

import com.cmclinnovations.mods.modssimpleagent.BackendInputFile;
import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Algorithm;
import com.cmclinnovations.mods.modssimpleagent.datamodels.InputMetaData;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;

class Sensitivity extends Simulation {

        public Sensitivity(Request request, BackendInputFile inputFile, MoDSBackend modsBackend,
                        InputMetaData inputMetaData) {
                super(request, inputFile, modsBackend, inputMetaData);
        }

        @Override
        protected Algorithm getPrimaryAlgorithm() {
                return getAlgorithmOfType("GenSurrogateAlg");
        }

        @Override
        public void run() throws IOException {
                populateInputFile();
                generateFiles();
        }

        @Override
        protected void generateFiles() throws FileGenerationException {
                generateInitialFileFromMetaData();
                super.generateFiles();
        }

        @Override
        public Request getResponse() {
                return super.getResponse().toBuilder().sensitivities(getSensitivity()).build();
        }
}
