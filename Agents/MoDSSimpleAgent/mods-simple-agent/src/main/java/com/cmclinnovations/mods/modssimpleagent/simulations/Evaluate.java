package com.cmclinnovations.mods.modssimpleagent.simulations;

import java.io.IOException;

import com.cmclinnovations.mods.modssimpleagent.BackendInputFile;
import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Algorithm;
import com.cmclinnovations.mods.modssimpleagent.datamodels.InputMetaData;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;

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

}
