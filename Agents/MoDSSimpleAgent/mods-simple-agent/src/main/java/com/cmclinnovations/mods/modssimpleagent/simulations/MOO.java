package com.cmclinnovations.mods.modssimpleagent.simulations;

import java.io.IOException;

import com.cmclinnovations.mods.modssimpleagent.BackendInputFile;
import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.datamodels.InputInfo;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;

class MOO extends MOOonly {

        public MOO(Request request, BackendInputFile inputFile, MoDSBackend modsBackend, InputInfo inputInfo)
                        throws IOException {
                super(request, inputFile, modsBackend, inputInfo);
        }

        @Override
        protected void generateFiles() throws FileGenerationException {
                generateDataAlgFiles();
                super.generateFiles();
        }
}
