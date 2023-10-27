package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;

public class RDF extends DataSubset {

    @Override
    public boolean usesBlazegraph() {
        return !isSkip();
    }

    @Override
    void loadInternal(Dataset parent) {
        Path subdirectory = this.getSubdirectory();
        if (null == subdirectory) {
            throw new RuntimeException("No 'subdirectory' specified - required for RDF data");
        }
        BlazegraphClient.getInstance().uploadRDFFiles(
                parent.getDirectory().resolve(subdirectory), parent.getNamespace());
    }

}
