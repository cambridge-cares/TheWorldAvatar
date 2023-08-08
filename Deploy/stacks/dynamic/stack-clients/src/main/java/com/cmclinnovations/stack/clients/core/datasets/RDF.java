package com.cmclinnovations.stack.clients.core.datasets;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;

public class RDF extends DataSubset {

    @Override
    public boolean usesBlazegraph() {
        return !isSkip();
    }

    @Override
    void loadInternal(Dataset parent) {
        BlazegraphClient.getInstance().uploadRDFFiles(
                parent.getDirectory().resolve(this.getSubdirectory()), parent.getNamespace());
    }

}
