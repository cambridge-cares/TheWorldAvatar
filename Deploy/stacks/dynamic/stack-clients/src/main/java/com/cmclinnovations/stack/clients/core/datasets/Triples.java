package com.cmclinnovations.stack.clients.core.datasets;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;

public class Triples extends DataSubset {

    @Override
    void loadInternal(Dataset parent) {
        BlazegraphClient.getInstance().uploadRDFFiles(parent.getDirectory(), parent.getNamespace());
    }

}
