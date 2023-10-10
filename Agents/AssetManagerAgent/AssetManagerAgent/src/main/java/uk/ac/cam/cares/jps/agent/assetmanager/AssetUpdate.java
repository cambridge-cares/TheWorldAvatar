package uk.ac.cam.cares.jps.agent.assetmanager;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.jooq.DeleteQuery;
import org.json.JSONArray;
import org.json.JSONObject;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import static uk.ac.cam.cares.jps.agent.assetmanager.ClassAndProperties.*;
import static uk.ac.cam.cares.jps.agent.assetmanager.QueryUtil.*;

public class AssetUpdate {
    private RemoteStoreClient storeClientAsset, storeClientDevice, storeClientPurchDoc;
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(AssetManagerAgent.class);
    public AssetUpdate (RemoteStoreClient clientAsset, RemoteStoreClient clientDevice, RemoteStoreClient clientPurchDoc) {
        storeClientAsset = clientAsset;
        storeClientDevice = clientDevice;
        storeClientPurchDoc = clientPurchDoc;
    }

    public void updateByLiteral (){
        ModifyQuery query = Queries.DELETE();
        
        
    }
    
}
