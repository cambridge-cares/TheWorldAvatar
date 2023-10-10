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

public class AssetDelete {
    private RemoteStoreClient storeClientAsset, storeClientDevice, storeClientPurchDoc;
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(AssetManagerAgent.class);
    public AssetDelete (RemoteStoreClient clientAsset, RemoteStoreClient clientDevice, RemoteStoreClient clientPurchDoc) {
        storeClientAsset = clientAsset;
        storeClientDevice = clientDevice;
        storeClientPurchDoc = clientPurchDoc;
    }

    public void deleteByLiteral (String literal, String predicate, RemoteStoreClient storeClient){
        deleteByLiteral (literal, iri(predicate), storeClient);
    }

    public void deleteByLiteral (String literal, Iri predicate, RemoteStoreClient storeClient){
        ModifyQuery query = Queries.DELETE();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable subject = SparqlBuilder.var("Subject");

        try {
            query.delete(subject.has(predicate, literal));
            query.where(subject.has(predicate, literal));

            storeClient.executeUpdate(query.getQueryString());
        } catch (Exception e) {
            LOGGER.error("Failed to delete literal: "+ literal , e);
            throw new JPSRuntimeException("Failed to delete literal: "+ literal , e);
        }

        
    }
    
}
