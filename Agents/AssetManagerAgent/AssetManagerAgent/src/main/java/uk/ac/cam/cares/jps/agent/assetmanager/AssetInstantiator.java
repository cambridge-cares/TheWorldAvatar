package uk.ac.cam.cares.jps.agent.assetmanager;


import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.jooq.False;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.model.vocabulary.RDFS;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


/**
 * Class for creating queries and instantiating asset triples
 */
public class AssetInstantiator {
    private RemoteStoreClient storeClient;

    // prefix
	private static final String ONTODEV = "https://www.theworldavatar.com/kg/ontodevice/";
    private static final String ONTOLAB = "https://www.theworldavatar.com/kg/ontolab/";
    private static final String ONTOSYSTEM = "https://www.theworldavatar.com/kg/ontosystem/";
    private static final String ONTOINMA = "https://www.theworldavatar.com/kg/ontoinma/";
    private static final Prefix P_DEV = SparqlBuilder.prefix("ontodevice",iri(ONTODEV));
    private static final Prefix P_LAB = SparqlBuilder.prefix("ontodevice",iri(ONTOLAB));
    private static final Prefix P_SYS = SparqlBuilder.prefix("ontodevice",iri(ONTOSYSTEM));
    private static final Prefix P_INMA = SparqlBuilder.prefix("ontodevice",iri(ONTOINMA));
    private static final Prefix P_SAREF = SparqlBuilder.prefix("saref", iri("https://saref.etsi.org/core/"));

    //properties
    private static final Iri consistsOf = P_SAREF.iri("consistsOf");

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(AssetManagerAgent.class);

    //constructor
    public AssetInstantiator() {
        
    }

    //asset instantiator
    
    
}