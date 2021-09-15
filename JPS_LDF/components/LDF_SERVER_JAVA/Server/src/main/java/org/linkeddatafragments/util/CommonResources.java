package org.linkeddatafragments.util;

import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.ResourceFactory;

/**
 *
 * @author mielvandersande
 */
@SuppressWarnings("javadoc")
/**
 * All common URIs needed for parsing and serializations
 */
public class CommonResources {

    /**
     *
     */
    public final static String RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";

    /**
     *
     */
    public final static Property RDF_TYPE = createProperty(RDF + "type");

    /**
     *
     */
    public final static Property RDF_SUBJECT = createProperty(RDF + "subject");

    /**
     *
     */
    public final static Property RDF_PREDICATE = createProperty(RDF + "predicate");

    /**
     *
     */
    public final static Property RDF_OBJECT = createProperty(RDF + "object");

    /**
     *
     */
    public final static String VOID = "http://rdfs.org/ns/void#";

    /**
     *
     */
    public final static Property VOID_TRIPLES = createProperty(VOID + "triples");

    /**
     *
     */
    public final static Property VOID_SUBSET = createProperty(VOID + "subset");

    /**
     *
     */
    public final static Property VOID_DATASET = createProperty(VOID + "Dataset");

    /**
     *
     */
    public final static String HYDRA = "http://www.w3.org/ns/hydra/core#";

    /**
     *
     */
    public final static Property HYDRA_TOTALITEMS = createProperty(HYDRA + "totalItems");

    /**
     *
     */
    public final static Property HYDRA_ITEMSPERPAGE = createProperty(HYDRA + "itemsPerPage");

    /**
     *
     */
    public final static Property HYDRA_SEARCH = createProperty(HYDRA + "search");

    /**
     *
     */
    public final static Property HYDRA_TEMPLATE = createProperty(HYDRA + "template");

    /**
     *
     */
    public final static Property HYDRA_MAPPING = createProperty(HYDRA + "mapping");

    /**
     *
     */
    public final static Property HYDRA_VARIABLE = createProperty(HYDRA + "variable");

    /**
     *
     */
    public final static Property HYDRA_PROPERTY = createProperty(HYDRA + "property");

    /**
     *
     */
    public final static Property HYDRA_COLLECTION = createProperty(HYDRA + "Collection");

    /**
     *
     */
    public final static Property HYDRA_PAGEDCOLLECTION = createProperty(HYDRA + "PagedCollection");

    /**
     *
     */
    public final static Property HYDRA_FIRSTPAGE = createProperty(HYDRA + "firstPage");

    /**
     *
     */
    public final static Property HYDRA_LASTPAGE = createProperty(HYDRA + "lastPage");

    /**
     *
     */
    public final static Property HYDRA_NEXTPAGE = createProperty(HYDRA + "nextPage");

    /**
     *
     */
    public final static Property HYDRA_PREVIOUSPAGE = createProperty(HYDRA + "previousPage");

    /**
     *
     */
    public final static Property INVALID_URI = createProperty("urn:invalid");

    private static Property createProperty(String uri) {
        return ResourceFactory.createProperty(uri);
    }
}
