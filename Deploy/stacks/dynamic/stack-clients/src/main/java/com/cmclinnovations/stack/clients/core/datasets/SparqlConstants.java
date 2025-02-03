package com.cmclinnovations.stack.clients.core.datasets;

import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;

public class SparqlConstants {
    private SparqlConstants() {
    }

    public static final String DEFAULT_BASE_IRI = "https://theworldavatar.io/kg/";
    public static final Iri BLAZEGRAPH_SERVICE = Rdf.iri(DEFAULT_BASE_IRI + "service#Blazegraph");
    public static final Iri POSTGIS_SERVICE = Rdf.iri(DEFAULT_BASE_IRI + "service#PostGIS");
    public static final Iri GEOSERVER_SERVICE = Rdf.iri(DEFAULT_BASE_IRI + "service#GeoServer");
    public static final Iri ONTOP_SERVICE = Rdf.iri(DEFAULT_BASE_IRI + "service#Ontop");
}