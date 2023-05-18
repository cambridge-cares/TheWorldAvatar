package com.cmclinnovations.jurongisland;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.derivation.Derivation;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.postgis.Point;

/**
 * Execute sparql queries to query and instantiate data
 */
public class QueryClient {
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);
    private StoreClientInterface queryStoreClient;
    private StoreClientInterface updateStoreClient;
    

    static final String PREFIX = "https://www.theworldavatar.com/kg/ontodispersion/";
    private static final Prefix P_DISP = SparqlBuilder.prefix("disp",iri(PREFIX));
    static final String OM_STRING = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    private static final Prefix P_OM = SparqlBuilder.prefix("om",iri(OM_STRING));
    private static final String OWL_PREFIX = "http://www.w3.org/2002/07/owl#";
    private static final Prefix P_OWL = SparqlBuilder.prefix("owl",iri(OWL_PREFIX));

    // classes
    // Since there is only CO2 emissions data for Jurong Island, other pollutants like NO2 and PM are 
    // not listed here. 
    private static final Iri StaticPointSource = P_DISP.iri("StaticPointSource");
    private static final Iri CO2 = P_DISP.iri("CO2");
    private static final Iri Density = P_OM.iri("Density");
    private static final Iri MassFlow = P_OM.iri("MassFlow");
    private static final Iri Temperature = P_OM.iri("Temperature");
    private static final Iri OWL_THING = P_OWL.iri("Thing");
    private static final Iri DENSITY_UNIT = P_OM.iri("KilogramPerCubicMetre");
    private static final Iri MASS_FLOW_UNIT = iri("http://www.theworldavatar.com/kb/ontochemplant/TonsPerYear");
    private static final Iri TEMPERATURE_UNIT = P_OM.iri("Kelvin");

    // properties
    private static final Iri ASSOCIATED_OCGML_OBJECT = P_DISP.iri("associatedOntoCityGMLCityObject");
    private static final Iri EMITS = P_DISP.iri("emits");
    private static final Iri HAS_QUANTITY = P_DISP.iri("hasQuantity");
    private static final Iri HAS_UNIT = P_OM.iri("hasUnit");
    private static final Iri HAS_NUMERICALVALUE = P_OM.iri("hasNumericalValue");
    

    public QueryClient(StoreClientInterface queryStoreClient, StoreClientInterface updateStoreClient) {
        this.queryStoreClient = queryStoreClient;
        this.updateStoreClient = updateStoreClient;
    }

       /*  SPARQL queries for buildings class */

    public JSONArray pollutantSourceQuery () {

        SelectQuery query = Queries.SELECT().prefix(P_GEO,P_BUILD,P_OM,P_CHEM);
        Variable iri = SparqlBuilder.var("IRI");
        Variable emission = SparqlBuilder.var("emission");
        Variable chemPlant = SparqlBuilder.var("chemPlant");
        Variable plantItem = SparqlBuilder.var("plantItem");
        Variable co2 = SparqlBuilder.var("CO2");

        GraphPattern gp = GraphPatterns.and(chemPlant.has(RDF.TYPE,CHEMICALPLANT).andHas(CONTAINS,plantItem),
                plantItem.has(RDF.TYPE,PLANTITEM).andHas(OCGML_REP,iri).andHas(HAS_INDIVIDUALCO2Emission,co2),
                co2.has(HAS_NUMERICALVALUE,emission));
        query.select(IRI,emission).where(gp).limit(Integer.parseInt(EnvConfig.NUMBER_SOURCES));

        return storeClient.executeQuery(query.getQueryString());

    }

    public void updateEmissionsData(JSONArray pollutantSourceData) {

        ModifyQuery modify = Queries.MODIFY();

        for (int i = 0; i < pollutantSourceData.length; i++) {

            String ocgmlIRI = pollutantSourceData.get(i).getString("IRI");
            Double emission = pollutantSourceData.get(i).getDouble("CO2");
            double densityValue = 1.0;
            double tempValue = 500.0;

            String pollutantSourceIRI = PREFIX + "/staticpointsource/" + UUID.randomUUID();
            String emissionIRI = PREFIX + "/co2/" + UUID.randomUUID();
            String densityIRI = PREFIX + "/density/" + UUID.randomUUID();
            String massFlowIRI = PREFIX + "/massflow/" + UUID.randomUUID();
            String temperatureIRI = PREFIX + "/temperature/" + UUID.randomUUID();

            modify.insert(iri(pollutantSourceIRI).isA(StaticPointSource).andHas(EMITS,iri(emissionIRI)));
            modify.insert(iri(emissionIRI).isA(CO2).andHas(HAS_QUANTITY,iri(densityIRI))
            .andHas(HAS_QUANTITY,iri(massFlowIRI)).andHas(HAS_QUANTITY,iri(temperatureIRI)));

            modify.insert(iri(emissionIRI).has(ASSOCIATED_OCGML_OBJECT,iri(ocgmlIRI)));
            modify.insert(iri(ocgmlIRI).isA(OWL_THING));

            // Triples for density, mass flow rate and temperature
            modify.insert(iri(densityIRI).isA(Density).andHas(HAS_NUMERICALVALUE,densityValue).andHas(HAS_UNIT,DENSITY_UNIT));
            modify.insert(iri(massFlowIRI).isA(MassFlow).andHas(HAS_NUMERICALVALUE,emission).andHas(HAS_UNIT,MASS_FLOW_UNIT));
            modify.insert(iri(temperatureIRI).isA(Temperature).andHas(HAS_NUMERICALVALUE,tempValue).andHas(HAS_UNIT,TEMPERATURE_UNIT));

            updateStoreClient.executeUpdate(modify.getQueryString());


        }

    }



}
