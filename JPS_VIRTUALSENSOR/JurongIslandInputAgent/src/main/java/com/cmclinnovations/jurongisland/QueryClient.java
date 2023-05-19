package com.cmclinnovations.jurongisland;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfLiteral;
import it.unibz.inf.ontop.model.vocabulary.GEO;
import org.eclipse.rdf4j.model.vocabulary.RDF;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

/**
 * Execute sparql queries to query and instantiate data
 */
public class QueryClient {
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);
    private StoreClientInterface queryStoreClient;
    private StoreClientInterface updateStoreClient;
    

    static final String PREFIX = "https://www.theworldavatar.com/kg/ontodispersion/";
    public static final String CHEM = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#" ;
    private static final Prefix P_DISP = SparqlBuilder.prefix("disp",iri(PREFIX));
    static final String OM_STRING = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    private static final String ONTO_BUILD = "https://www.theworldavatar.com/kg/ontobuiltenv/" ;
    private static final Prefix P_OM = SparqlBuilder.prefix("om",iri(OM_STRING));
    private static final String OWL_PREFIX = "http://www.w3.org/2002/07/owl#";
    private static final Prefix P_OWL = SparqlBuilder.prefix("owl",iri(OWL_PREFIX));
    private static final Prefix P_CHEM = SparqlBuilder.prefix("chem",iri(CHEM));
    private static final Prefix P_GEO = SparqlBuilder.prefix("geo", iri(GEO.PREFIX));
    private static final Prefix P_BUILD = SparqlBuilder.prefix("build",iri(ONTO_BUILD));

    // classes
    // Since there is only CO2 emissions data for Jurong Island, other pollutants like NO2 and PM are 
    // not listed here. 
    private static final Iri STATIC_POINT_SOURCE = P_DISP.iri("StaticPointSource");
    private static final Iri CO2 = P_DISP.iri("CO2");
    private static final Iri DENSITY = P_OM.iri("Density");
    private static final Iri MASS_FLOW = P_OM.iri("MassFlow");
    private static final Iri TEMPERATURE = P_OM.iri("Temperature");
    private static final Iri OWL_THING = P_OWL.iri("Thing");
    private static final Iri DENSITY_UNIT = P_OM.iri("KilogramPerCubicMetre");
    private static final Iri MASS_FLOW_UNIT = iri("http://www.theworldavatar.com/kb/ontochemplant/TonsPerYear");
    private static final Iri TEMPERATURE_UNIT = P_OM.iri("Kelvin");
    private static final Iri CHEMICALPLANT = P_CHEM.iri("ChemicalPlant") ;
    private static final Iri PLANTITEM = iri("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#PlantItem") ;


    // properties
    private static final Iri HAS_OCGML_OBJECT = P_DISP.iri("hasOntoCityGMLCityObject");
    private static final Iri EMITS = P_DISP.iri("emits");
    private static final Iri HAS_QUANTITY = P_DISP.iri("hasQuantity");
    private static final Iri HAS_UNIT = P_OM.iri("hasUnit");
    private static final Iri HAS_NUMERICALVALUE = P_OM.iri("hasNumericalValue");
    private static final Iri HAS_INDIVIDUALCO2Emission = P_CHEM.iri("hasIndividualCO2Emission");
    private static final Iri CONTAINS = P_GEO.iri("ehContains");
    private static final Iri OCGML_REP = P_BUILD.iri("hasOntoCityGMLRepresentation");

    public QueryClient(StoreClientInterface queryStoreClient, StoreClientInterface updateStoreClient) {
        this.queryStoreClient = queryStoreClient;
        this.updateStoreClient = updateStoreClient;
    }

       /*  SPARQL queries for buildings class */

    public JSONArray pollutantSourceQuery () {

        SelectQuery query = Queries.SELECT().prefix(P_BUILD,P_GEO,P_OM,P_CHEM);
        Variable iri = SparqlBuilder.var("IRI");
        Variable emission = SparqlBuilder.var("emission");
        Variable chemPlant = SparqlBuilder.var("chemPlant");
        Variable plantItem = SparqlBuilder.var("plantItem");
        Variable co2 = SparqlBuilder.var("CO2");

        GraphPattern gp = GraphPatterns.and(chemPlant.has(RDF.TYPE,CHEMICALPLANT).andHas(CONTAINS,plantItem),
                plantItem.has(RDF.TYPE,PLANTITEM).andHas(OCGML_REP,iri).andHas(HAS_INDIVIDUALCO2Emission,co2),
                co2.has(HAS_NUMERICALVALUE,emission));
        query.select(iri,emission).where(gp).limit(Integer.parseInt(EnvConfig.NUMBER_SOURCES));

        return queryStoreClient.executeQuery(query.getQueryString());

    }

    public void updateEmissionsData(JSONArray pollutantSourceData) {

        ModifyQuery modify = Queries.MODIFY().prefix(P_DISP,P_OWL,P_OM);

        for (int i = 0; i < pollutantSourceData.length(); i++) {

            String ocgmlIRI = pollutantSourceData.getJSONObject(i).getString("IRI");
            Double emission = pollutantSourceData.getJSONObject(i).getDouble("emission");
            RdfLiteral.NumericLiteral emissionValue = Rdf.literalOf(emission);
            double density = Double.parseDouble(EnvConfig.DENSITY);
            RdfLiteral.NumericLiteral densityValue = Rdf.literalOf(density);
            double temp = Double.parseDouble(EnvConfig.TEMPERATURE);
            RdfLiteral.NumericLiteral tempValue = Rdf.literalOf(temp);

            String pollutantSourceIRI = PREFIX + "staticpointsource/" + UUID.randomUUID();
            String emissionIRI = PREFIX + "co2/" + UUID.randomUUID();
            String densityIRI = PREFIX + "density/" + UUID.randomUUID();
            String massFlowIRI = PREFIX + "massflow/" + UUID.randomUUID();
            String temperatureIRI = PREFIX + "temperature/" + UUID.randomUUID();

            modify.insert(iri(pollutantSourceIRI).isA(STATIC_POINT_SOURCE).andHas(EMITS,iri(emissionIRI)));
            modify.insert(iri(emissionIRI).isA(CO2).andHas(HAS_QUANTITY,iri(densityIRI))
            .andHas(HAS_QUANTITY,iri(massFlowIRI)).andHas(HAS_QUANTITY,iri(temperatureIRI)));

            modify.insert(iri(emissionIRI).has(HAS_OCGML_OBJECT,iri(ocgmlIRI)));
            modify.insert(iri(ocgmlIRI).isA(OWL_THING));

            // Triples for density, mass flow rate and temperature
            modify.insert(iri(densityIRI).isA(DENSITY).andHas(HAS_NUMERICALVALUE,densityValue).andHas(HAS_UNIT,DENSITY_UNIT));
            modify.insert(iri(massFlowIRI).isA(MASS_FLOW).andHas(HAS_NUMERICALVALUE,emissionValue).andHas(HAS_UNIT,MASS_FLOW_UNIT));
            modify.insert(iri(temperatureIRI).isA(TEMPERATURE).andHas(HAS_NUMERICALVALUE,tempValue).andHas(HAS_UNIT,TEMPERATURE_UNIT));

        }

        updateStoreClient.executeUpdate(modify.getQueryString());

    }

    public void updatePirmasensEmissions() {
        List<String> ocgmlIRIs = Arrays.asList(
            "http://www.theworldavatar.com:83/citieskg/namespace/pirmasensEPSG32633/sparql/cityobject/UUID_LOD2_Pirmasens_4f8d0f1a-3b21-40d4-8b90-89723e31a7ca/",
            "http://www.theworldavatar.com:83/citieskg/namespace/pirmasensEPSG32633/sparql/cityobject/UUID_LOD2_Pirmasens_c38d038b-a677-4e0c-95d9-f02c09cf991c/");

            List<Double> no2Emissions = Arrays.asList(100.0,100.0);

            


    }



}
