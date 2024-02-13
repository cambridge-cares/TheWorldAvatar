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
    public static final String CHEM = "http://www.theworldavatar.com/kg/ontochemplant/";
    private static final Prefix P_DISP = SparqlBuilder.prefix("disp", iri(PREFIX));
    static final String OM_STRING = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    private static final String ONTO_BUILD = "https://www.theworldavatar.com/kg/ontobuiltenv/";
    private static final Prefix P_OM = SparqlBuilder.prefix("om", iri(OM_STRING));
    private static final String OWL_PREFIX = "http://www.w3.org/2002/07/owl#";
    private static final Prefix P_OWL = SparqlBuilder.prefix("owl", iri(OWL_PREFIX));
    private static final Prefix P_CHEM = SparqlBuilder.prefix("chem", iri(CHEM));
    private static final Prefix P_GEO = SparqlBuilder.prefix("geo", iri(GEO.PREFIX));
    private static final Prefix P_BUILD = SparqlBuilder.prefix("build", iri(ONTO_BUILD));

    // classes
    private static final Iri STATIC_POINT_SOURCE = P_DISP.iri("StaticPointSource");
    private static final Iri EMISSION = P_DISP.iri("Emission");
    private static final Iri CO2 = P_DISP.iri("CO2");
    private static final Iri NOx = P_DISP.iri("NOx");
    private static final Iri PM25 = P_DISP.iri("PM2.5");
    private static final Iri PM10 = P_DISP.iri("PM10");
    private static final Iri DENSITY = P_OM.iri("Density");
    private static final Iri MASS_FLOW = P_OM.iri("MassFlow");
    private static final Iri TEMPERATURE = P_OM.iri("Temperature");
    private static final Iri DENSITY_UNIT = P_OM.iri("kilogramPerCubicmetre");
    private static final Iri MASS_FLOW_UNIT = P_OM.iri("kilogramPerSecond-Time");
    private static final Iri TEMPERATURE_UNIT = P_OM.iri("kelvin");
    private static final Iri PLANTITEM = iri(
            "http://www.theworldavatar.com/kg/ontocape/chemicalprocesssystem/cpsrealization/plant/plantitem");
    private static final Iri MEASURE = P_OM.iri("Measure");

    // properties
    private static final Iri HAS_OCGML_OBJECT = P_DISP.iri("hasOntoCityGMLCityObject");
    private static final Iri EMITS = P_DISP.iri("emits");
    private static final Iri HAS_QUANTITY = P_OM.iri("hasQuantity");
    private static final Iri HAS_UNIT = P_OM.iri("hasUnit");
    private static final Iri HAS_NUMERICALVALUE = P_OM.iri("hasNumericalValue");
    private static final Iri HAS_INDIVIDUALCO2Emission = P_CHEM.iri("hasIndividualCO2Emission");
    private static final Iri CONTAINS = P_GEO.iri("ehContains");
    private static final Iri OCGML_REP = P_BUILD.iri("hasOntoCityGMLRepresentation");
    private static final Iri HAS_VALUE = P_OM.iri("hasValue");
    private static final Iri HAS_POLLUTANT_ID = P_DISP.iri("hasPollutantID");

    public QueryClient(StoreClientInterface queryStoreClient, StoreClientInterface updateStoreClient) {
        this.queryStoreClient = queryStoreClient;
        this.updateStoreClient = updateStoreClient;
    }

    /* SPARQL queries for buildings class */

    public JSONArray pollutantSourceQuery() {

        SelectQuery query = Queries.SELECT().prefix(P_BUILD, P_GEO, P_OM, P_CHEM);
        Variable iri = SparqlBuilder.var("IRI");
        Variable emission = SparqlBuilder.var("emission");
        Variable chemPlant = SparqlBuilder.var("chemPlant");
        Variable plantItem = SparqlBuilder.var("plantItem");
        Variable co2 = SparqlBuilder.var("CO2");

        GraphPattern gp = GraphPatterns.and(chemPlant.has(CONTAINS, plantItem),
                plantItem.has(RDF.TYPE, PLANTITEM).andHas(OCGML_REP, iri)
                        .andHas(HAS_INDIVIDUALCO2Emission, co2),
                co2.has(HAS_NUMERICALVALUE, emission));
        query.select(iri, emission).where(gp).limit(Integer.parseInt(EnvConfig.NUMBER_SOURCES));

        return queryStoreClient.executeQuery(query.getQueryString());

    }

    public void updateEmissionsData(JSONArray pollutantSourceData) {

        ModifyQuery modify = Queries.MODIFY().prefix(P_DISP, P_OWL, P_OM);

        double convertTonsYrtoKgS = 1000.0 / (365 * 24 * 60 * 60);

        for (int i = 0; i < pollutantSourceData.length(); i++) {

            String ocgmlIRI = pollutantSourceData.getJSONObject(i).getString("IRI");
            Double emission = pollutantSourceData.getJSONObject(i).getDouble("emission");
            emission *= convertTonsYrtoKgS;
            RdfLiteral.NumericLiteral emissionValue = Rdf.literalOf(emission);
            double density = Double.parseDouble(EnvConfig.DENSITY);
            RdfLiteral.NumericLiteral densityValue = Rdf.literalOf(density);
            double temp = Double.parseDouble(EnvConfig.TEMPERATURE);
            RdfLiteral.NumericLiteral tempValue = Rdf.literalOf(temp);

            String pollutantSourceIRI = PREFIX + "staticpointsource/" + UUID.randomUUID();
            String emissionIRI = PREFIX + "co2/" + UUID.randomUUID();
            String pollutantId = PREFIX + "co2/" + UUID.randomUUID();
            String densityIRI = PREFIX + "density/" + UUID.randomUUID();
            String densityMeasureIRI = PREFIX + "densityMeasure/" + UUID.randomUUID();
            String massFlowIRI = PREFIX + "massflow/" + UUID.randomUUID();
            String massFlowMeasureIRI = PREFIX + "massflowMeasure/" + UUID.randomUUID();
            String temperatureIRI = PREFIX + "temperature/" + UUID.randomUUID();
            String temperatureMeasureIRI = PREFIX + "temperatureMeasure/" + UUID.randomUUID();

            modify.insert(iri(pollutantSourceIRI).isA(STATIC_POINT_SOURCE).andHas(EMITS, iri(emissionIRI)));
            modify.insert(iri(emissionIRI).isA(EMISSION).andHas(HAS_POLLUTANT_ID, iri(pollutantId))
                    .andHas(HAS_QUANTITY, iri(densityIRI))
                    .andHas(HAS_QUANTITY, iri(massFlowIRI))
                    .andHas(HAS_QUANTITY, iri(temperatureIRI)));
            modify.insert(iri(pollutantId).isA(CO2));

            modify.insert(iri(pollutantSourceIRI).has(HAS_OCGML_OBJECT, iri(ocgmlIRI)));

            // Triples for density, mass flow rate and temperature
            modify.insert(iri(densityIRI).isA(DENSITY).andHas(HAS_VALUE, iri(densityMeasureIRI)));
            modify.insert(iri(densityMeasureIRI).isA(MEASURE).andHas(HAS_NUMERICALVALUE, densityValue)
                    .andHas(HAS_UNIT,
                            DENSITY_UNIT));
            modify.insert(iri(massFlowIRI).isA(MASS_FLOW).andHas(HAS_VALUE, iri(massFlowMeasureIRI)));
            modify.insert(iri(massFlowMeasureIRI).isA(MEASURE).andHas(HAS_NUMERICALVALUE, emissionValue)
                    .andHas(HAS_UNIT, MASS_FLOW_UNIT));
            modify.insert(iri(temperatureIRI).isA(TEMPERATURE).andHas(HAS_VALUE,
                    iri(temperatureMeasureIRI)));
            modify.insert(iri(temperatureMeasureIRI).isA(MEASURE).andHas(HAS_NUMERICALVALUE, tempValue)
                    .andHas(
                            HAS_UNIT, TEMPERATURE_UNIT));
        }

        updateStoreClient.executeUpdate(modify.getQueryString());

    }

    public void updatePirmasensEmissions() {
        List<String> ocgmlIRIs = Arrays.asList(
                "https://www.theworldavatar.com/kg/Building/5d030b8f-439d-408a-bae5-c8f4667fdc69",
                "https://www.theworldavatar.com/kg/Building/023a7cee-e39f-4961-91a2-14a5c5b16eee");
        // List<String> ocgmlIRIs = Arrays.asList(
        //         "http://www.theworldavatar.com:83/citieskg/namespace/pirmasensEPSG32633/sparql/building/UUID_LOD2_Pirmasens_4f8d0f1a-3b21-40d4-8b90-89723e31a7ca/",
        //         "http://www.theworldavatar.com:83/citieskg/namespace/pirmasensEPSG32633/sparql/building/UUID_LOD2_Pirmasens_c38d038b-a677-4e0c-95d9-f02c09cf991c/");
        List<Double> noxEmissions = Arrays.asList(100.0, 100.0);
        List<Double> pm25Emissions = Arrays.asList(100.0, 100.0);
        List<Double> pm10Emissions = Arrays.asList(100.0, 100.0);

        double gasDensity = Double.parseDouble(EnvConfig.DENSITY);

        List<Double> noxDensities = Arrays.asList(gasDensity, gasDensity);
        List<Double> pm25Densities = Arrays.asList(gasDensity, gasDensity);
        List<Double> pm10Densities = Arrays.asList(gasDensity, gasDensity);

        int numberSources = 2;
        double convertTonsYrtoKgS = 1000.0 / (365 * 24 * 60 * 60);

        ModifyQuery modify = Queries.MODIFY().prefix(P_DISP, P_OWL, P_OM);

        for (int i = 0; i < numberSources; i++) {

            String ocgmlIRI = ocgmlIRIs.get(i);
            Double noxEmission = noxEmissions.get(i) * convertTonsYrtoKgS;
            RdfLiteral.NumericLiteral noxEmissionValue = Rdf.literalOf(noxEmission);
            Double pm25Emission = pm25Emissions.get(i) * convertTonsYrtoKgS;
            RdfLiteral.NumericLiteral pm25EmissionValue = Rdf.literalOf(pm25Emission);
            Double pm10Emission = pm10Emissions.get(i) * convertTonsYrtoKgS;
            RdfLiteral.NumericLiteral pm10EmissionValue = Rdf.literalOf(pm10Emission);

            Double noxDensity = noxDensities.get(i);
            RdfLiteral.NumericLiteral noxDensityValue = Rdf.literalOf(noxDensity);
            Double pm25Density = pm25Densities.get(i);
            RdfLiteral.NumericLiteral pm25DensityValue = Rdf.literalOf(pm25Density);
            Double pm10Density = pm10Densities.get(i);
            RdfLiteral.NumericLiteral pm10DensityValue = Rdf.literalOf(pm10Density);

            double temp = Double.parseDouble(EnvConfig.TEMPERATURE);
            RdfLiteral.NumericLiteral tempValue = Rdf.literalOf(temp);

            String pollutantSourceIRI = PREFIX + "staticpointsource/" + UUID.randomUUID();
            String temperatureIRI = PREFIX + "temperature/" + UUID.randomUUID();
            String temperatureMeasureIRI = PREFIX + "temperatureMeasure/" + UUID.randomUUID();
            String noxEmissionIRI = PREFIX + "nox/" + UUID.randomUUID();
            String pm25EmissionIRI = PREFIX + "pm25/" + UUID.randomUUID();
            String pm10EmissionIRI = PREFIX + "pm10/" + UUID.randomUUID();
            String noxPollutantId = PREFIX + "nox/" + UUID.randomUUID();
            String pm25PollutantId = PREFIX + "pm25/" + UUID.randomUUID();
            String pm10PollutantId = PREFIX + "pm10/" + UUID.randomUUID();
            String noxDensityIRI = PREFIX + "noxdensity/" + UUID.randomUUID();
            String noxDensityMeasureIRI = PREFIX + "noxdensityMeasure/" + UUID.randomUUID();
            String noxMassFlowIRI = PREFIX + "noxmassflow/" + UUID.randomUUID();
            String noxMassFlowMeasureIRI = PREFIX + "noxmassflowMeasure/" + UUID.randomUUID();
            String pm25DensityIRI = PREFIX + "pm25density/" + UUID.randomUUID();
            String pm25DensityMeasureIRI = PREFIX + "pm25densityMeasure/" + UUID.randomUUID();
            String pm25MassFlowIRI = PREFIX + "pm25massflow/" + UUID.randomUUID();
            String pm25MassFlowMeasureIRI = PREFIX + "pm25massflowMeasure/" + UUID.randomUUID();
            String pm10DensityIRI = PREFIX + "pm10density/" + UUID.randomUUID();
            String pm10DensityMeasureIRI = PREFIX + "pm10densityMeasure/" + UUID.randomUUID();
            String pm10MassFlowIRI = PREFIX + "pm10massflow/" + UUID.randomUUID();
            String pm10MassFlowMeasureIRI = PREFIX + "pm10massflowMeasure/" + UUID.randomUUID();

            // Generic set of triples for a single emission source. Assign same temperature
            // to all pollutants.
            // However, density and mass flow rate will vary.
            modify.insert(iri(pollutantSourceIRI).isA(STATIC_POINT_SOURCE)
                    .andHas(EMITS, iri(noxEmissionIRI))
                    .andHas(EMITS, iri(pm25EmissionIRI)).andHas(EMITS, iri(pm10EmissionIRI))
                    .andHas(HAS_OCGML_OBJECT, iri(ocgmlIRI)));
            modify.insert(iri(temperatureIRI).isA(TEMPERATURE).andHas(HAS_VALUE,
                    iri(temperatureMeasureIRI)));
            modify.insert(iri(temperatureMeasureIRI).isA(MEASURE).andHas(HAS_NUMERICALVALUE, tempValue)
                    .andHas(
                            HAS_UNIT, TEMPERATURE_UNIT));

            // Triples for specific pollutants (nox, PM2.5, PM10)
            modify.insert(iri(noxEmissionIRI).isA(EMISSION).andHas(HAS_POLLUTANT_ID, iri(noxPollutantId))
                    .andHas(HAS_QUANTITY, iri(noxDensityIRI))
                    .andHas(HAS_QUANTITY, iri(noxMassFlowIRI))
                    .andHas(HAS_QUANTITY, iri(temperatureIRI)));
            modify.insert(iri(noxPollutantId).isA(NOx));
            modify.insert(iri(noxDensityIRI).isA(DENSITY).andHas(HAS_VALUE, iri(noxDensityMeasureIRI)));
            modify.insert(iri(noxDensityMeasureIRI).isA(MEASURE).andHas(HAS_NUMERICALVALUE, noxDensityValue)
                    .andHas(HAS_UNIT, DENSITY_UNIT));
            modify.insert(iri(noxMassFlowIRI).isA(MASS_FLOW).andHas(HAS_VALUE, iri(noxMassFlowMeasureIRI)));
            modify.insert(iri(noxMassFlowMeasureIRI).isA(MEASURE)
                    .andHas(HAS_NUMERICALVALUE, noxEmissionValue)
                    .andHas(HAS_UNIT, MASS_FLOW_UNIT));

            modify.insert(iri(pm25EmissionIRI).isA(EMISSION).andHas(HAS_POLLUTANT_ID, iri(pm25PollutantId))
                    .andHas(HAS_QUANTITY, iri(pm25DensityIRI))
                    .andHas(HAS_QUANTITY, iri(pm25MassFlowIRI))
                    .andHas(HAS_QUANTITY, iri(temperatureIRI)));
            modify.insert(iri(pm25PollutantId).isA(PM25));
            modify.insert(iri(pm25DensityIRI).isA(DENSITY).andHas(HAS_VALUE, iri(pm25DensityMeasureIRI)));
            modify.insert(iri(pm25DensityMeasureIRI).isA(MEASURE)
                    .andHas(HAS_NUMERICALVALUE, pm25DensityValue)
                    .andHas(HAS_UNIT, DENSITY_UNIT));
            modify.insert(iri(pm25MassFlowIRI).isA(MASS_FLOW).andHas(HAS_VALUE,
                    iri(pm25MassFlowMeasureIRI)));
            modify.insert(iri(pm25MassFlowMeasureIRI).isA(MEASURE)
                    .andHas(HAS_NUMERICALVALUE, pm25EmissionValue)
                    .andHas(HAS_UNIT, MASS_FLOW_UNIT));

            modify.insert(iri(pm10EmissionIRI).isA(EMISSION).andHas(HAS_POLLUTANT_ID, iri(pm10PollutantId))
                    .andHas(HAS_QUANTITY, iri(pm10DensityIRI))
                    .andHas(HAS_QUANTITY, iri(pm10MassFlowIRI))
                    .andHas(HAS_QUANTITY, iri(temperatureIRI)));
            modify.insert(iri(pm10PollutantId).isA(PM10));
            modify.insert(iri(pm10DensityIRI).isA(DENSITY).andHas(HAS_VALUE, iri(pm10DensityMeasureIRI)));
            modify.insert(iri(pm10DensityMeasureIRI).isA(MEASURE)
                    .andHas(HAS_NUMERICALVALUE, pm10DensityValue)
                    .andHas(HAS_UNIT, DENSITY_UNIT));
            modify.insert(iri(pm10MassFlowIRI).isA(MASS_FLOW).andHas(HAS_VALUE,
                    iri(pm10MassFlowMeasureIRI)));
            modify.insert(iri(pm10MassFlowMeasureIRI).isA(MEASURE)
                    .andHas(HAS_NUMERICALVALUE, pm10EmissionValue)
                    .andHas(HAS_UNIT, MASS_FLOW_UNIT));

        }

        updateStoreClient.executeUpdate(modify.getQueryString());

    }

}