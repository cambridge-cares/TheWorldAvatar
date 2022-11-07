package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

import uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.HistoricalHouse45UtilitiesAgent;

import java.util.UUID;

/**
 * Creates the required INSERT Query statements.
 *
 * @author qhouyee
 */
class InsertQueryBuilder extends QueryBuilderNode {
    /**
     * Add the statements to link the building instance to their CityGML IRI counterpart.
     *
     * @param insertQueryBuilder INSERT DATA query generated using a String builder.
     */
    protected static void addOntoBuiltEnvInsertStatements(StringBuilder insertQueryBuilder) {
        if (!HistoricalHouse45UtilitiesAgent.ontoCityGMLBuildingIRI.isEmpty() && !OntoBimAdapter.BUILDING_INST.isEmpty()) {
            insertQueryBuilder.append(OPEN_ANCHOR + OntoBimAdapter.BUILDING_INST + CLOSED_ANCHOR + WHITESPACE
                    + ONTOBUILTENV_HAS_CITYGML_REP + WHITESPACE
                    + OPEN_ANCHOR+ HistoricalHouse45UtilitiesAgent.ontoCityGMLBuildingIRI + CLOSED_ANCHOR + FULLSTOP);
        }
    }

    /**
     * Add the statements for all measurement units to be inserted into the SPARQL endpoint.
     *
     * @param insertQueryBuilder INSERT DATA query generated using a String builder.
     */
    protected static void addUnitsInsertStatements(StringBuilder insertQueryBuilder) {
        insertQueryBuilder.append(OM_KWH + WHITESPACE + SKOS_NOTATION + WHITESPACE + KWH_LITERAL + FULLSTOP);
        insertQueryBuilder.append(OM_CBM + WHITESPACE + SKOS_NOTATION + WHITESPACE + CBM_LITERAL + FULLSTOP);
        insertQueryBuilder.append(OM_LITRE + WHITESPACE + SKOS_NOTATION + WHITESPACE + LITRE_LITERAL + FULLSTOP);
    }

    /**
     * Add the statements for electricity consumption to be inserted into the SPARQL endpoint.
     *
     * @param measureIRI         Electricity Consumption IRI generated that should be linked to OntoBIM instances.
     * @param insertQueryBuilder INSERT DATA query generated using a String builder.
     */
    protected static void addElectricityInsertStatements(String measureIRI, StringBuilder insertQueryBuilder) {
        String elecQuantityIRI = BASE_PREFIX + MONTHLY_ELECTRICITY_INST + UUID.randomUUID();
        insertQueryBuilder.append(elecQuantityIRI + WHITESPACE + RDFTYPE + WHITESPACE + UBEMMP_PREFIX + "MonthlyElectricityConsumption" + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASVALUE + WHITESPACE + OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + FULLSTOP);
        insertQueryBuilder.append(OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + WHITESPACE + RDFTYPE + WHITESPACE + OM_ENERGY + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASUNIT + WHITESPACE + OM_KWH + FULLSTOP);

        // Only include the following triple if they contain a certain floor name, and there are instances to retrieve
        if (measureIRI.contains("Attic") && !OntoBimAdapter.ATTIC_INST.isEmpty()) {
            insertQueryBuilder.append(OPEN_ANCHOR + OntoBimAdapter.ATTIC_INST + CLOSED_ANCHOR + WHITESPACE + UBEMMP_CONSUMES_UTILITIES + WHITESPACE + elecQuantityIRI + FULLSTOP);
        } else if (measureIRI.contains("FirstFloor") && !OntoBimAdapter.FIRST_FLOOR_INST.isEmpty()) {
            insertQueryBuilder.append(OPEN_ANCHOR + OntoBimAdapter.FIRST_FLOOR_INST + CLOSED_ANCHOR + WHITESPACE + UBEMMP_CONSUMES_UTILITIES + WHITESPACE + elecQuantityIRI + FULLSTOP);
        } else if (measureIRI.contains("GroundFloor") && !OntoBimAdapter.GROUND_FLOOR_INST.isEmpty()) {
            insertQueryBuilder.append(OPEN_ANCHOR + OntoBimAdapter.GROUND_FLOOR_INST + CLOSED_ANCHOR + WHITESPACE + UBEMMP_CONSUMES_UTILITIES + WHITESPACE + elecQuantityIRI + FULLSTOP);
        }
    }

    /**
     * Add the statements for water consumption to be inserted into the SPARQL endpoint.
     *
     * @param measureIRI         Water Consumption IRI generated that should be linked to OntoBIM instances.
     * @param insertQueryBuilder INSERT DATA query generated using a String builder.
     */
    protected static void addWaterInsertStatements(String measureIRI, StringBuilder insertQueryBuilder) {
        String waterQuantityIRI = BASE_PREFIX + MONTHLY_WATER_INST + UUID.randomUUID();
        insertQueryBuilder.append(waterQuantityIRI + WHITESPACE + RDFTYPE + WHITESPACE + UBEMMP_PREFIX + "MonthlyWaterConsumption" + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASVALUE + WHITESPACE + OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + FULLSTOP);
        insertQueryBuilder.append(OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + WHITESPACE + RDFTYPE + WHITESPACE + OM_VOLUME + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASUNIT + WHITESPACE + OM_CBM + FULLSTOP);
        if (!OntoBimAdapter.BUILDING_INST.isEmpty()) {
            insertQueryBuilder.append(OPEN_ANCHOR + OntoBimAdapter.BUILDING_INST + CLOSED_ANCHOR + WHITESPACE + UBEMMP_CONSUMES_UTILITIES + WHITESPACE + waterQuantityIRI + FULLSTOP);
        }
    }

    /**
     * Add the statements for oil consumption to be inserted into the SPARQL endpoint.
     *
     * @param measureIRI         Oil Consumption IRI generated that should be linked to OntoBIM instances.
     * @param insertQueryBuilder INSERT DATA query generated using a String builder.
     */
    protected static void addOilInsertStatements(String measureIRI, StringBuilder insertQueryBuilder) {
        String oilQuantityIRI = BASE_PREFIX + MONTHLY_OIL_INST + UUID.randomUUID();
        insertQueryBuilder.append(oilQuantityIRI + WHITESPACE + RDFTYPE + WHITESPACE + UBEMMP_PREFIX + "MonthlyOilConsumption" + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASVALUE + WHITESPACE + OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + FULLSTOP);
        insertQueryBuilder.append(OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + WHITESPACE + RDFTYPE + WHITESPACE + OM_VOLUME + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASUNIT + WHITESPACE + OM_LITRE + FULLSTOP);
        if (!OntoBimAdapter.BUILDING_INST.isEmpty()) {
            insertQueryBuilder.append(OPEN_ANCHOR + OntoBimAdapter.BUILDING_INST + CLOSED_ANCHOR + WHITESPACE + UBEMMP_CONSUMES_UTILITIES + WHITESPACE + oilQuantityIRI + FULLSTOP);
        }
    }
}
