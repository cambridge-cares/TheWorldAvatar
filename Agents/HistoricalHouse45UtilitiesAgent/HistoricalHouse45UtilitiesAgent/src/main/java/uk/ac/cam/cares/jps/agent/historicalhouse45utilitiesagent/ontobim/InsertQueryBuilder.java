package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

import java.util.UUID;

/**
 * Creates the required INSERT Query statements.
 *
 * @author qhouyee
 */
class InsertQueryBuilder {
    // Prefixes
    protected static final String BASE_PREFIX = "twa:";
    protected static final String RDF_PREFIX = "rdf:";
    protected static final String OM_PREFIX = "om:";
    protected static final String SKOS_PREFIX = "skos:";
    protected static final String QUDT_PREFIX = "qudt:";
    protected static final String UBEMMP_PREFIX = "ontoubemmp:";
    // Instances
    private static final String MONTHLY_ELECTRICITY_INST = "MonthlyElectricityConsumption_Quantity_";
    private static final String MONTHLY_WATER_INST = "MonthlyWaterConsumption_Quantity_";
    private static final String MONTHLY_OIL_INST = "MonthlyOilConsumption_Quantity_";
    // Properties
    private static final String RDFTYPE = RDF_PREFIX + "type";
    private static final String OM_HASVALUE = OM_PREFIX + "hasValue";
    private static final String OM_HASUNIT = OM_PREFIX + "hasUnit";
    private static final String OM_ENERGY = OM_PREFIX + "Energy";
    private static final String OM_KWH = OM_PREFIX + "kilowattHour";
    private static final String OM_VOLUME = OM_PREFIX + "Volume";
    private static final String OM_CBM = OM_PREFIX + "cubicMetre";
    private static final String OM_LITRE = OM_PREFIX + "litre";
    private static final String SKOS_NOTATION = SKOS_PREFIX + "notation";
    // Literal
    private static final String KWH_LITERAL = "'kW.h'^^" + QUDT_PREFIX + "UCUMcs";
    private static final String CBM_LITERAL = "'m3'^^" + QUDT_PREFIX + "UCUMcs";
    private static final String LITRE_LITERAL = "'L'^^" + QUDT_PREFIX + "UCUMcs";
    // Characters
    private static final String WHITESPACE = " ";
    private static final String TAB = "\t";
    private static final String SEMICOLON = ";\n";
    private static final String FULLSTOP = ".\n";
    protected static final String OPEN_ANCHOR = "<";
    protected static final String CLOSED_ANCHOR = ">";

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
        insertQueryBuilder.append(BASE_PREFIX + MONTHLY_ELECTRICITY_INST + UUID.randomUUID() + WHITESPACE + RDFTYPE
                + WHITESPACE + UBEMMP_PREFIX + "MonthlyElectricityConsumption" + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASVALUE + WHITESPACE + OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + FULLSTOP);
        insertQueryBuilder.append(OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + WHITESPACE + RDFTYPE + WHITESPACE + OM_ENERGY + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASUNIT + WHITESPACE + OM_KWH + FULLSTOP);
    }

    /**
     * Add the statements for water consumption to be inserted into the SPARQL endpoint.
     *
     * @param measureIRI         Water Consumption IRI generated that should be linked to OntoBIM instances.
     * @param insertQueryBuilder INSERT DATA query generated using a String builder.
     */
    protected static void addWaterInsertStatements(String measureIRI, StringBuilder insertQueryBuilder) {
        insertQueryBuilder.append(BASE_PREFIX + MONTHLY_WATER_INST + UUID.randomUUID() + WHITESPACE + RDFTYPE
                + WHITESPACE + UBEMMP_PREFIX + "MonthlyWaterConsumption" + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASVALUE + WHITESPACE + OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + FULLSTOP);
        insertQueryBuilder.append(OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + WHITESPACE + RDFTYPE + WHITESPACE + OM_VOLUME + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASUNIT + WHITESPACE + OM_CBM + FULLSTOP);
    }

    /**
     * Add the statements for oil consumption to be inserted into the SPARQL endpoint.
     *
     * @param measureIRI         Oil Consumption IRI generated that should be linked to OntoBIM instances.
     * @param insertQueryBuilder INSERT DATA query generated using a String builder.
     */
    protected static void addOilInsertStatements(String measureIRI, StringBuilder insertQueryBuilder) {
        insertQueryBuilder.append(BASE_PREFIX + MONTHLY_OIL_INST + UUID.randomUUID() + WHITESPACE + RDFTYPE
                + WHITESPACE + UBEMMP_PREFIX + "MonthlyOilConsumption" + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASVALUE + WHITESPACE + OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + FULLSTOP);
        insertQueryBuilder.append(OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + WHITESPACE + RDFTYPE + WHITESPACE + OM_VOLUME + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASUNIT + WHITESPACE + OM_LITRE + FULLSTOP);
    }
}
