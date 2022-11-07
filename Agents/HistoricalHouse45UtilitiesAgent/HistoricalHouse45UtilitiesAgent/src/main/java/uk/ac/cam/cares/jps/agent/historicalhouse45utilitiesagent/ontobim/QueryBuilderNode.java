package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

class QueryBuilderNode {
    // Prefixes
    protected static final String BASE_PREFIX = "twa:";
    protected static final String RDF_PREFIX = "rdf:";
    protected static final String RDFS_PREFIX = "rdfs:";
    protected static final String OM_PREFIX = "om:";
    protected static final String SKOS_PREFIX = "skos:";
    protected static final String QUDT_PREFIX = "qudt:";
    protected static final String BOT_PREFIX = "bot:";
    protected static final String UBEMMP_PREFIX = "ontoubemmp:";
    protected static final String ONTOBUILTENV_PREFIX = "builtenv:";
    protected static final String TIMESERIES_PREFIX = "ontotimeseries:";
    // Instances
    protected static final String MONTHLY_ELECTRICITY_INST = "MonthlyElectricityConsumption_Quantity_";
    protected static final String MONTHLY_WATER_INST = "MonthlyWaterConsumption_Quantity_";
    protected static final String MONTHLY_OIL_INST = "MonthlyOilConsumption_Quantity_";
    // Classes
    protected static final String BOT_BUILDING = BOT_PREFIX + "Building";
    protected static final String BOT_STOREY = BOT_PREFIX + "Storey";
    // Properties
    protected static final String RDFTYPE = RDF_PREFIX + "type";
    protected static final String RDFS_LABEL = RDFS_PREFIX + "label";
    protected static final String TIMESERIES_HAS_TS = TIMESERIES_PREFIX + "hasTimeSeries";
    protected static final String OM_HASVALUE = OM_PREFIX + "hasValue";
    protected static final String OM_HASUNIT = OM_PREFIX + "hasUnit";
    protected static final String OM_ENERGY = OM_PREFIX + "Energy";
    protected static final String OM_KWH = OM_PREFIX + "kilowattHour";
    protected static final String OM_VOLUME = OM_PREFIX + "Volume";
    protected static final String OM_CBM = OM_PREFIX + "cubicMetre";
    protected static final String OM_LITRE = OM_PREFIX + "litre";
    protected static final String SKOS_NOTATION = SKOS_PREFIX + "notation";
    protected static final String UBEMMP_CONSUMES_UTILITIES = UBEMMP_PREFIX + "consumesUtilities";
    protected static final String ONTOBUILTENV_HAS_CITYGML_REP = ONTOBUILTENV_PREFIX + "hasOntoCityGMLRepresentation";
    // Literal
    protected static final String KWH_LITERAL = "'kW.h'^^" + QUDT_PREFIX + "UCUMcs";
    protected static final String CBM_LITERAL = "'m3'^^" + QUDT_PREFIX + "UCUMcs";
    protected static final String LITRE_LITERAL = "'L'^^" + QUDT_PREFIX + "UCUMcs";
    // Characters
    protected static final String WHITESPACE = " ";
    protected static final String TAB = "\t";
    protected static final String SEMICOLON = ";\n";
    protected static final String FULLSTOP = ".\n";
    protected static final String OPEN_ANCHOR = "<";
    protected static final String CLOSED_ANCHOR = ">";
}
