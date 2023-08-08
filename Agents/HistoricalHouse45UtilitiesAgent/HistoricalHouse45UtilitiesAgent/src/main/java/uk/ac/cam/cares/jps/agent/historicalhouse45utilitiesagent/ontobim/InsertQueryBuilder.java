package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

import uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.BuildingIRISingleton;

import java.util.UUID;

/**
 * Creates the required INSERT Query statements.
 *
 * @author qhouyee
 */
class InsertQueryBuilder extends QueryBuilderNode {
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
     * Add the statements to link the building instance to their CityGML IRI counterpart.
     *
     * @param insertQueryBuilder INSERT DATA query generated using a String builder.
     * @param singleton          A singleton instance containing the required instances.
     */
    protected static void addOntoBuiltEnvInsertStatements(StringBuilder insertQueryBuilder, BuildingIRISingleton singleton) {
        if (!singleton.getOntoCityGmlBuildingIri().isEmpty() && !singleton.getBuildingIri().isEmpty()) {
            insertQueryBuilder.append(OPEN_ANCHOR + singleton.getBuildingIri() + CLOSED_ANCHOR + WHITESPACE
                    + ONTOBUILTENV_HAS_CITYGML_REP + WHITESPACE
                    + OPEN_ANCHOR + singleton.getOntoCityGmlBuildingIri() + CLOSED_ANCHOR + FULLSTOP);
        }
    }

    /**
     * Add the statements for electricity consumption to be inserted into the SPARQL endpoint.
     *
     * @param measureIRI         Electricity Consumption IRI generated that should be linked to OntoBIM instances.
     * @param insertQueryBuilder INSERT DATA query generated using a String builder.
     * @param singleton          A singleton instance containing the required instances.
     */
    protected static void addElectricityInsertStatements(String measureIRI, StringBuilder insertQueryBuilder, BuildingIRISingleton singleton) {
        String elecQuantityIRI = BASE_PREFIX + MONTHLY_ELECTRICITY_INST + UUID.randomUUID();
        insertQueryBuilder.append(elecQuantityIRI + WHITESPACE + RDFTYPE + WHITESPACE + UBEMMP_PREFIX + "MonthlyElectricityConsumption" + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASVALUE + WHITESPACE + OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + FULLSTOP);
        insertQueryBuilder.append(OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + WHITESPACE + RDFTYPE + WHITESPACE + OM_ENERGY + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASUNIT + WHITESPACE + OM_KWH + FULLSTOP);

        // Only include the following triple if they contain a certain floor name, and there are instances to retrieve
        if (measureIRI.contains("Attic") && !singleton.getAtticIri().isEmpty()) {
            insertQueryBuilder.append(OPEN_ANCHOR + singleton.getAtticIri() + CLOSED_ANCHOR + WHITESPACE + UBEMMP_CONSUMES_UTILITIES + WHITESPACE + elecQuantityIRI + FULLSTOP);
        } else if (measureIRI.contains("FirstFloor") && !singleton.getFirstFloorIri().isEmpty()) {
            insertQueryBuilder.append(OPEN_ANCHOR + singleton.getFirstFloorIri() + CLOSED_ANCHOR + WHITESPACE + UBEMMP_CONSUMES_UTILITIES + WHITESPACE + elecQuantityIRI + FULLSTOP);
        } else if (measureIRI.contains("GroundFloor") && !singleton.getGroundFloorIri().isEmpty()) {
            insertQueryBuilder.append(OPEN_ANCHOR + singleton.getGroundFloorIri() + CLOSED_ANCHOR + WHITESPACE + UBEMMP_CONSUMES_UTILITIES + WHITESPACE + elecQuantityIRI + FULLSTOP);
        }
    }

    /**
     * Add the statements for water consumption to be inserted into the SPARQL endpoint.
     *
     * @param measureIRI         Water Consumption IRI generated that should be linked to OntoBIM instances.
     * @param insertQueryBuilder INSERT DATA query generated using a String builder.
     * @param singleton          A singleton instance containing the required instances.
     */
    protected static void addWaterInsertStatements(String measureIRI, StringBuilder insertQueryBuilder, BuildingIRISingleton singleton) {
        String waterQuantityIRI = BASE_PREFIX + MONTHLY_WATER_INST + UUID.randomUUID();
        insertQueryBuilder.append(waterQuantityIRI + WHITESPACE + RDFTYPE + WHITESPACE + UBEMMP_PREFIX + "MonthlyWaterConsumption" + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASVALUE + WHITESPACE + OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + FULLSTOP);
        insertQueryBuilder.append(OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + WHITESPACE + RDFTYPE + WHITESPACE + OM_VOLUME + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASUNIT + WHITESPACE + OM_CBM + FULLSTOP);
        if (!singleton.getBuildingIri().isEmpty()) {
            insertQueryBuilder.append(OPEN_ANCHOR + singleton.getBuildingIri() + CLOSED_ANCHOR + WHITESPACE + UBEMMP_CONSUMES_UTILITIES + WHITESPACE + waterQuantityIRI + FULLSTOP);
        }
    }

    /**
     * Add the statements for oil consumption to be inserted into the SPARQL endpoint.
     *
     * @param measureIRI         Oil Consumption IRI generated that should be linked to OntoBIM instances.
     * @param insertQueryBuilder INSERT DATA query generated using a String builder.
     * @param singleton          A singleton instance containing the required instances.
     */
    protected static void addOilInsertStatements(String measureIRI, StringBuilder insertQueryBuilder, BuildingIRISingleton singleton) {
        String oilQuantityIRI = BASE_PREFIX + MONTHLY_OIL_INST + UUID.randomUUID();
        insertQueryBuilder.append(oilQuantityIRI + WHITESPACE + RDFTYPE + WHITESPACE + UBEMMP_PREFIX + "MonthlyOilConsumption" + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASVALUE + WHITESPACE + OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + FULLSTOP);
        insertQueryBuilder.append(OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + WHITESPACE + RDFTYPE + WHITESPACE + OM_VOLUME + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASUNIT + WHITESPACE + OM_LITRE + FULLSTOP);
        if (!singleton.getBuildingIri().isEmpty()) {
            insertQueryBuilder.append(OPEN_ANCHOR + singleton.getBuildingIri() + CLOSED_ANCHOR + WHITESPACE + UBEMMP_CONSUMES_UTILITIES + WHITESPACE + oilQuantityIRI + FULLSTOP);
        }
    }

    /**
     * Add the statements for different utility meters to be inserted into the SPARQL endpoint.
     *
     * @param measureIRI         Utility Consumption with Sensor Display IRI generated that should be linked to OntoBIM instances.
     * @param insertQueryBuilder INSERT DATA query generated using a String builder.
     * @param singleton          A singleton instance containing the required instances.
     */
    protected static void addMeterInsertStatements(String measureIRI, StringBuilder insertQueryBuilder, BuildingIRISingleton singleton) {
        String meteringFunctionIri = SAREF_PREFIX + METERING_FUNCTION_INST + UUID.randomUUID();
        String measurementIri = SAREF_PREFIX + MEASUREMENT_INST + UUID.randomUUID();
        if (!singleton.getElecMeterIri().isEmpty() && measureIRI.contains("Electricity")) {
            insertQueryBuilder.append(OPEN_ANCHOR + singleton.getElecMeterIri() + CLOSED_ANCHOR + WHITESPACE + SAREF_HAS_FUNCTION + WHITESPACE + meteringFunctionIri + FULLSTOP);
            addCommonMeterStatements(measureIRI, meteringFunctionIri, measurementIri, insertQueryBuilder);
        } else if (!singleton.getWaterMeterIri().isEmpty() && measureIRI.contains("Water")) {
            insertQueryBuilder.append(OPEN_ANCHOR + singleton.getWaterMeterIri() + CLOSED_ANCHOR + WHITESPACE + SAREF_HAS_FUNCTION + WHITESPACE + meteringFunctionIri + FULLSTOP);
            addCommonMeterStatements(measureIRI, meteringFunctionIri, measurementIri, insertQueryBuilder);
        } else if (!singleton.getOilMeterIri().isEmpty() && measureIRI.contains("Oil")) {
            insertQueryBuilder.append(OPEN_ANCHOR + singleton.getOilMeterIri() + CLOSED_ANCHOR + WHITESPACE + SAREF_HAS_FUNCTION + WHITESPACE + meteringFunctionIri + FULLSTOP);
            addCommonMeterStatements(measureIRI, meteringFunctionIri, measurementIri, insertQueryBuilder);
        }
    }

    /**
     * Add the common statements for different utility meters.
     *
     * @param measureIRI          Utility Consumption with Sensor Display IRI generated that should be linked to OntoBIM instances.
     * @param insertQueryBuilder  INSERT DATA query generated using a String builder.
     * @param meteringFunctionIri Metering function IRI generated.
     * @param measurementIri      SAREF Measurement IRI generated.
     */
    private static void addCommonMeterStatements(String measureIRI, String meteringFunctionIri, String measurementIri, StringBuilder insertQueryBuilder) {
        insertQueryBuilder.append(meteringFunctionIri + WHITESPACE + RDFTYPE + WHITESPACE + SAREF_METERING_FUNCTION + SEMICOLON);
        insertQueryBuilder.append(TAB + SAREF_HAS_METER_READING + WHITESPACE + measurementIri + FULLSTOP);
        insertQueryBuilder.append(measurementIri + WHITESPACE + RDFTYPE + WHITESPACE + SAREF_MEASUREMENT + SEMICOLON);
        insertQueryBuilder.append(TAB + OM_HASVALUE + WHITESPACE + OPEN_ANCHOR + measureIRI + CLOSED_ANCHOR + FULLSTOP);
    }
}
