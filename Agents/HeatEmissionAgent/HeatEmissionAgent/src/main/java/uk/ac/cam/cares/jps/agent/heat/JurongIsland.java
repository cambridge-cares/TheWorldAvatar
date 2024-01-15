package uk.ac.cam.cares.jps.agent.heat;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.jena.arq.querybuilder.AbstractQueryBuilder;
import org.apache.jena.arq.querybuilder.ExprFactory;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.sparql.expr.Expr;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;

import com.jayway.jsonpath.JsonPath;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.agent.heat.objects.Factory;
import uk.ac.cam.cares.jps.agent.heat.objects.FactoryType;
import uk.ac.cam.cares.jps.agent.heat.objects.HeatSource;
import uk.ac.cam.cares.jps.agent.heat.objects.HeatSourceType;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;

public class JurongIsland {

        // Store client to query SPARQL endpoint
        private String queryEndpoint = null;
        private String updateEndpoint = null;
        private RemoteStoreClient storeClient;

        // Array for storing queried data
        private List<HeatSource> heatSources = new ArrayList<>();

        // SPARQL Prefixes
        private static final String builtenv = "https://www.theworldavatar.com/kg/ontobuiltenv/";
        private static final String geosparql = "http://www.opengis.net/ont/geosparql#";
        private static final String ontoChemPlant = "http://www.theworldavatar.com/kg/ontochemplant/";
        private static final String ontoUnitsMeasure = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
        private static final String rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
        private static final String rdfs = "http://www.w3.org/2000/01/rdf-schema#";
        private static final String ontoCompanyPrefix = "http://www.theworldavatar.com/kg/ontocompany/";
        private static final String ontoCityGmlPrefix = "http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#";

        // SPARQL classes and predicates
        private static final String hasGeneratedHeat = ontoCompanyPrefix + "hasGeneratedHeat";
        private static final String generatedHeat = ontoCompanyPrefix + "GeneratedHeat";
        private static final String measure = ontoUnitsMeasure + "Measure";
        private static final String hasValue = ontoUnitsMeasure + "hasValue";
        private static final String hasNumericalValue = ontoUnitsMeasure + "hasNumericalValue";
        private static final String hasUnit = ontoUnitsMeasure + "hasUnit";
        private static final String megaWatt = ontoUnitsMeasure + "MegaWatt";
        private static final String rdfType = rdf + "type";
        private static final String plantItem = "http://www.theworldavatar.com/kg/ontocape/chemicalprocesssystem/cpsrealization/plant/plantitem";
        private static final String building = "http://www.purl.org/oema/infrastructure/Building";

        public JurongIsland(JSONObject request) {
                if (request.has("endpoint")) {
                        this.queryEndpoint = request.getString("endpoint");
                        this.updateEndpoint = queryEndpoint;
                } else {
                        String namespace = request.getString("namespace");
                        EndpointConfig endpointConfig = new EndpointConfig(namespace);
                        queryEndpoint = endpointConfig.getKgurl();
                        updateEndpoint = queryEndpoint;
                }
        }

        private void getHeatSourceProperties() {

                ExprFactory exprFactory = new ExprFactory();

                WhereBuilder wb2 = new WhereBuilder().addPrefix("ocp", ontoChemPlant).addPrefix("om", ontoUnitsMeasure)
                                .addWhere("?plant", "ocp:hasDesignCapacity", "?dc")
                                .addWhere("?dc", "om:hasUnit",
                                                "http://www.theworldavatar.com/kg/ontochemplant/MegaWatt");

                Expr expr = exprFactory.notexists(wb2);

                WhereBuilder wb = new WhereBuilder()
                                .addPrefix("obe", builtenv)
                                .addPrefix("ocp", ontoChemPlant)
                                .addPrefix("geo", geosparql)
                                .addPrefix("rdfs", rdfs)
                                .addPrefix("om", ontoUnitsMeasure)
                                .addPrefix("rdf", rdf);

                wb.addWhere("?chemical_plant", "ocp:hasFuelType", "?ft")
                                .addWhere("?chemical_plant", "geo:ehContains", "?heat_source")
                                .addWhere("?heat_source", "obe:hasOntoCityGMLRepresentation", "?ocgml_iri")
                                .addWhere("?heat_source", "rdf:type",
                                                "?source_type")
                                .addValueVar("?source_type",
                                                plantItem,
                                                building)
                                .addWhere("?heat_source", "ocp:hasIndividualCO2Emission", "?x")
                                .addWhere("?x", "om:hasNumericalValue", "?co2")
                                .addFilter(expr);
                SelectBuilder sb = new SelectBuilder()
                                .addVar("heat_source")
                                .addVar("co2")
                                .addVar("ocgml_iri")
                                .addVar("source_type")
                                .addWhere(wb);
                JSONArray queryResult = storeClient.executeQuery(sb.buildString());

                Map<String, HeatSourceType> classToType = new HashMap<>();
                classToType.put(plantItem, HeatSourceType.PlantItem);
                classToType.put(building, HeatSourceType.Building);

                for (int i = 0; i < queryResult.length(); i++) {
                        String heatSourceIri = queryResult.getJSONObject(i).getString("heat_source");
                        Double carbonEmissions = queryResult.getJSONObject(i).getDouble("co2");
                        String ontoCityGmlIri = queryResult.getJSONObject(i).getString("ocgml_iri");
                        String sourceType = queryResult.getJSONObject(i).getString("source_type");
                        HeatSource heatSource = new HeatSource(heatSourceIri, ontoCityGmlIri, carbonEmissions);
                        heatSource.sourceType = classToType.get(sourceType);
                        heatSource.calculateHeat();
                        heatSources.add(heatSource);
                }
        }

        private void deleteHeat() {

                UpdateBuilder db = new UpdateBuilder().addPrefix("rdf", rdf)
                                .addDelete("?measure", "?p", "?o")
                                .addWhere("?s", NodeFactory.createURI(
                                                "http://www.theworldavatar.com/kg/ontocompany/hasGeneratedHeat"),
                                                "?heat")
                                .addWhere("?heat", "rdf:type", NodeFactory.createURI(generatedHeat))
                                .addWhere("?heat", NodeFactory.createURI(hasValue), "?measure")
                                .addWhere("?measure", "?p", "?o");

                UpdateBuilder db2 = new UpdateBuilder().addDelete("?heat", "?p", "?o")
                                .addWhere("?s", NodeFactory.createURI(hasGeneratedHeat), "?heat")
                                .addWhere("?heat", "?p", "?o");
                UpdateBuilder db3 = new UpdateBuilder()
                                .addDelete("?s", NodeFactory.createURI(hasGeneratedHeat), "?heat")
                                .addWhere("?s", NodeFactory.createURI(hasGeneratedHeat),
                                                "?heat");

                storeClient.executeUpdate(db.buildRequest().toString());
                storeClient.executeUpdate(db2.buildRequest().toString());
                storeClient.executeUpdate(db3.buildRequest().toString());

        }

        private void updateHeat() {
                UpdateBuilder ub = new UpdateBuilder();

                heatSources.stream().forEach(hs -> {

                        String heatIri = generatedHeat + "_" + UUID.randomUUID().toString();
                        String measureIri = measure + "_" + UUID.randomUUID().toString();
                        ub.addInsert(NodeFactory.createURI(hs.iri), NodeFactory.createURI(hasGeneratedHeat),
                                        NodeFactory.createURI(heatIri));
                        ub.addInsert(NodeFactory.createURI(heatIri), NodeFactory.createURI(rdfType),
                                        NodeFactory.createURI(generatedHeat));
                        ub.addInsert(NodeFactory.createURI(heatIri), NodeFactory.createURI(hasValue),
                                        NodeFactory.createURI(measureIri))
                                        .addInsert(NodeFactory.createURI(measureIri), NodeFactory.createURI(rdfType),
                                                        NodeFactory.createURI(measure))
                                        .addInsert(NodeFactory.createURI(measureIri),
                                                        NodeFactory.createURI(hasNumericalValue),
                                                        NodeFactory.createLiteral(Double.toString(hs.heatEmission),
                                                                        XSDDatatype.XSDdouble));
                        ub.addInsert(NodeFactory.createURI(measureIri), NodeFactory.createURI(hasUnit),
                                        NodeFactory.createURI(megaWatt));

                });

                storeClient.executeUpdate(ub.buildRequest().toString());

        }

        private void deleteLocations() {

                WhereBuilder wb = new WhereBuilder().addValueVar("?source_type",
                                "http://www.theworldavatar.com/kg/ontocape/chemicalprocesssystem/cpsrealization/plant/plantitem",
                                "http://www.purl.org/oema/infrastructure/Building");
                UpdateBuilder db = new UpdateBuilder().addPrefix("rdf", rdf).addPrefix("ocy", ontoCompanyPrefix)
                                .addDelete("?s", "ocy:hasLongitudeEPSG24500", "?lon")
                                .addWhere("?s", "rdf:type", "?source_type")
                                .addWhere(wb);

                UpdateBuilder db2 = new UpdateBuilder().addPrefix("rdf", rdf).addPrefix("ocy", ontoCompanyPrefix)
                                .addDelete("?s", "ocy:hasLatitudeEPSG24500", "?lat")
                                .addWhere("?s", "rdf:type", "?source_type")
                                .addWhere(wb);

                storeClient.executeUpdate(db.buildRequest().toString());
                storeClient.executeUpdate(db2.buildRequest().toString());

        }

        private void setLocations() {
                String ontoCityGmlEndpoint = "http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql";
                RemoteStoreClient storeClient2 = new RemoteStoreClient(ontoCityGmlEndpoint);

                List<String> buildingIris = heatSources.stream().filter(hs -> hs.sourceType == HeatSourceType.Building)
                                .map(hs -> hs.iri).collect(Collectors.toList());

                WhereBuilder wb = new WhereBuilder().addPrefix("ocgml", ontoCityGmlPrefix)
                                .addWhere("?surface", "ocgml:buildingId", "?building_iri")
                                .addWhere("?surface", "ocgml:objectClassId", "?classId")
                                .addValueVar("building_iri", buildingIris)
                                .addValueVar("?classId", 35);

        }

        private void updateLocations() {

                UpdateBuilder ub = new UpdateBuilder();

                String hasLongitudeEPSG24500 = ontoCompanyPrefix + "hasLongitudeEPSG24500";
                String hasLatitudeEPSG24500 = ontoCompanyPrefix + "hasLatitudeEPSG24500";

                heatSources.stream().forEach(hs -> {
                        ub.addInsert(NodeFactory.createURI(hs.iri), NodeFactory.createURI(hasLongitudeEPSG24500),
                                        NodeFactory.createLiteral(Double.toString(hs.location.getX()),
                                                        XSDDatatype.XSDdouble));
                        ub.addInsert(NodeFactory.createURI(hs.iri), NodeFactory.createURI(hasLatitudeEPSG24500),
                                        NodeFactory.createLiteral(Double.toString(hs.location.getY()),
                                                        XSDDatatype.XSDdouble));

                });

                storeClient.executeUpdate(ub.buildRequest().toString());

        }

        public JSONObject calculateHeat() {

                storeClient = new RemoteStoreClient(queryEndpoint, updateEndpoint);
                heatSources.clear();
                getHeatSourceProperties();
                deleteHeat();
                updateHeat();
                deleteLocations();
                setLocations();
                updateLocations();

                JSONArray heatresult = new JSONArray();
                JSONObject heatresult1 = new JSONObject();
                JSONArray IRIandCO2QueryResult = IRIandCO2Query();
                StringBuffer Combined = new StringBuffer("{\r\n" +
                                "\"type\": \"FeatureCollection\",\r\n" +
                                "\"features\": [\n");

                double heat_data = 0;

                for (int i = 0; i < IRIandCO2QueryResult.length(); i++) {
                        String IRI = IRIandCO2QueryResult.getJSONObject(i).getString("IRI");
                        String CO2 = IRIandCO2QueryResult.getJSONObject(i).getString("CO2");
                        String Plant_item = IRIandCO2QueryResult.getJSONObject(i).getString("plant_item");
                        String ChemPlant = IRIandCO2QueryResult.getJSONObject(i).getString("chemical_plant");
                        String ChemPlantName = "<" + ChemPlant + ">";

                        JSONArray plantInfoQueryResult = FuelCEIEfficiency(ChemPlantName);
                        String CEI = plantInfoQueryResult.getJSONObject(0).getString("CEI");
                        String Efficiency = plantInfoQueryResult.getJSONObject(0).getString("efficiency");

                        JSONArray coordiSpatialQueryResult = CoordinateQuery(IRI);
                        String heatcoordi = HeatEmissionCoordinate(coordiSpatialQueryResult);
                        String[] heatcoordi_split = heatcoordi.split("#");
                        Double x_coordinate = Double.parseDouble(heatcoordi_split[0]);
                        Double y_coordinate = Double.parseDouble(heatcoordi_split[1]);

                        // Calculate the heat emission amount in the unit of MW within the indicated
                        // region boundary
                        double[] region_boundary = Boundary(requestParams);
                        if (region_boundary[0] < x_coordinate && region_boundary[2] > x_coordinate
                                        && region_boundary[1] < y_coordinate && region_boundary[3] > y_coordinate) {
                                double heatamount = Double.parseDouble(CO2) / Double.parseDouble(CEI) * 1e12 / 365 / 24
                                                / 3600 / 1e6
                                                * Double.parseDouble(Efficiency);
                                JSONObject row = new JSONObject();
                                row.put("Coordinate", heatcoordi);
                                row.put("Heat Emission", heatamount);
                                heatresult.put(row);
                                // sparqlUpdate(Plant_item, Double.toString(heatamount));
                                heat_data = heatamount;
                        }

                        String inputCRS = "EPSG:24500";
                        String outputCRS = "EPSG:4326";
                        double[] xyOriginal = { x_coordinate, y_coordinate };
                        double[] xyTransformed = CRSTransformer.transform(inputCRS, outputCRS, xyOriginal);

                        String z_coordi = heatcoordi_split[2];
                        String x_coordi = Double.toString(xyTransformed[0]);
                        String y_coordi = Double.toString(xyTransformed[1]);

                        StringBuffer heatemission = new StringBuffer("{\n\"type\": \"Feature\",\n");
                        heatemission.append("\"properties\": {\n\"height:m\":").append(z_coordi + "," + "\n");
                        heatemission.append("\"AH_type\":").append("\"SH\",\n");
                        heatemission.append("\"AH_0:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_1:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_2:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_3:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_4:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_5:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_6:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_7:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_8:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_9:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_10:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_11:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_12:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_13:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_14:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_15:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_16:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_17:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_18:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_19:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_20:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_21:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_22:MW\":").append(heat_data).append(",\n");
                        heatemission.append("\"AH_23:MW\":").append(heat_data).append("\n").append("},\n");
                        heatemission.append("\"geometry\": {\n" + "\"type\": \"Point\",\n" + "\"coordinates\": [\n")
                                        .append(x_coordi + ",\n").append(y_coordi + "\n]\n}\n},\n");
                        Combined.append(heatemission);

                }

                // Output heat emission data in GeoJSON format for DUCT

                String outputFP = Paths.get(System.getProperty("user.dir"), "output", "base.geojson").toString();
                String output = Combined.substring(0, Combined.length() - 2) + "\n]\n}";
                File file1 = new File(outputFP);
                FileWriter fw;
                try {
                        fw = new FileWriter(file1);
                        PrintWriter pw = new PrintWriter(fw);
                        pw.println(output);
                        pw.close();
                } catch (IOException e) {
                        e.printStackTrace();
                }

                heatresult1.put("result", heatresult);
                return heatresult1;
        }

        // Set up the region boundary
        public static double[] Boundary(JSONObject inputBounds) {
                String upper_limits = JsonPath.read(inputBounds.toString(), "$.upper_bounds");
                String lower_limits = JsonPath.read(inputBounds.toString(), "$.lower_bounds");
                String[] upper_split = upper_limits.split("#");
                String[] lower_split = lower_limits.split("#");
                double[] result = new double[4];
                result[0] = Double.parseDouble(lower_split[0]);
                result[1] = Double.parseDouble(lower_split[1]);
                result[2] = Double.parseDouble(upper_split[0]);
                result[3] = Double.parseDouble(upper_split[1]);
                return result;
        }

        // Query all the chemical plants, plant items and respective IRI as well as CO2
        // emissions
        public static JSONArray IRIandCO2Query() {
                StringBuffer IRIandCO2Query = new StringBuffer(
                                "PREFIX ns2: <https://www.theworldavatar.com/kg/ontobuiltenv/>\n");
                IRIandCO2Query.append("PREFIX geo: <http://www.opengis.net/ont/geosparql#>\n");
                IRIandCO2Query.append("PREFIX ocp: <http://www.theworldavatar.com/kg/ontochemplant/>\n");
                IRIandCO2Query.append("PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>\n");
                IRIandCO2Query.append("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>");
                IRIandCO2Query.append("PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n");
                IRIandCO2Query.append("PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> \n");
                IRIandCO2Query.append("SELECT ?chemical_plant ?plant_item ?IRI ?CO2 ?unit WHERE { \n");
                IRIandCO2Query.append("?chemical_plant ocp:hasFuelType ?ft. \n");
                IRIandCO2Query.append("?chemical_plant rdfs:label ?plant_name. \n");
                IRIandCO2Query.append("?ft rdfs:label ?ftl. \n");
                IRIandCO2Query.append("?chemical_plant geo:ehContains ?plant_item . \n");
                IRIandCO2Query.append("?plant_item ns2:hasOntoCityGMLRepresentation ?IRI . \n");
                IRIandCO2Query.append(
                                "?plant_item rdf:type <http://www.theworldavatar.com/kg/ontocape/chemicalprocesssystem/cpsrealization/plant/plantitem>. \n");
                IRIandCO2Query.append("?plant_item ocp:hasIndividualCO2Emission ?x .  \n");
                IRIandCO2Query.append("?x om:hasNumericalValue ?CO2 . \n");
                IRIandCO2Query.append("?x om:hasUnit ?a . \n");
                IRIandCO2Query.append("?a om:symbol ?unit . \n");
                IRIandCO2Query.append(
                                "FILTER (str(?plant_name) not in (\"Chemical_plant_of_Sembcorp Integrated Wastewater Treatment Plant\","
                                                +
                                                "\"Chemical_plant_of_SembCorp EFW (Energy from Waste)\"," +
                                                "\"Chemical_plant_of_SembCorp Industries Ltd\"," +
                                                "\"Chemical_plant_of_SembCorp Utilities\"," +
                                                "\"Chemical_plant_of_YTL PowerSeraya Pte. Limited\", " +
                                                "\"Chemical_plant_of_Tuas Power Utilities\", " +
                                                "\"Chemical_plant_of_Sembcorp Cogen Pte Ltd@Sakra\")) \n");
                IRIandCO2Query.append("}");
                JSONArray IRIandCO2QueryResult = AccessAgentCaller.queryStore("jibusinessunits",
                                IRIandCO2Query.toString());
                return IRIandCO2QueryResult;
        }

        // Chemical plant fuel, CEI and thermal efficiency query
        public static JSONArray FuelCEIEfficiency(String ChemialPlant) {
                StringBuffer FuelCEIEffiQuery = new StringBuffer(
                                "PREFIX kb: <http://www.theworldavatar.com/kb/ontochemplant/>\n");
                FuelCEIEffiQuery.append("PREFIX ocp: <http://www.theworldavatar.com/kg/ontochemplant/>\n");
                FuelCEIEffiQuery.append("PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>\n");
                FuelCEIEffiQuery.append("SELECT ?fuel ?CEI ?unit ?efficiency WHERE {");
                FuelCEIEffiQuery.append(ChemialPlant).append(" ocp:hasThermalEfficiency ?efficiency .");
                FuelCEIEffiQuery.append(ChemialPlant).append(" ocp:hasFuelType ?fuel .");
                FuelCEIEffiQuery.append("?fuel ocp:hasCarbonEmissionIndex ?cei .");
                FuelCEIEffiQuery.append("?cei om:hasNumericalValue ?CEI .");
                FuelCEIEffiQuery.append("?cei om:hasUnit ?a .");
                FuelCEIEffiQuery.append("?a om:symbol ?unit .}");
                JSONArray plantInfoQueryResult = AccessAgentCaller.queryStore("jibusinessunits",
                                FuelCEIEffiQuery.toString());
                return plantInfoQueryResult;
        }

        // Geometric coordination query
        public static JSONArray CoordinateQuery(String CityFurnitureIRI) {
                StringBuffer coordinateQuery = new StringBuffer(
                                "PREFIX ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>\n");
                coordinateQuery.append("SELECT ?geometricIRI ?polygonData WHERE {\n");
                coordinateQuery.append(
                                "GRAPH <http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql/surfacegeometry/> {?geometricIRI ocgml:GeometryType ?polygonData.\n");
                coordinateQuery.append("?geometricIRI ocgml:cityObjectId <").append(CityFurnitureIRI).append(">.}}");
                JSONArray coordiSpatialQueryResult = AccessAgentCaller.queryStore("jriEPSG24500",
                                coordinateQuery.toString());
                return coordiSpatialQueryResult;
        }

        // Calculate heat emission in xyz coordinate
        public static String HeatEmissionCoordinate(JSONArray coordiSpatialQueryResult) {
                String buildingX = "0";
                String buildingY = "0";
                String buildingZ = "0";

                for (int i = 0; i < coordiSpatialQueryResult.length(); i++) {
                        JSONObject coordiS = coordiSpatialQueryResult.getJSONObject(i);
                        String coordiData = coordiS.getString("polygonData");
                        ArrayList<String> z_values = new ArrayList<>();
                        String[] split = coordiData.split("#");
                        double sum_x = 0;
                        double sum_y = 0;
                        double sum_z = 0;
                        double min_z = 0;

                        for (Integer j = 1; j <= split.length; j++) {
                                if (j % 3 == 0) {
                                        z_values.add(split[j - 1]);
                                        sum_x = sum_x + Double.parseDouble(split[j - 3]);
                                        sum_y = sum_y + Double.parseDouble(split[j - 2]);
                                        sum_z = sum_z + Double.parseDouble(split[j - 1]);
                                        min_z = Math.min(min_z, Double.parseDouble(split[j - 1]));
                                }
                        }
                        if (min_z == sum_z / (split.length / 3) && !z_values.isEmpty()) {
                                buildingX = String.valueOf(sum_x / (split.length / 3));
                                buildingY = String.valueOf(sum_y / (split.length / 3));
                        }
                        if (!z_values.isEmpty() && Double.parseDouble(buildingZ) < Double
                                        .parseDouble(Collections.max(z_values))) {
                                buildingZ = Collections.max(z_values);
                        }
                }
                StringBuffer coordinate = new StringBuffer();
                coordinate.append(buildingX).append("#").append(buildingY).append("#").append(buildingZ);
                return coordinate.toString();
        }

        // Put the heat emssion data into the blazegraph via SPARQL update
        public void sparqlUpdate(String Plant_item, String heatValue) {
                String Heat_of_plantitem = Plant_item + "_Heat";
                String heat_label = Heat_of_plantitem.substring(47);
                String hasGeneratedHeat = "http://www.theworldavatar.com/kg/ontochemplant/hasGeneratedHeat";
                String genHeat = "http://www.theworldavatar.com/kg/ontochemplant/GeneratedHeat";
                String heatIri = genHeat + "_" + UUID.randomUUID();
                String measure = "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure";
                String measureIri = measure + "_" + UUID.randomUUID();
                String measure_label = "Measure_" + heat_label;

                UpdateBuilder ub = new UpdateBuilder()
                                .addPrefix("rdf", "https://www.w3.org/1999/02/22-rdf-syntax-ns")
                                .addPrefix("rdfs", "https://www.w3.org/2000/01/rdf-schema#")
                                .addInsert(NodeFactory.createURI(Plant_item),
                                                NodeFactory.createURI(
                                                                "http://theworldavatar.com/kg/ontochemplant/hasGeneratedHeat"),
                                                NodeFactory.createURI(heatIri))
                                .addInsert(NodeFactory.createURI(heatIri), "rdf:type",
                                                NodeFactory.createURI(
                                                                genHeat))
                                .addInsert(NodeFactory.createURI(heatIri), "rdfs:label", heat_label)
                                .addInsert(NodeFactory.createURI(heatIri),
                                                NodeFactory.createURI(
                                                                "http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue"),
                                                NodeFactory.createURI(measureIri))
                                .addInsert(NodeFactory.createURI(measureIri), "rdf:type",
                                                NodeFactory.createURI(measure))
                                .addInsert(NodeFactory.createURI(measureIri), "rdfs:label", measure_label)
                                .addInsert(NodeFactory.createURI(measureIri), NodeFactory.createURI(
                                                "http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue"),
                                                heatValue)
                                .addInsert(NodeFactory.createURI(measureIri),
                                                NodeFactory.createURI(
                                                                "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit"),
                                                NodeFactory.createURI(
                                                                "http://www.theworldavatar.com/kg/ontochemplant/MegaWatt"));

                UpdateRequest ur = ub.buildRequest();
                System.out.println(ur);
                AccessAgentCaller.updateStore("http://localhost:48888/ontochemplant", ur.toString());
                // This line below is for update the data in Claudius
                // AccessAgentCaller.updateStore("jibusinessunits", ur.toString());
        }
}
