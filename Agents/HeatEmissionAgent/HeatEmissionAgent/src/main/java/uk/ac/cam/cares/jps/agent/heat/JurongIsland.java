package uk.ac.cam.cares.jps.agent.heat;

import java.beans.Expression;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
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
import org.apache.jena.arq.querybuilder.clauses.ValuesClause;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.sparql.expr.Expr;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;

import com.jayway.jsonpath.JsonPath;

import org.json.JSONObject;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.geom.impl.CoordinateArraySequence;

import uk.ac.cam.cares.jps.agent.heat.objects.Factory;
import uk.ac.cam.cares.jps.agent.heat.objects.FactoryType;
import uk.ac.cam.cares.jps.agent.heat.objects.HeatSource;
import uk.ac.cam.cares.jps.agent.heat.objects.HeatSourceType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class JurongIsland {

        // Logger
        private static final Logger LOGGER = LogManager.getLogger(JurongIsland.class);

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

        // Store Client to query OntoCityGML endpoint
        String ontoCityGmlEndpoint = "http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql";
        RemoteStoreClient storeClientOcgml = new RemoteStoreClient(ontoCityGmlEndpoint);

        private int numberBuildings = 0, numberPlantItems = 0;

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

        Geometry convertStringToGeometry(String geomString) {
                String[] geomSplit = geomString.split("#");
                int nc = geomSplit.length / 3;
                Coordinate[] coordinates = new Coordinate[nc];

                for (int i = 0; i < geomSplit.length; i += 3) {
                        Double xc = Double.parseDouble(geomSplit[i]);
                        Double yc = Double.parseDouble(geomSplit[i + 1]);
                        Double zc = Double.parseDouble(geomSplit[i + 1]);
                        int index = i / 3;
                        coordinates[index] = new Coordinate(xc, yc, zc);
                }

                Geometry buildingFootprint = new GeometryFactory().createPolygon(coordinates);
                buildingFootprint.setSRID(24500);

                return buildingFootprint;
        }

        /**
         * Quries ontoCityGmlEndpoint to obtain building footprints
         */

        private void setBuildingFootprints() {

                ExprFactory exprFactory = new ExprFactory();
                Expr expr = exprFactory.not(exprFactory.isBlank("polygon"));

                Map<String, HeatSource> iriToBuilding = new HashMap<>();

                heatSources.stream().filter(hs -> hs.sourceType == HeatSourceType.Building)
                                .forEach(hs -> iriToBuilding.put(hs.iri, hs));

                List<String> buildingIris = new ArrayList<>(iriToBuilding.keySet());
                numberBuildings = buildingIris.size();

                WhereBuilder wb = new WhereBuilder().addPrefix("ocgml", ontoCityGmlPrefix)
                                .addWhere("?surface", "ocgml:GeometryType", "?polygon")
                                .addWhere("?geom", "ocgml:lod2MultiSurfaceId", "?surface")
                                .addWhere("?geom", "ocgml:objectClassId",
                                                NodeFactory.createLiteral(String.valueOf(35), XSDDatatype.XSDint))
                                .addWhere("?surface", "ocgml:buildingId", "?building_iri")
                                .addFilter(expr);
                buildingIris.stream().forEach(bi -> wb.addValueVar("building_iri", bi));

                SelectBuilder sb = new SelectBuilder().addVar("building_iri").addVar("polygon").addWhere(wb);

                JSONArray queryResult = storeClientOcgml.executeQuery(sb.buildString());

                for (int i = 0; i < queryResult.length(); i++) {
                        String footPrintString = queryResult.getJSONObject(i).getString("polygon");
                        String iri = queryResult.getJSONObject(i).getString("building_iri");
                        Geometry buildingFootPrint = convertStringToGeometry(footPrintString);
                        iriToBuilding.get(iri).footPrint = buildingFootPrint;
                }

        }

        private Map<String, List<Geometry>> cityFurnitureQuery(Map<String, HeatSource> iriToCityFurniture) {

                ExprFactory exprFactory = new ExprFactory();
                Expr expr = exprFactory.not(exprFactory.isBlank("polygon"));

                List<String> cityFurnitureIris = new ArrayList<>(iriToCityFurniture.keySet());

                WhereBuilder wb = new WhereBuilder().addPrefix("ocgml", ontoCityGmlPrefix)
                                .addWhere("?geom", "ocgml:GeometryType", "?polygon")
                                .addWhere("?geom", "ocgml:cityObjectId", "?city_furniture_iri")
                                .addFilter(expr);
                cityFurnitureIris.stream().forEach(cfi -> wb.addValueVar("city_furniture_iri", cfi));

                SelectBuilder sb = new SelectBuilder().addVar("city_furniture_iri").addVar("polygon").addWhere(wb);

                JSONArray queryResult = storeClientOcgml.executeQuery(sb.buildString());

                Map<String, List<Geometry>> iriToPolygonMap = new HashMap<>();

                for (int i = 0; i < queryResult.length(); i++) {
                        String cityFurnitureIri = queryResult.getJSONObject(i)
                                        .getString("city_furniture_iri");
                        String polygonString = queryResult.getJSONObject(i)
                                        .getString("polygon");
                        if (iriToPolygonMap.containsKey(cityFurnitureIri)) {
                                List<Geometry> polyList = iriToPolygonMap.get(cityFurnitureIri);
                                polyList.add(convertStringToGeometry(polygonString));
                                iriToPolygonMap.put(cityFurnitureIri, polyList);
                        } else {
                                List<Geometry> polyList = new ArrayList<>();
                                polyList.add(convertStringToGeometry(polygonString));
                                iriToPolygonMap.put(cityFurnitureIri, polyList);
                        }
                }

                return iriToPolygonMap;

        }

        private void setCityFurnitureFootprints() {

                Map<String, HeatSource> iriToCityFurniture = new HashMap<>();

                heatSources.stream().filter(hs -> hs.sourceType == HeatSourceType.PlantItem)
                                .forEach(hs -> iriToCityFurniture.put(hs.iri, hs));

                Map<String, List<Geometry>> iriToPolygonMap = cityFurnitureQuery(iriToCityFurniture);
                List<String> cityFurnitureIriList = new ArrayList<>(iriToPolygonMap.keySet());

                numberPlantItems = cityFurnitureIriList.size();

                for (String ocgmlIRI : cityFurnitureIriList) {
                        List<Geometry> polygonData = iriToPolygonMap.get(ocgmlIRI);
                        // Go through the set of polygons to retrieve the following:
                        // 1. maximum and minimum z-coordinates of all vertices across all polygons
                        // (minZ & maxZ).
                        // 2. base polygon

                        Geometry basePolygon = null;
                        final double minZ = polygonData.stream()
                                        .flatMap(polygon -> Arrays.stream(polygon.getCoordinates()))
                                        .mapToDouble(Coordinate::getZ).min().orElse(0.0);
                        final double maxZ = polygonData.stream()
                                        .flatMap(polygon -> Arrays.stream(polygon.getCoordinates()))
                                        .mapToDouble(Coordinate::getZ).max().orElse(0.0);

                        List<Geometry> basePolygons = polygonData.stream().filter(polygon -> {
                                double polyMinZ = Arrays.stream(polygon.getCoordinates()).mapToDouble(Coordinate::getZ)
                                                .min()
                                                .orElse(minZ);
                                double polyMaxZ = Arrays.stream(polygon.getCoordinates()).mapToDouble(Coordinate::getZ)
                                                .max()
                                                .orElse(maxZ);
                                return (polyMinZ == polyMaxZ) && (minZ == polyMinZ);
                        }).collect(Collectors.toList());

                        if (basePolygons.size() == 1)
                                basePolygon = basePolygons.get(0);
                        else {
                                LOGGER.error("Could not identify base polygon for city furniture object with OCGML IRI {}",
                                                ocgmlIRI);
                                throw new JPSRuntimeException("Error in BuildingsData class.");
                        }

                        iriToCityFurniture.get(ocgmlIRI).footPrint = basePolygon;

                }

        }

        private void updateLocations() {

                UpdateBuilder ub = new UpdateBuilder();

                String hasLongitudeEPSG24500 = ontoCompanyPrefix + "hasLongitudeEPSG24500";
                String hasLatitudeEPSG24500 = ontoCompanyPrefix + "hasLatitudeEPSG24500";

                String hasLongitudeEPSG4326 = ontoCompanyPrefix + "hasLongitudeEPSG4326";
                String hasLatitudeEPSG4326 = ontoCompanyPrefix + "hasLatitudeEPSG4326";

                heatSources.stream().forEach(hs -> {
                        ub.addInsert(NodeFactory.createURI(hs.iri), NodeFactory.createURI(hasLongitudeEPSG24500),
                                        NodeFactory.createLiteral(Double.toString(hs.location.getX()),
                                                        XSDDatatype.XSDdouble));
                        ub.addInsert(NodeFactory.createURI(hs.iri), NodeFactory.createURI(hasLatitudeEPSG24500),
                                        NodeFactory.createLiteral(Double.toString(hs.location.getY()),
                                                        XSDDatatype.XSDdouble));
                        double[] xyOriginal = { hs.location.getX(), hs.location.getY() };
                        double[] xyTransformed = CRSTransformer.transform("EPSG:24500", "EPSG:4326", xyOriginal);
                        ub.addInsert(NodeFactory.createURI(hs.iri), NodeFactory.createURI(hasLongitudeEPSG4326),
                                        NodeFactory.createLiteral(Double.toString(xyTransformed[0]),
                                                        XSDDatatype.XSDdouble));
                        ub.addInsert(NodeFactory.createURI(hs.iri), NodeFactory.createURI(hasLatitudeEPSG4326),
                                        NodeFactory.createLiteral(Double.toString(xyTransformed[1]),
                                                        XSDDatatype.XSDdouble));

                });

                storeClient.executeUpdate(ub.buildRequest().toString());

        }

        private void setHeatSourceLocations() {
                heatSources.stream().forEach(hs -> {
                        hs.location = hs.footPrint.getCentroid();
                        hs.location.setSRID(hs.footPrint.getSRID());
                });
        }

        public JSONObject calculateHeat() {

                storeClient = new RemoteStoreClient(queryEndpoint, updateEndpoint);
                heatSources.clear();
                numberBuildings = 0;
                numberPlantItems = 0;
                getHeatSourceProperties();
                deleteHeat();
                updateHeat();
                /*
                 * Buildings coordinates already exist.
                 * deleteLocations();
                 * setBuildingFootprints();
                 * setCityFurnitureFootprints();
                 * setHeatSourceLocations();
                 * updateLocations();
                 */

                JSONObject result = new JSONObject();
                result.put("number_plant_items", numberPlantItems);
                result.put("number_buildings", numberBuildings);
                result.put("success", "true");

                return result;

        }

}
