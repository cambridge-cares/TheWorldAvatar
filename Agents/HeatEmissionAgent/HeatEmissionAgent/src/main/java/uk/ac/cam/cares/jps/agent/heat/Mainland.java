package uk.ac.cam.cares.jps.agent.heat;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.graph.NodeFactory;
import org.json.JSONObject;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.agent.heat.objects.Factory;
import uk.ac.cam.cares.jps.agent.heat.objects.FactoryType;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class Mainland {

        private String queryEndpoint = null;
        private String updateEndpoint = null;
        private static final String rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
        private static final String rdfs = "http://www.w3.org/2000/01/rdf-schema#";
        private static final String ontoCompanyPrefix = "http://www.theworldavatar.com/kg/ontocompany/";
        private static final String ontoChemPlantPrefix = "http://www.theworldavatar.com/kg/ontochemplant/";
        private static final String ontoMeasurePrefix = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
        private static final String hasGeneratedHeat = ontoCompanyPrefix + "hasGeneratedHeat";
        private static final String generatedHeat = ontoCompanyPrefix + "GeneratedHeat";
        private static final String measure = ontoMeasurePrefix + "Measure";
        private static final String hasValue = ontoMeasurePrefix + "hasValue";
        private static final String hasNumericalValue = ontoMeasurePrefix + "hasNumericalValue";
        private static final String hasUnit = ontoMeasurePrefix + "hasUnit";
        private static final String megaWatt = ontoMeasurePrefix + "MegaWatt";
        private static final String rdfType = rdf + "type";

        private static final String ChemicalPlant = ontoChemPlantPrefix + "ChemicalPlant";
        private static final String DataCentre = ontoCompanyPrefix + "DataCentre";
        private static final String FoodPlant = ontoCompanyPrefix + "FoodPlant";
        private static final String BeveragePlant = ontoCompanyPrefix + "BeveragePlant";
        private static final String PharmaceuticalPlant = ontoCompanyPrefix + "PharmaceuticalPlant";
        private static final String PrecisionEngineeringPlant = ontoCompanyPrefix + "PrecisionEngineeringPlant";
        private static final String PrintingPlant = ontoCompanyPrefix + "PrintingPlant";

        private List<Factory> factories = new ArrayList<>();

        private Map<String, FactoryType> facilityType = new HashMap<>();

        private RemoteStoreClient storeClient;

        public Mainland(JSONObject request) {
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

        public JSONObject calculateHeat() {

                storeClient = new RemoteStoreClient(queryEndpoint, updateEndpoint);

                // Reset all variables
                factories.clear();
                facilityType.clear();

                facilityType.put(ChemicalPlant, FactoryType.ChemicalPlant);
                facilityType.put(DataCentre, FactoryType.DataCenter);
                facilityType.put(BeveragePlant, FactoryType.BeveragePlant);
                facilityType.put(FoodPlant, FactoryType.FoodPlant);

                getCompanyProperties();
                deleteHeat();
                updateHeat();

                JSONObject status = new JSONObject();
                status.put("number_factories", factories.size());
                status.put("success", "true");

                return status;
        }

        private void getCompanyProperties() {
                WhereBuilder wb = new WhereBuilder()
                                .addPrefix("ontocompany", ontoCompanyPrefix).addPrefix("rdfs", rdfs)
                                .addPrefix("om", ontoMeasurePrefix)
                                .addPrefix("rdf", rdf);

                String designCapacity = "?dc";
                String specificEnergyConsumption = "?se";
                String thermalEfficiency = "?te";
                wb.addWhere("?plant", "ontocompany:hasDesignCapacity/om:hasValue/om:hasNumericalValue", designCapacity)
                                .addWhere("?plant",
                                                "ontocompany:hasSpecificEnergyConsumption/om:hasValue/om:hasNumericalValue",
                                                specificEnergyConsumption)
                                .addWhere("?plant", "ontocompany:hasThermalEfficiency", thermalEfficiency)
                                .addWhere("?plant", "rdf:type", "?plantClass");
                SelectBuilder sb = new SelectBuilder()
                                .addVar("plant")
                                .addVar("dc")
                                .addVar("se")
                                .addVar("te")
                                .addVar("plantClass")
                                .addWhere(wb);
                JSONArray queryResult = storeClient.executeQuery(sb.buildString());

                // Process query result

                for (int i = 0; i < queryResult.length(); i++) {
                        String plantType = queryResult.getJSONObject(i).getString("plantClass");
                        Double capacity = queryResult.getJSONObject(i).getDouble("dc");
                        Double specificEnergy = queryResult.getJSONObject(i).getDouble("se");
                        Double efficiency = queryResult.getJSONObject(i).getDouble("te");
                        String iri = queryResult.getJSONObject(i).getString("plant");
                        Factory fac = new Factory(iri, facilityType.get(plantType), capacity, specificEnergy,
                                        efficiency);
                        fac.calculateHeat();
                        factories.add(fac);
                }

        }

        private void deleteHeat() {

                /*
                 * delete {?measure_instance ?p ?o} where {
                 * 
                 * ?s <http://www.theworldavatar.com/kg/ontocompany/hasGeneratedHeat> ?heat.
                 * ?heat rdf:type <http://www.theworldavatar.com/kg/ontocompany/GeneratedHeat>.
                 * ?heat <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue>
                 * ?measure_instance.
                 * ?measure_instance ?p ?o.
                 * 
                 * 
                 * }
                 * 
                 * 
                 * 
                 * delete {?heat ?p ?o} where {
                 * 
                 * ?s <http://www.theworldavatar.com/kg/ontocompany/hasGeneratedHeat> ?heat.
                 * ?heat ?p ?o.
                 * 
                 * 
                 * }
                 * 
                 * 
                 * delete {?s <http://www.theworldavatar.com/kg/ontocompany/hasGeneratedHeat>
                 * ?heat} where {
                 * 
                 * ?s <http://www.theworldavatar.com/kg/ontocompany/hasGeneratedHeat> ?heat.
                 * 
                 * 
                 * }
                 * 
                 * 
                 * 
                 */

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

                factories.stream().forEach(fac -> {

                        String heatIri = generatedHeat + "_" + UUID.randomUUID().toString();
                        String measureIri = measure + "_" + UUID.randomUUID().toString();
                        ub.addInsert(NodeFactory.createURI(fac.iri), NodeFactory.createURI(hasGeneratedHeat),
                                        NodeFactory.createURI(heatIri));
                        ub.addInsert(NodeFactory.createURI(heatIri), NodeFactory.createURI(rdfType),
                                        NodeFactory.createURI(generatedHeat));
                        ub.addInsert(NodeFactory.createURI(heatIri), NodeFactory.createURI(hasValue),
                                        NodeFactory.createURI(measureIri))
                                        .addInsert(NodeFactory.createURI(measureIri), NodeFactory.createURI(rdfType),
                                                        NodeFactory.createURI(measure))
                                        .addInsert(NodeFactory.createURI(measureIri),
                                                        NodeFactory.createURI(hasNumericalValue),
                                                        NodeFactory.createLiteral(Double.toString(fac.heatEmission),
                                                                        XSDDatatype.XSDdouble));
                        ub.addInsert(NodeFactory.createURI(measureIri), NodeFactory.createURI(hasUnit),
                                        NodeFactory.createURI(megaWatt));

                });

                storeClient.executeUpdate(ub.buildRequest().toString());

        }

}
