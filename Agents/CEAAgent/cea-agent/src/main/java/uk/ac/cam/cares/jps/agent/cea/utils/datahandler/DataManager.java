package uk.ac.cam.cares.jps.agent.cea.utils.datahandler;

import uk.ac.cam.cares.jps.agent.cea.utils.uri.BuildingURIHelper;
import uk.ac.cam.cares.jps.agent.cea.data.CEAConstants;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;

import java.util.*;
import java.util.stream.Stream;

public class DataManager {
    private OntologyURIHelper ontologyUriHelper;

    public DataManager(OntologyURIHelper uriHelper) {
        this.ontologyUriHelper = uriHelper;
    }

    /**
     * Checks building linked to ontoCityGML is initialised in KG and is a bot:Building instance
     * @param uriString city object id
     * @param route route to pass to access agent
     * @return building
     */
    public String checkBuildingInitialised(String uriString, String route) {
        WhereBuilder wb = new WhereBuilder();
        SelectBuilder sb = new SelectBuilder();

        wb.addPrefix("rdf", ontologyUriHelper.getOntologyUri(OntologyURIHelper.rdf))
                .addPrefix("ontoBuiltEnv", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontobuiltenv))
                .addPrefix("bot", ontologyUriHelper.getOntologyUri(OntologyURIHelper.bot))
                .addWhere("?building", "ontoBuiltEnv:hasOntoCityGMLRepresentation", "?s")
                .addWhere("?building", "rdf:type", "bot:Building");

        sb.addVar("?building").addWhere(wb);

        sb.setVar( Var.alloc( "s" ), NodeFactory.createURI(BuildingURIHelper.getBuildingUri(uriString)));

        JSONArray queryResultArray = new JSONArray(AccessAgentCaller.queryStore(route, sb.build().toString()));
        String building = "";
        if(!queryResultArray.isEmpty()){
            building = queryResultArray.getJSONObject(0).get("building").toString();
        }
        return building;
    }

    /**
     * Initialises building in KG with buildingUri as the bot:Building IRI, and link to ontoCityGMLRepresentation
     * @param uriString city object id
     * @param buildingUri building IRI from other endpoints if exist
     * @param route route to pass to access agent
     * @param graph graph name
     * @return building
     */
    public String initialiseBuilding(String uriString, String buildingUri, String route, String graph) {

        UpdateBuilder ub = new UpdateBuilder();

        if (buildingUri.isEmpty()) {
            if (!graph.isEmpty()) {
                buildingUri = graph + "Building_" + UUID.randomUUID() + "/";
            }
            else{
                buildingUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontobuiltenv) + "Building_" + UUID.randomUUID() + "/";
            }
        }

        WhereBuilder wb =
                new WhereBuilder()
                        .addPrefix("rdf", ontologyUriHelper.getOntologyUri(OntologyURIHelper.rdf))
                        .addPrefix("owl", ontologyUriHelper.getOntologyUri(OntologyURIHelper.owl))
                        .addPrefix("bot", ontologyUriHelper.getOntologyUri(OntologyURIHelper.bot))
                        .addPrefix("ontoBuiltEnv", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontobuiltenv))
                        .addWhere(NodeFactory.createURI(buildingUri), "rdf:type", "bot:Building")
                        .addWhere(NodeFactory.createURI(buildingUri), "rdf:type", "owl:NamedIndividual")
                        .addWhere(NodeFactory.createURI(buildingUri), "ontoBuiltEnv:hasOntoCityGMLRepresentation", NodeFactory.createURI(BuildingURIHelper.getBuildingUri(uriString)));

        if (!graph.isEmpty()){
            ub.addInsert(NodeFactory.createURI(graph), wb);
        }
        else{
            ub.addInsert(wb);
        }

        UpdateRequest ur = ub.buildRequest();

        //Use access agent
        AccessAgentCaller.updateStore(route, ur.toString());

        return buildingUri;
    }

    /**
     * Checks if energy profile data already exist in KG and get IRIs if they do
     * @param building building uri in energy profile graph
     * @param tsIris map of time series iris to data types
     * @param scalarIris map of iris in kg to data type
     * @param route route to pass to access agent
     * @return if time series are initialised
     */
    public Boolean checkDataInitialised(String building, LinkedHashMap<String,String> tsIris, LinkedHashMap<String,String> scalarIris, String route) {
        DataRetriever dataRetriever = new DataRetriever(ontologyUriHelper);

        ArrayList<String> result;
        List<String> allMeasures = new ArrayList<>();
        Stream.of(CEAConstants.TIME_SERIES, CEAConstants.SCALARS).forEach(allMeasures::addAll);

        for (String measurement: allMeasures) {
            result = dataRetriever.getDataIRI(building, measurement, route);
            if (!result.isEmpty()) {
                if (CEAConstants.TIME_SERIES.contains(measurement)) {
                    tsIris.put(measurement, result.get(0));
                } else {
                    scalarIris.put(measurement, result.get(0));
                }
            } else {
                return false;
            }
        }
        return true;
    }

    /**
     * Initialises energy profile data in KG
     * @param uriCounter keep track of uris
     * @param scalars map of scalar measurements
     * @param buildingUri building uri
     * @param tsIris map of time series iris to data types
     * @param scalarIris map of iris in kg to data types
     * @param route route to pass to access agent
     * @param graph graph name
     */
    public void initialiseData(Integer uriCounter, LinkedHashMap<String, List<String>> scalars, String buildingUri, LinkedHashMap<String,String> tsIris, LinkedHashMap<String,String> scalarIris, String route, String graph){

        WhereBuilder wb =
                new WhereBuilder()
                        .addPrefix("ontoubemmp", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP))
                        .addPrefix("rdf", ontologyUriHelper.getOntologyUri(OntologyURIHelper.rdf))
                        .addPrefix("owl", ontologyUriHelper.getOntologyUri(OntologyURIHelper.owl))
                        .addPrefix("om", ontologyUriHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                        .addPrefix("bot", ontologyUriHelper.getOntologyUri(OntologyURIHelper.bot))
                        .addPrefix("obs", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontobuiltstructure));

        UpdateBuilder ub = new UpdateBuilder();

        //Device uris
        String pvRoofPanelUri = "PVRoofPanel_" + UUID.randomUUID() + "/";
        String pvWallSouthPanelUri = "PVWallSouthPanel_" + UUID.randomUUID() + "/";
        String pvWallNorthPanelUri = "PVWallNorthPanel_" + UUID.randomUUID() + "/";
        String pvWallEastPanelUri = "PVWallEastPanel_" + UUID.randomUUID() + "/";
        String pvWallWestPanelUri = "PVWallWestPanel_" + UUID.randomUUID() + "/";
        String pvtPlateRoofCollectorUri = "PVTPlateRoofCollector_" + UUID.randomUUID() + "/";
        String pvtPlateWallSouthCollectorUri = "PVTPlateWallSouthCollector_" + UUID.randomUUID() + "/";
        String pvtPlateWallNorthCollectorUri = "PVTPlateWallNorthCollector_" + UUID.randomUUID() + "/";
        String pvtPlateWallEastCollectorUri = "PVTPlateWallEastCollector_" + UUID.randomUUID() + "/";
        String pvtPlateWallWestCollectorUri = "PVTPlateWallWestCollector_" + UUID.randomUUID() + "/";
        String pvtTubeRoofCollectorUri = "PVTTubeRoofCollector_" + UUID.randomUUID() + "/";
        String pvtTubeWallSouthCollectorUri = "PVTTubeWallSouthCollector_" + UUID.randomUUID() + "/";
        String pvtTubeWallNorthCollectorUri = "PVTTubeWallNorthCollector_" + UUID.randomUUID() + "/";
        String pvtTubeWallEastCollectorUri = "PVTTubeWallEastCollector_" + UUID.randomUUID() + "/";
        String pvtTubeWallWestCollectorUri = "PVTTubeWallWestCollector_" + UUID.randomUUID() + "/";
        String thermalPlateRoofCollectorUri = "ThermalPlateRoofCollector_" + UUID.randomUUID() + "/";
        String thermalPlateWallSouthCollectorUri = "ThermalPlateWallSouthCollector_" + UUID.randomUUID() + "/";
        String thermalPlateWallNorthCollectorUri = "ThermalPlateWallNorthCollector_" + UUID.randomUUID() + "/";
        String thermalPlateWallEastCollectorUri = "ThermalPlateWallEastCollector_" + UUID.randomUUID() + "/";
        String thermalPlateWallWestCollectorUri = "ThermalPlateWallWestCollector_" + UUID.randomUUID() + "/";
        String thermalTubeRoofCollectorUri = "ThermalTubeRoofCollector_" + UUID.randomUUID() + "/";
        String thermalTubeWallSouthCollectorUri = "ThermalTubeWallSouthCollector_" + UUID.randomUUID() + "/";
        String thermalTubeWallNorthCollectorUri = "ThermalTubeWallNorthCollector_" + UUID.randomUUID() + "/";
        String thermalTubeWallEastCollectorUri = "ThermalTubeWallEastCollector_" + UUID.randomUUID() + "/";
        String thermalTubeWallWestCollectorUri = "ThermalTubeWallWestCollector_" + UUID.randomUUID() + "/";


        if (!graph.isEmpty()){
            pvRoofPanelUri = graph + pvRoofPanelUri;
            pvWallSouthPanelUri = graph + pvWallSouthPanelUri;
            pvWallNorthPanelUri = graph + pvWallNorthPanelUri;
            pvWallEastPanelUri = graph + pvWallEastPanelUri;
            pvWallWestPanelUri = graph + pvWallWestPanelUri;
            pvtPlateRoofCollectorUri = graph + pvtPlateRoofCollectorUri;
            pvtPlateWallSouthCollectorUri = graph + pvtPlateWallSouthCollectorUri;
            pvtPlateWallNorthCollectorUri = graph + pvtPlateWallNorthCollectorUri;
            pvtPlateWallEastCollectorUri = graph + pvtPlateWallEastCollectorUri;
            pvtPlateWallWestCollectorUri = graph + pvtPlateWallWestCollectorUri;
            pvtTubeRoofCollectorUri = graph + pvtTubeRoofCollectorUri;
            pvtTubeWallSouthCollectorUri = graph + pvtTubeWallSouthCollectorUri;
            pvtTubeWallNorthCollectorUri = graph + pvtTubeWallNorthCollectorUri;
            pvtTubeWallEastCollectorUri = graph + pvtTubeWallEastCollectorUri;
            pvtTubeWallWestCollectorUri = graph + pvtTubeWallWestCollectorUri;
            thermalPlateRoofCollectorUri = graph + thermalPlateRoofCollectorUri;
            thermalPlateWallSouthCollectorUri = graph + thermalPlateWallSouthCollectorUri;
            thermalPlateWallNorthCollectorUri = graph + thermalPlateWallNorthCollectorUri;
            thermalPlateWallEastCollectorUri = graph + thermalPlateWallEastCollectorUri;
            thermalPlateWallWestCollectorUri = graph + thermalPlateWallWestCollectorUri;
            thermalTubeRoofCollectorUri = graph + thermalTubeRoofCollectorUri;
            thermalTubeWallSouthCollectorUri = graph + thermalTubeWallSouthCollectorUri;
            thermalTubeWallNorthCollectorUri = graph + thermalTubeWallNorthCollectorUri;
            thermalTubeWallEastCollectorUri = graph + thermalTubeWallEastCollectorUri;
            thermalTubeWallWestCollectorUri = graph + thermalTubeWallWestCollectorUri;
        }
        else{
            pvRoofPanelUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvRoofPanelUri;
            pvWallSouthPanelUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvWallSouthPanelUri;
            pvWallNorthPanelUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvWallNorthPanelUri;
            pvWallEastPanelUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvWallEastPanelUri;
            pvWallWestPanelUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvWallWestPanelUri;
            pvtPlateRoofCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtPlateRoofCollectorUri;
            pvtPlateWallSouthCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtPlateWallSouthCollectorUri;
            pvtPlateWallNorthCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtPlateWallNorthCollectorUri;
            pvtPlateWallEastCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtPlateWallEastCollectorUri;
            pvtPlateWallWestCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtPlateWallWestCollectorUri;
            pvtTubeRoofCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtTubeRoofCollectorUri;
            pvtTubeWallSouthCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtTubeWallSouthCollectorUri;
            pvtTubeWallNorthCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtTubeWallNorthCollectorUri;
            pvtTubeWallEastCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtTubeWallEastCollectorUri;
            pvtTubeWallWestCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtTubeWallWestCollectorUri;
            thermalPlateRoofCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalPlateRoofCollectorUri;
            thermalPlateWallSouthCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalPlateWallSouthCollectorUri;
            thermalPlateWallNorthCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalPlateWallNorthCollectorUri;
            thermalPlateWallEastCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalPlateWallEastCollectorUri;
            thermalPlateWallWestCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalPlateWallWestCollectorUri;
            thermalTubeRoofCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalTubeRoofCollectorUri;
            thermalTubeWallSouthCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalTubeWallSouthCollectorUri;
            thermalTubeWallNorthCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalTubeWallNorthCollectorUri;
            thermalTubeWallEastCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalTubeWallEastCollectorUri;
            thermalTubeWallWestCollectorUri = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalTubeWallWestCollectorUri;
        }

        Map<String, String> facades = new HashMap<>();

        // save om:Measure uris for scalars and create om:Quantity uris for scalars and time series
        // (time series om:Measure iris already created in createTimeSeries)
        for (String measurement: CEAConstants.SCALARS) {
            String measure = measurement + UUID.randomUUID() + "/";
            String quantity = measurement + "Quantity_" + UUID.randomUUID() + "/";
            String facade = measurement.split("SolarSuitableArea")[0] + UUID.randomUUID() + "/";
            if (!graph.isEmpty()){
                measure = graph + measure;
                quantity = graph + quantity;
                facade = graph + facade;
            }
            else{
                measure = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + measure;
                quantity = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + quantity;
                facade = ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + facade;
            }
            scalarIris.put(measurement, measure);

            switch(measurement){
                case(CEAConstants.KEY_ROOF_SOLAR_SUITABLE_AREA):
                    createFacadeUpdate(wb, buildingUri, facade, "obs:RoofFacade");
                    createSolarSuitableAreaUpdate(wb, facade, quantity, measure, scalars.get(CEAConstants.KEY_ROOF_SOLAR_SUITABLE_AREA).get(uriCounter));
                    facades.put("Roof", facade);
                    break;
                case(CEAConstants.KEY_SOUTH_WALL_SOLAR_SUITABLE_AREA):
                    createFacadeUpdate(wb, buildingUri, facade, "obs:SouthWallFacade");
                    createSolarSuitableAreaUpdate(wb, facade, quantity, measure, scalars.get(CEAConstants.KEY_SOUTH_WALL_SOLAR_SUITABLE_AREA).get(uriCounter));
                    facades.put("SouthWall", facade);
                    break;
                case(CEAConstants.KEY_NORTH_WALL_SOLAR_SUITABLE_AREA):
                    createFacadeUpdate(wb, buildingUri, facade, "obs:NorthWallFacade");
                    createSolarSuitableAreaUpdate(wb, facade, quantity, measure, scalars.get(CEAConstants.KEY_NORTH_WALL_SOLAR_SUITABLE_AREA).get(uriCounter));
                    facades.put("NorthWall", facade);
                    break;
                case(CEAConstants.KEY_EAST_WALL_SOLAR_SUITABLE_AREA):
                    createFacadeUpdate(wb, buildingUri, facade, "obs:EastWallFacade");
                    createSolarSuitableAreaUpdate(wb, facade, quantity, measure, scalars.get(CEAConstants.KEY_EAST_WALL_SOLAR_SUITABLE_AREA).get(uriCounter));
                    facades.put("EastWall", facade);
                    break;
                case(CEAConstants.KEY_WEST_WALL_SOLAR_SUITABLE_AREA):
                    createFacadeUpdate(wb, buildingUri, facade, "obs:WestWallFacade");
                    createSolarSuitableAreaUpdate(wb, facade, quantity, measure, scalars.get(CEAConstants.KEY_WEST_WALL_SOLAR_SUITABLE_AREA).get(uriCounter));
                    facades.put("WestWall", facade);
                    break;
            }
        }

        for (String measurement: CEAConstants.TIME_SERIES) {
            String quantity = measurement+"Quantity_" + UUID.randomUUID() + "/";
            quantity = !graph.isEmpty() ? graph + quantity : ontologyUriHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + quantity;
            if (measurement.equals(CEAConstants.KEY_GRID_CONSUMPTION) || measurement.equals(CEAConstants.KEY_ELECTRICITY_CONSUMPTION) || measurement.equals(CEAConstants.KEY_COOLING_CONSUMPTION) || measurement.equals(CEAConstants.KEY_HEATING_CONSUMPTION)) {
                createConsumptionUpdate(wb, buildingUri, "ontoubemmp:" + measurement, quantity, tsIris.get(measurement));
            }
            else if (measurement.equals(CEAConstants.KEY_PV_ROOF_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("Roof"), pvRoofPanelUri, "ontoubemmp:PVPanel", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PV_WALL_SOUTH_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("SouthWall"), pvWallSouthPanelUri, "ontoubemmp:PVPanel", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PV_WALL_NORTH_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("NorthWall"), pvWallNorthPanelUri, "ontoubemmp:PVPanel", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PV_WALL_EAST_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("EastWall"), pvWallEastPanelUri, "ontoubemmp:PVPanel", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PV_WALL_WEST_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("WestWall"), pvWallWestPanelUri, "ontoubemmp:PVPanel", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_PLATE_ROOF_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("Roof"), pvtPlateRoofCollectorUri, quantity, "ontoubemmp:PVTPlateCollector", tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_PLATE_WALL_SOUTH_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("SouthWall"), pvtPlateWallSouthCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_PLATE_WALL_NORTH_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("NorthWall"), pvtPlateWallNorthCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_PLATE_WALL_EAST_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("EastWall"), pvtPlateWallEastCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_PLATE_WALL_WEST_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("WestWall"), pvtPlateWallWestCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_PLATE_ROOF_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("Roof"), pvtPlateRoofCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_PLATE_WALL_SOUTH_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("SouthWall"), pvtPlateWallSouthCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_PLATE_WALL_NORTH_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("NorthWall"), pvtPlateWallNorthCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_PLATE_WALL_EAST_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("EastWall"), pvtPlateWallEastCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_PLATE_WALL_WEST_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("WestWall"), pvtPlateWallWestCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_TUBE_ROOF_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("Roof"), pvtTubeRoofCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_TUBE_WALL_SOUTH_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("SouthWall"), pvtTubeWallSouthCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_TUBE_WALL_NORTH_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("NorthWall"), pvtTubeWallNorthCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_TUBE_WALL_EAST_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("EastWall"), pvtTubeWallEastCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_TUBE_WALL_WEST_E_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("WestWall"), pvtTubeWallWestCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_TUBE_ROOF_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("Roof"), pvtTubeRoofCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_TUBE_WALL_SOUTH_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("SouthWall"), pvtTubeWallSouthCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_TUBE_WALL_NORTH_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("NorthWall"), pvtTubeWallNorthCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_TUBE_WALL_EAST_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("EastWall"), pvtTubeWallEastCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_PVT_TUBE_WALL_WEST_Q_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("WestWall"), pvtTubeWallWestCollectorUri, "ontoubemmp:PVTTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_THERMAL_PLATE_ROOF_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("Roof"), thermalPlateRoofCollectorUri, "ontoubemmp:ThermalPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_THERMAL_PLATE_WALL_SOUTH_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("SouthWall"), thermalPlateWallSouthCollectorUri, "ontoubemmp:ThermalPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_THERMAL_PLATE_WALL_NORTH_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("NorthWall"), thermalPlateWallNorthCollectorUri, "ontoubemmp:ThermalPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_THERMAL_PLATE_WALL_EAST_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("EastWall"), thermalPlateWallEastCollectorUri, "ontoubemmp:ThermalPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_THERMAL_PLATE_WALL_WEST_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("WestWall"), thermalPlateWallWestCollectorUri, "ontoubemmp:ThermalPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_THERMAL_TUBE_ROOF_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("Roof"), thermalTubeRoofCollectorUri, "ontoubemmp:ThermalTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_THERMAL_TUBE_WALL_SOUTH_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("SouthWall"), thermalTubeWallSouthCollectorUri, "ontoubemmp:ThermalTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_THERMAL_TUBE_WALL_NORTH_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("NorthWall"), thermalTubeWallNorthCollectorUri, "ontoubemmp:ThermalTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_THERMAL_TUBE_WALL_EAST_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("EastWall"), thermalTubeWallEastCollectorUri, "ontoubemmp:ThermalTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
            else if (measurement.equals(CEAConstants.KEY_THERMAL_TUBE_WALL_WEST_SUPPLY)){
                createSolarGeneratorSupplyUpdate(wb, facades.get("WestWall"), thermalTubeWallWestCollectorUri, "ontoubemmp:ThermalTubeCollector", quantity, tsIris.get(measurement), "ontoubemmp:HeatSupply");
            }
        }

        if (graph.isEmpty()){
            ub.addInsert(wb);
        }
        else{
            ub.addInsert(NodeFactory.createURI(graph), wb);
        }

        UpdateRequest ur = ub.buildRequest();

        //Use access agent
        AccessAgentCaller.updateStore(route, ur.toString());
    }

    /**
     * Updates numerical value of scalars in KG
     * @param scalars map of scalar measurements
     * @param scalarIris map of iris in kg to data types
     * @param route route to pass to access agent
     * @param uriCounter keep track of uris
     * @param graph graph name
     */
    public void updateScalars(String route, LinkedHashMap<String,String> scalarIris, LinkedHashMap<String, List<String>> scalars, Integer uriCounter, String graph) {
        for (String measurement: CEAConstants.SCALARS) {
            WhereBuilder wb1 = new WhereBuilder().addPrefix("om", ontologyUriHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                    .addWhere(NodeFactory.createURI(scalarIris.get(measurement)), "om:hasNumericalValue", "?s");
            UpdateBuilder ub1 = new UpdateBuilder().addPrefix("om", ontologyUriHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                    .addWhere(wb1);

            WhereBuilder wb2 = new WhereBuilder().addPrefix("om", ontologyUriHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                    .addWhere(NodeFactory.createURI(scalarIris.get(measurement)), "om:hasNumericalValue", scalars.get(measurement).get(uriCounter));
            UpdateBuilder ub2 = new UpdateBuilder().addPrefix("om", ontologyUriHelper.getOntologyUri(OntologyURIHelper.unitOntology));

            if (!graph.isEmpty()){
                ub1.addDelete(NodeFactory.createURI(graph), wb1);
                ub2.addInsert(NodeFactory.createURI(graph), wb2);
            }
            else{
                ub1.addDelete(wb1);
                ub2.addInsert(wb2);
            }

            UpdateRequest ur1 = ub1.buildRequest();
            UpdateRequest ur2 = ub2.buildRequest();

            //Use access agent
            AccessAgentCaller.updateStore(route, ur1.toString());
            AccessAgentCaller.updateStore(route, ur2.toString());
        }
    }

    /**
     * Creates updates for building facades
     * @param builder update builder
     * @param building building iri
     * @param facade facade iri
     * @param facadeType type of facade
     */
    public void createFacadeUpdate(WhereBuilder builder, String building, String facade, String facadeType) {
        builder.addWhere(NodeFactory.createURI(building), "obs:hasFacade", NodeFactory.createURI(facade))
                .addWhere(NodeFactory.createURI(facade), "rdf:type", facadeType);
    }

    /**
     * Creates update for energy consumption
     * @param builder update builder
     * @param consumer iri of building/device
     * @param consumptionType type in ontology
     * @param quantity om:Quantity iri
     * @param measure om:Measure iri
     */
    public void createConsumptionUpdate(WhereBuilder builder, String consumer, String consumptionType, String quantity, String measure){
        builder.addWhere(NodeFactory.createURI(quantity), "rdf:type", consumptionType)
                .addWhere(NodeFactory.createURI(quantity), "rdf:type", "owl:NamedIndividual")
                .addWhere(NodeFactory.createURI(quantity), "om:hasDimension", "om:energy-Dimension")
                .addWhere(NodeFactory.createURI(quantity), "om:hasValue", NodeFactory.createURI(measure))
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "om:Measure")
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "owl:NamedIndividual")
                .addWhere(NodeFactory.createURI(measure), "om:hasUnit", "om:kilowattHour")
                .addWhere(NodeFactory.createURI(consumer), "ontoubemmp:consumesEnergy",NodeFactory.createURI(quantity));
    }

    /**
     * Creates update for solar energy generators supply
     * @param builder update builder
     * @param facade facade iri
     * @param solarGenerator solar energy generator iri
     * @param solarGeneratorType type of solar energy generator
     * @param quantity om:Quantity iri
     * @param measure om:Measure iri
     */
    public void createSolarGeneratorSupplyUpdate(WhereBuilder builder, String facade, String solarGenerator, String solarGeneratorType, String quantity, String measure, String energySupply){
        builder.addWhere(NodeFactory.createURI(facade), "ontoubemmp:hasTheoreticalEnergyProduction", NodeFactory.createURI(solarGenerator))
                .addWhere(NodeFactory.createURI(solarGenerator), "rdf:type", solarGeneratorType)
                .addWhere(NodeFactory.createURI(solarGenerator), "ontoubemmp:producesEnergy", NodeFactory.createURI(quantity))
                .addWhere(NodeFactory.createURI(quantity), "rdf:type", energySupply)
                .addWhere(NodeFactory.createURI(quantity), "rdf:type", "owl:NamedIndividual")
                .addWhere(NodeFactory.createURI(quantity), "om:hasDimension", "om:energy-Dimension")
                .addWhere(NodeFactory.createURI(quantity), "om:hasValue", NodeFactory.createURI(measure))
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "om:Measure")
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "owl:NamedIndividual")
                .addWhere(NodeFactory.createURI(measure), "om:hasUnit", "om:kilowattHour");
    }

    /**
     * Creates update for solar suitable areas
     * @param builder update builder
     * @param facade obs:facade iri
     * @param quantity om:Quantity iri
     * @param measure om:Measure iri
     * @param value numerical value
     */
    public void createSolarSuitableAreaUpdate(WhereBuilder builder, String facade, String quantity, String measure, String value) {
        builder.addWhere(NodeFactory.createURI(facade), "ontoubemmp:hasSolarSuitableArea", NodeFactory.createURI(quantity))
                .addWhere(NodeFactory.createURI(quantity), "rdf:type", "om:Area")
                .addWhere(NodeFactory.createURI(quantity), "rdf:type", "owl:NamedIndividual")
                .addWhere(NodeFactory.createURI(quantity), "om:hasDimension", "om:area-Dimension")
                .addWhere(NodeFactory.createURI(quantity), "om:hasValue", NodeFactory.createURI(measure))
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "owl:NamedIndividual")
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "om:Measure")
                .addWhere(NodeFactory.createURI(measure), "om:hasNumericalValue", value)
                .addWhere(NodeFactory.createURI(measure), "om:hasUnit", "om:squareMetre");
    }
}
