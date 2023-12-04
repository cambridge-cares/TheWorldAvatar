package uk.ac.cam.cares.jps.agent.cea.utils.datahandler;

import org.apache.jena.arq.querybuilder.AskBuilder;
import uk.ac.cam.cares.jps.agent.cea.data.CEAConstants;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;

import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;

import java.util.*;
import java.util.stream.Stream;

public class DataManager {
    /**
     * Checks if uriString is initialised in KG and is a gml:Building instance
     * @param uriString building IRI
     * @param route route to pass to access agent
     * @return building
     */
    public static boolean checkBuildingInitialised(String uriString, String route) {
        WhereBuilder wb = new WhereBuilder();
        AskBuilder ab = new AskBuilder();

        wb.addPrefix("rdf", OntologyURIHelper.getOntologyUri(OntologyURIHelper.rdf))
                .addPrefix("ontoBuiltEnv", OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontobuiltenv))
                .addPrefix("gml", OntologyURIHelper.getOntologyUri(OntologyURIHelper.gml))
                .addWhere("?building", "rdf:type", "gml:Building");

        ab.addWhere(wb);

        ab.setVar(Var.alloc( "building"), NodeFactory.createURI(uriString));

        JSONArray queryResultArray = new JSONArray(AccessAgentCaller.queryStore(route, ab.build().toString()));

        return queryResultArray.getJSONObject(0).getBoolean("ASK");
    }

    /**
     * Initialises building in KG with buildingUri as the gml:Building IRI
     * @param buildingUri building IRI from other endpoints if exist
     * @param route route to pass to access agent
     */
    public static void initialiseBuilding(String buildingUri, String route) {
        UpdateBuilder ub = new UpdateBuilder();

        WhereBuilder wb =
                new WhereBuilder()
                        .addPrefix("rdf", OntologyURIHelper.getOntologyUri(OntologyURIHelper.rdf))
                        .addPrefix("owl", OntologyURIHelper.getOntologyUri(OntologyURIHelper.owl))
                        .addPrefix("gml", OntologyURIHelper.getOntologyUri(OntologyURIHelper.gml))
                        .addWhere(NodeFactory.createURI(buildingUri), "rdf:type", "gml:Building")
                        .addWhere(NodeFactory.createURI(buildingUri), "rdf:type", "owl:NamedIndividual");

        ub.addInsert(wb);

        UpdateRequest ur = ub.buildRequest();

        AccessAgentCaller.updateStore(route, ur.toString());
    }

    /**
     * Checks if CEA output data already exist in KG and get IRIs if they do
     * @param building building IRI
     * @param tsIris map of time series IRIs to data types
     * @param scalarIris map of IRIs in KG to data type
     * @param route route to pass to access agent
     * @return if CEA output data  are initialised
     */
    public static Boolean checkDataInitialised(String building, LinkedHashMap<String,String> tsIris, LinkedHashMap<String,String> scalarIris, String route) {
        ArrayList<String> result;
        List<String> allMeasures = new ArrayList<>();
        Stream.of(CEAConstants.TIME_SERIES, CEAConstants.SCALARS).forEach(allMeasures::addAll);

        for (String measurement: allMeasures) {
            result = DataRetriever.getDataIRI(building, measurement, route);
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
     * Initialises CEA output data in KG
     * @param uriCounter keep track of URIs
     * @param scalars map of scalar measurements
     * @param buildingUri building IRI
     * @param tsIris map of time series IRIs to data types
     * @param scalarIris map of IRIs in KG to data types
     * @param route route to pass to access agent
     */
    public static void initialiseData(Integer uriCounter, LinkedHashMap<String, List<Double>> scalars, String buildingUri, LinkedHashMap<String,String> tsIris, LinkedHashMap<String,String> scalarIris, String route) {
        WhereBuilder wb =
                new WhereBuilder()
                        .addPrefix("ontoubemmp", OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP))
                        .addPrefix("rdf", OntologyURIHelper.getOntologyUri(OntologyURIHelper.rdf))
                        .addPrefix("owl", OntologyURIHelper.getOntologyUri(OntologyURIHelper.owl))
                        .addPrefix("om", OntologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                        .addPrefix("obs", OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontobuiltstructure));

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

        pvRoofPanelUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvRoofPanelUri;
        pvWallSouthPanelUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvWallSouthPanelUri;
        pvWallNorthPanelUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvWallNorthPanelUri;
        pvWallEastPanelUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvWallEastPanelUri;
        pvWallWestPanelUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvWallWestPanelUri;
        pvtPlateRoofCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtPlateRoofCollectorUri;
        pvtPlateWallSouthCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtPlateWallSouthCollectorUri;
        pvtPlateWallNorthCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtPlateWallNorthCollectorUri;
        pvtPlateWallEastCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtPlateWallEastCollectorUri;
        pvtPlateWallWestCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtPlateWallWestCollectorUri;
        pvtTubeRoofCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtTubeRoofCollectorUri;
        pvtTubeWallSouthCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtTubeWallSouthCollectorUri;
        pvtTubeWallNorthCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtTubeWallNorthCollectorUri;
        pvtTubeWallEastCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtTubeWallEastCollectorUri;
        pvtTubeWallWestCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + pvtTubeWallWestCollectorUri;
        thermalPlateRoofCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalPlateRoofCollectorUri;
        thermalPlateWallSouthCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalPlateWallSouthCollectorUri;
        thermalPlateWallNorthCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalPlateWallNorthCollectorUri;
        thermalPlateWallEastCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalPlateWallEastCollectorUri;
        thermalPlateWallWestCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalPlateWallWestCollectorUri;
        thermalTubeRoofCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalTubeRoofCollectorUri;
        thermalTubeWallSouthCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalTubeWallSouthCollectorUri;
        thermalTubeWallNorthCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalTubeWallNorthCollectorUri;
        thermalTubeWallEastCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalTubeWallEastCollectorUri;
        thermalTubeWallWestCollectorUri = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + thermalTubeWallWestCollectorUri;

        Map<String, String> facades = new HashMap<>();

        // save om:Measure uris for scalars and create om:Quantity uris for scalars and time series
        // (time series om:Measure iris already created in createTimeSeries)
        for (String measurement: CEAConstants.SCALARS) {
            String measure = measurement + "_" + UUID.randomUUID() + "/";
            String quantity = measurement + "Quantity_" + UUID.randomUUID() + "/";
            String facade = measurement.split("SolarSuitableArea")[0] + "_" + UUID.randomUUID() + "/";

            measure = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + measure;
            quantity = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + quantity;
            facade = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + facade;

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
            quantity = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + quantity;
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
                createSolarGeneratorSupplyUpdate(wb, facades.get("Roof"), pvtPlateRoofCollectorUri, "ontoubemmp:PVTPlateCollector", quantity, tsIris.get(measurement), "ontoubemmp:ElectricitySupply");
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

        ub.addInsert(wb);

        UpdateRequest ur = ub.buildRequest();

        //Use access agent
        AccessAgentCaller.updateStore(route, ur.toString());
    }

    /**
     * Updates numerical value of scalars in KG
     * @param scalars map of scalar measurements
     * @param scalarIris map of IRIs in KG to data types
     * @param route route to pass to access agent
     * @param uriCounter keep track of URIs
     */
    public static void updateScalars(String route, LinkedHashMap<String,String> scalarIris, LinkedHashMap<String, List<Double>> scalars, Integer uriCounter) {
        for (String measurement: CEAConstants.SCALARS) {
            WhereBuilder wb1 = new WhereBuilder().addPrefix("om", OntologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                    .addWhere(NodeFactory.createURI(scalarIris.get(measurement)), "om:hasNumericalValue", "?s");
            UpdateBuilder ub1 = new UpdateBuilder().addPrefix("om", OntologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                    .addWhere(wb1);

            WhereBuilder wb2 = new WhereBuilder().addPrefix("om", OntologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                    .addWhere(NodeFactory.createURI(scalarIris.get(measurement)), "om:hasNumericalValue", scalars.get(measurement).get(uriCounter));
            UpdateBuilder ub2 = new UpdateBuilder().addPrefix("om", OntologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology));

            ub1.addDelete(wb1);
            ub2.addInsert(wb2);

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
     * @param building building IRI
     * @param facade facade IRI
     * @param facadeType type of facade
     */
    public static void createFacadeUpdate(WhereBuilder builder, String building, String facade, String facadeType) {
        builder.addWhere(NodeFactory.createURI(building), "obs:hasFacade", NodeFactory.createURI(facade))
                .addWhere(NodeFactory.createURI(facade), "rdf:type", facadeType);
    }

    /**
     * Creates update for energy consumption
     * @param builder update builder
     * @param consumer IRI of building/device
     * @param consumptionType type in ontology
     * @param quantity om:Quantity IRI
     * @param measure om:Measure IRI
     */
    public static void createConsumptionUpdate(WhereBuilder builder, String consumer, String consumptionType, String quantity, String measure){
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
     * @param facade facade IRI
     * @param solarGenerator solar energy generator IRI
     * @param solarGeneratorType type of solar energy generator
     * @param quantity om:Quantity IRI
     * @param measure om:Measure IRI
     */
    public static void createSolarGeneratorSupplyUpdate(WhereBuilder builder, String facade, String solarGenerator, String solarGeneratorType, String quantity, String measure, String energySupply){
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
     * @param facade obs:Facade IRI
     * @param quantity om:Quantity IRI
     * @param measure om:Measure IRI
     * @param value numerical value
     */
    public static void createSolarSuitableAreaUpdate(WhereBuilder builder, String facade, String quantity, String measure, Double value) {
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
