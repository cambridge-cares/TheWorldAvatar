package uk.ac.cam.cares.jps.agent.cea.tasks;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import uk.ac.cam.cares.jps.agent.cea.data.CEAOutputData;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class CEAOutputHandler {
    private static final String FS = System.getProperty("file.separator");
    private static final Map<String, List<String>> SOLAR_SUPPLY;

    private static final String ROOF_AREA = "PV_roof_tops_m2";
    private static final String NORTH_AREA = "PV_walls_north_tops_m2";
    private static final String SOUTH_AREA = "PV_walls_south_tops_m2";
    private static final String WEST_AREA = "PV_walls_west_tops_m2";
    private static final String EAST_AREA = "PV_walls_east_tops_m2";

    private static final String GRID = "GRID_kWh";
    private static final String ELECTRICITY = "E_sys_kWh";
    private static final String HEATING = "QH_sys_kWh";
    private static final String COOLING = "QC_sys_kWh";

    private static final String ROOF_SUPPLY = "roofs_top_supply_kWH";
    private static final String NORTH_SUPPLY = "walls_south_supply_kWH";
    private static final String SOUTH_SUPPLY = "walls_north_supply_kWH";
    private static final String WEST_SUPPLY = "walls_west_supply_kWH";
    private static final String EAST_SUPPLY = "walls_east_supply_kWH";


    /**
     * Intialise SOLAR_SUPPLY
     */
    static {
        Map<String, List<String>> map = new HashMap<>();
        List<String> EQ = new ArrayList<>();
        List<String> E = new ArrayList<>();
        List<String> Q = new ArrayList<>();

        EQ.add("E");
        EQ.add("Q");
        E.add("E");
        Q.add("Q");

        map.put("PV", E);
        map.put("PVT_FP", EQ);
        map.put("PVT_ET", EQ);
        map.put("SC_FP", Q);
        map.put("SC_ET", Q);

        SOLAR_SUPPLY = map;
    }

    /**
     * Extracts CEA outputs from CSVs and return them stored as CEAOutputData
     * @param directoryPath CEA output data directory path
     * @param iris list of building IRIs
     * @return CEAOutputData
     */
    public static CEAOutputData extractCEAOutputs(String directoryPath, List<String> iris) throws IOException {
        CEAOutputData result = new CEAOutputData();

        String demandPath = directoryPath + FS + "demands";
        String solarPath = directoryPath + FS + "potentials" + FS + "solar";

        result.iris = iris;

        result = extractArea(solarPath + FS + "PV_total_buildings.csv", result);
        result.times = extractTimes(directoryPath);

        List<String> geometryNames = getGeometryNames(solarPath + FS + "PV_total.csv");

        for (String geometryName : geometryNames) {
            Pattern pattern = Pattern.compile("^B\\d{5}_(\\d+)");
            Matcher matcher = pattern.matcher(geometryName);
            int id = Integer.valueOf(matcher.group(1));

            result = extractDemand(demandPath, result, geometryName, id);
            result = extractSolarSupply(solarPath, result, geometryName, id);
        }

        return result;
    }

    /**
     * Extracts timestamps of CEA outputs and return them in a list
     * @param filePath path to CSV file containing the timestamps
     * @return time stamps as a list of string
     */
    public static List<String> extractTimes(String filePath) throws IOException {
        List<String> result = new ArrayList<>();

        CSVParser parser = CSVParser.parse(new File(filePath), StandardCharsets.UTF_8, CSVFormat.DEFAULT);

        for (CSVRecord record : parser) {
           result.add(record.get("Date").replaceAll("\\s", "T"));
        }

        return result;
    }

    /**
     * Extracts the names of the geometries that CEA was run on and return them in a list
     * @param filePath path to CSV file containing the names of the geometries
     * @return name of the geometries as a list of string
     */
    public static List<String> getGeometryNames(String filePath) throws IOException {
        List<String> result = new ArrayList<>();
        CSVParser parser = CSVParser.parse(new File(filePath), StandardCharsets.UTF_8, CSVFormat.DEFAULT);
        for (CSVRecord record : parser) {
            result.add(record.get("Name"));
        }
        return result;
    }

    /**
     * Extracts CEA outputs on energy demands and return them as CEAOutputData
     * @param directoryPath path to directory containing all the CSV files on energy demand outputs
     * @param ceaOutputData CEAOutputData containing information on timestamps and building IRIs
     * @param geometryName geometry name as used in CEA simulations
     * @param id corresponding id to building IRI
     * @return CEAOutputData with added information on building energy demands
     */
    public static CEAOutputData extractDemand(String directoryPath, CEAOutputData ceaOutputData, String geometryName, Integer id) throws IOException {
        int iriSize = ceaOutputData.iris.size();
        int timeSize = ceaOutputData.times.size();

        List<Double> zeroList = Collections.nCopies(timeSize, 0.0);

        List<List<Double>> gridList = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> electricityList = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> heatList = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> coolList = Collections.nCopies(iriSize, zeroList);

        String fileName = geometryName + ".csv";

        CSVParser parser = CSVParser.parse(new File(directoryPath + FS + fileName), StandardCharsets.UTF_8, CSVFormat.DEFAULT);
        int i = 0;
        for (CSVRecord record : parser) {
            gridList.get(id).set(i, gridList.get(id).get(i) + Double.valueOf(record.get(GRID)));
            electricityList.get(id).set(i, electricityList.get(id).get(i) + Double.valueOf(record.get(ELECTRICITY)));
            heatList.get(id).set(i, heatList.get(id).get(i) + Double.valueOf(record.get(HEATING)));
            coolList.get(id).set(i, coolList.get(id).get(i) + Double.valueOf(record.get(COOLING)));
            i++;
        }
        
        ceaOutputData.GridConsumption = nestedDoubleToString(gridList);
        ceaOutputData.ElectricityConsumption = nestedDoubleToString(electricityList);
        ceaOutputData.HeatingConsumption = nestedDoubleToString(heatList);
        ceaOutputData.CoolingConsumption = nestedDoubleToString(coolList);

        return ceaOutputData;
    }

    /**
     * Extracts CEA outputs on solar potentials and return them as CEAOutputData
     * @param directoryPath path to directory containing all the CSV files on solar potential outputs
     * @param ceaOutputData CEAOutputData containing information on timestamps and building IRIs
     * @param geometryName geometry name as used in CEA simulations
     * @param id corresponding id to building IRI
     * @return CEAOutputData with added information on solar potentials
     */
    public static CEAOutputData extractSolarSupply(String directoryPath, CEAOutputData ceaOutputData, String geometryName, Integer id) throws IOException {
        int iriSize = ceaOutputData.iris.size();
        int timeSize = ceaOutputData.times.size();

        List<Double> zeroList = Collections.nCopies(timeSize, 0.0);

        List<List<Double>> pvRoof = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvNorth = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvSouth = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvWest = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvEast = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtPlateERoof = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtPlateENorth = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtPlateESouth = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtPlateEWest = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtPlateEEast = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtPlateQRoof = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtPlateQNorth = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtPlateQSouth = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtPlateQWest = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtPlateQEast = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtTubeERoof = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtTubeENorth = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtTubeESouth = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtTubeEWest = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtTubeEEast = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtTubeQRoof = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtTubeQNorth = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtTubeQSouth = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtTubeQWest = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> pvtTubeQEast = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> scPlateRoof = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> scPlateNorth = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> scPlateSouth = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> scPlateWest = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> scPlateEast = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> scTubeRoof = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> scTubeNorth = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> scTubeSouth = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> scTubeWest = Collections.nCopies(iriSize, zeroList);
        List<List<Double>> scTubeEast = Collections.nCopies(iriSize, zeroList);
        

        for (Map.Entry<String, List<String>> entry : SOLAR_SUPPLY.entrySet()) {
            String generator = entry.getKey();
            if (generator.contains("PVT")) {generator = "PVT";}
            String generatorType = entry.getKey();
            List<String> supplies = entry.getValue();
            String fileName = geometryName + "_" + generatorType + ".csv";

            CSVParser parser = CSVParser.parse(new File(directoryPath + FS + fileName), StandardCharsets.UTF_8, CSVFormat.DEFAULT);
            int i = 0;
            for (CSVRecord record : parser) {
                for (String supply : supplies) {
                    String roofColumn = generator + ROOF_SUPPLY.replace("supply", supply);
                    String northColumn = generator + NORTH_SUPPLY.replace("supply", supply);
                    String southColumn = generator + SOUTH_SUPPLY.replace("supply", supply);
                    String westColumn = generator + WEST_SUPPLY.replace("supply", supply);
                    String eastColumn = generator + EAST_SUPPLY.replace("supply", supply);

                    Double roof = Double.valueOf(record.get(roofColumn));
                    Double north = Double.valueOf(record.get(northColumn));
                    Double south = Double.valueOf(record.get(southColumn));
                    Double west = Double.valueOf(record.get(westColumn));
                    Double east = Double.valueOf(record.get(eastColumn));

                    // pv results
                    if (generatorType.equals("PV")) {
                        pvRoof.get(id).set(i, pvRoof.get(id).get(i) + roof);
                        pvNorth.get(id).set(i, pvNorth.get(id).get(i) + north);
                        pvSouth.get(id).set(i, pvSouth.get(id).get(i) + south);
                        pvWest.get(id).set(i, pvWest.get(id).get(i) + west);
                        pvEast.get(id).set(i, pvEast.get(id).get(i) + east);
                    }
                    // plate solar collector results
                    else if (generatorType.equals("SC_FP")){
                        scPlateRoof.get(id).set(i, scPlateRoof.get(id).get(i) + roof);
                        scPlateNorth.get(id).set(i, scPlateNorth.get(id).get(i) + north);
                        scPlateSouth.get(id).set(i, scPlateSouth.get(id).get(i) + south);
                        scPlateWest.get(id).set(i, scPlateWest.get(id).get(i) + west);
                        scPlateEast.get(id).set(i, scPlateEast.get(id).get(i) + east);
                    }
                    // tube solar collector results
                    else if (generatorType.equals("SC_ET")){
                        scTubeRoof.get(id).set(i, scTubeRoof.get(id).get(i) + roof);
                        scTubeNorth.get(id).set(i, scTubeNorth.get(id).get(i) + north);
                        scTubeSouth.get(id).set(i, scTubeSouth.get(id).get(i) + south);
                        scTubeWest.get(id).set(i, scTubeWest.get(id).get(i) + west);
                        scTubeEast.get(id).set(i, scTubeEast.get(id).get(i) + east);
                    }
                    // plate pvt results
                    else if (generatorType.equals("PVT_FP")){
                        // electricity results
                        if (supply.equals("E")) {
                            pvtPlateERoof.get(id).set(i, pvtPlateERoof.get(id).get(i) + roof);
                            pvtPlateENorth.get(id).set(i, pvtPlateENorth.get(id).get(i) + north);
                            pvtPlateESouth.get(id).set(i, pvtPlateESouth.get(id).get(i) + south);
                            pvtPlateEWest.get(id).set(i, pvtPlateEWest.get(id).get(i) + west);
                            pvtPlateEEast.get(id).set(i, pvtPlateEEast.get(id).get(i) + east);
                        }
                        // heat results
                        else {
                            pvtPlateQRoof.get(id).set(i, pvtPlateQRoof.get(id).get(i) + roof);
                            pvtPlateQNorth.get(id).set(i, pvtPlateQNorth.get(id).get(i) + north);
                            pvtPlateQSouth.get(id).set(i, pvtPlateQSouth.get(id).get(i) + south);
                            pvtPlateQWest.get(id).set(i, pvtPlateQWest.get(id).get(i) + west);
                            pvtPlateQEast.get(id).set(i, pvtPlateQEast.get(id).get(i) + east);
                        }
                    }
                    // tube pvt results
                    else if (generatorType.equals("PVT_ET")) {
                        // electricity results
                        if (supply.equals("E")) {
                            pvtTubeERoof.get(id).set(i, pvtTubeERoof.get(id).get(i) + roof);
                            pvtTubeENorth.get(id).set(i, pvtTubeENorth.get(id).get(i) + north);
                            pvtTubeESouth.get(id).set(i, pvtTubeESouth.get(id).get(i) + south);
                            pvtTubeEWest.get(id).set(i, pvtTubeEWest.get(id).get(i) + west);
                            pvtTubeEEast.get(id).set(i, pvtTubeEEast.get(id).get(i) + east);
                        }
                        // heat results
                        else {
                            pvtTubeQRoof.get(id).set(i, pvtTubeQRoof.get(id).get(i) + roof);
                            pvtTubeQNorth.get(id).set(i, pvtTubeQNorth.get(id).get(i) + north);
                            pvtTubeQSouth.get(id).set(i, pvtTubeQSouth.get(id).get(i) + south);
                            pvtTubeQWest.get(id).set(i, pvtTubeQWest.get(id).get(i) + west);
                            pvtTubeQEast.get(id).set(i, pvtTubeQEast.get(id).get(i) + east);
                        }
                    }
                }
                i++;
            }
        }
        
        ceaOutputData.PVRoofSupply = nestedDoubleToString(pvRoof);
        ceaOutputData.PVWallNorthSupply = nestedDoubleToString(pvNorth);
        ceaOutputData.PVWallSouthSupply = nestedDoubleToString(pvSouth);
        ceaOutputData.PVWallWestSupply = nestedDoubleToString(pvWest);
        ceaOutputData.PVWallEastSupply = nestedDoubleToString(pvEast);

        ceaOutputData.ThermalPlateRoofSupply = nestedDoubleToString(scPlateRoof);
        ceaOutputData.ThermalPlateWallNorthSupply = nestedDoubleToString(scPlateNorth);
        ceaOutputData.ThermalPlateWallSouthSupply = nestedDoubleToString(scPlateSouth);
        ceaOutputData.ThermalPlateWallWestSupply = nestedDoubleToString(scPlateWest);
        ceaOutputData.ThermalPlateWallEastSupply = nestedDoubleToString(scPlateEast);
        ceaOutputData.ThermalTubeRoofSupply = nestedDoubleToString(scTubeRoof);
        ceaOutputData.ThermalTubeWallNorthSupply = nestedDoubleToString(scTubeNorth);
        ceaOutputData.ThermalTubeWallSouthSupply = nestedDoubleToString(scTubeSouth);
        ceaOutputData.ThermalTubeWallWestSupply = nestedDoubleToString(scTubeWest);
        ceaOutputData.ThermalTubeWallEastSupply = nestedDoubleToString(scTubeEast);

        ceaOutputData.PVTPlateRoofESupply = nestedDoubleToString(pvtPlateERoof);
        ceaOutputData.PVTPlateWallNorthESupply = nestedDoubleToString(pvtPlateENorth);
        ceaOutputData.PVTPlateWallSouthESupply = nestedDoubleToString(pvtPlateESouth);
        ceaOutputData.PVTPlateWallWestESupply = nestedDoubleToString(pvtPlateEWest);
        ceaOutputData.PVTPlateWallEastESupply = nestedDoubleToString(pvtPlateEEast);
        ceaOutputData.PVTPlateRoofQSupply = nestedDoubleToString(pvtPlateQRoof);
        ceaOutputData.PVTPlateWallNorthQSupply = nestedDoubleToString(pvtPlateQNorth);
        ceaOutputData.PVTPlateWallSouthQSupply = nestedDoubleToString(pvtPlateQSouth);
        ceaOutputData.PVTPlateWallWestQSupply = nestedDoubleToString(pvtPlateQWest);
        ceaOutputData.PVTPlateWallEastQSupply = nestedDoubleToString(pvtPlateQEast);

        ceaOutputData.PVTTubeRoofESupply = nestedDoubleToString(pvtTubeERoof);
        ceaOutputData.PVTTubeWallNorthESupply = nestedDoubleToString(pvtTubeENorth);
        ceaOutputData.PVTTubeWallSouthESupply = nestedDoubleToString(pvtTubeESouth);
        ceaOutputData.PVTTubeWallWestESupply = nestedDoubleToString(pvtTubeEWest);
        ceaOutputData.PVTTubeWallEastESupply = nestedDoubleToString(pvtTubeEEast);
        ceaOutputData.PVTTubeRoofQSupply = nestedDoubleToString(pvtTubeQRoof);
        ceaOutputData.PVTTubeWallNorthQSupply = nestedDoubleToString(pvtTubeQNorth);
        ceaOutputData.PVTTubeWallSouthQSupply = nestedDoubleToString(pvtTubeQSouth);
        ceaOutputData.PVTTubeWallWestQSupply = nestedDoubleToString(pvtTubeQWest);
        ceaOutputData.PVTTubeWallEastQSupply = nestedDoubleToString(pvtTubeQEast);

        return ceaOutputData;
    }

    /**
     * Extracts CEA outputs on solar generator areas and return them as CEAOutputData
     * @param filePath path to CSV file containing the areas of solar generators
     * @param ceaOutputData CEAOutputData containing information on building IRIs
     * @return CEAOutputData with added information on solar generator areas
     */
    public static CEAOutputData extractArea(String filePath, CEAOutputData ceaOutputData) throws IOException {
        int size = ceaOutputData.iris.size();

        List<Double> roofResult = new ArrayList<>(Collections.nCopies(size, 0.0));
        List<Double> northResult = new ArrayList<>(Collections.nCopies(size, 0.0));
        List<Double> southResult = new ArrayList<>(Collections.nCopies(size, 0.0));
        List<Double> westResult = new ArrayList<>(Collections.nCopies(size, 0.0));
        List<Double> eastResult = new ArrayList<>(Collections.nCopies(size, 0.0));

        CSVParser parser = CSVParser.parse(new File(filePath), StandardCharsets.UTF_8, CSVFormat.DEFAULT);

        for (CSVRecord record : parser) {
            int id = Integer.parseInt(record.get("Name").split("_")[1]);
            roofResult.set(id, roofResult.get(id) + Double.valueOf(record.get(ROOF_AREA)));
            northResult.set(id, roofResult.get(id) + Double.valueOf(record.get(NORTH_AREA)));
            southResult.set(id, roofResult.get(id) + Double.valueOf(record.get(SOUTH_AREA)));
            westResult.set(id, roofResult.get(id) + Double.valueOf(record.get(WEST_AREA)));
            eastResult.set(id, roofResult.get(id) + Double.valueOf(record.get(EAST_AREA)));
        }

        ceaOutputData.RoofSolarSuitableArea = doubleToString(roofResult);
        ceaOutputData.NorthWallSolarSuitableArea = doubleToString(northResult);
        ceaOutputData.SouthWallSolarSuitableArea = doubleToString(southResult);
        ceaOutputData.WestWallSolarSuitableArea = doubleToString(westResult);
        ceaOutputData.EastWallSolarSuitableArea = doubleToString(eastResult);

        return ceaOutputData;
    }

    /**
     * Convert double values in a list of list to strings
     * @param data list of list of doubles
     * @return data as list of list of strings
     */
    private static List<List<String>> nestedDoubleToString(List<List<Double>> data) {
        return data.stream()
                .map(innerList -> doubleToString(innerList))
                .collect(Collectors.toList());
    }

    /**
     * Convert double values in a list to strings
     * @param data list of doubles
     * @return data as list of strings
     */
    private static List<String> doubleToString(List<Double> data) {
        return data.stream()
                .map(value -> String.valueOf(value))
                .collect(Collectors.toList());
    }

}
