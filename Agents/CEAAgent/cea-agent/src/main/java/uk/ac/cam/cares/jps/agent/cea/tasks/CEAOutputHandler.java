package uk.ac.cam.cares.jps.agent.cea.tasks;

import uk.ac.cam.cares.jps.agent.cea.data.CEAOutputData;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class CEAOutputHandler {
    private static final String FS = System.getProperty("file.separator");
    private static final String PATTERN = "^B\\d+_(\\d+)";
    private static final CSVFormat csvFormat = CSVFormat.DEFAULT.builder()
            .setDelimiter(",")
            .setHeader()
            .setSkipHeaderRecord(true)
            .build();
    
    private static final Map<String, List<String>> SOLAR_SUPPLY;

    private static final String ROOF_AREA = "PV_roofs_top_m2";
    private static final String NORTH_AREA = "PV_walls_north_m2";
    private static final String SOUTH_AREA = "PV_walls_south_m2";
    private static final String WEST_AREA = "PV_walls_west_m2";
    private static final String EAST_AREA = "PV_walls_east_m2";

    private static final String GRID = "GRID_kWh";
    private static final String ELECTRICITY = "E_sys_kWh";
    private static final String HEATING = "QH_sys_kWh";
    private static final String COOLING = "QC_sys_kWh";

    private static final String ROOF_SUPPLY = "roofs_top_supply_kWh";
    private static final String NORTH_SUPPLY = "walls_north_supply_kWh";
    private static final String SOUTH_SUPPLY = "walls_south_supply_kWh";
    private static final String WEST_SUPPLY = "walls_west_supply_kWh";
    private static final String EAST_SUPPLY = "walls_east_supply_kWh";


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

        String demandPath = directoryPath + FS + "demand";
        String solarPath = directoryPath + FS + "potentials" + FS + "solar";

        result.iris = iris;

        List<String> geometryNames = getGeometryNames(solarPath + FS + "PV_total_buildings.csv");

        extractArea(solarPath + FS + "PV_total_buildings.csv", result);
        result.times = extractTimes(solarPath + FS + "PV_total.csv");
        
        initialiseCEAOutputTS(result);

        for (String geometryName : geometryNames) {
            Pattern pattern = Pattern.compile(PATTERN);
            Matcher matcher = pattern.matcher(geometryName);
            matcher.matches();
            int id = Integer.parseInt(matcher.group(1));

            extractDemand(demandPath, result, geometryName, id);
            extractSolarSupply(solarPath, result, geometryName, id);
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

        try (CSVParser parser = CSVParser.parse(new File(filePath), StandardCharsets.UTF_8, csvFormat)) {
            for (CSVRecord record : parser) {
                result.add(record.get("Date").replaceAll("\\s", "T"));
            }
            return result;
        }
    }

    /**
     * Extracts the names of the geometries that CEA was run on and return them in a list
     * @param filePath path to CSV file containing the names of the geometries
     * @return name of the geometries as a list of string
     */
    public static List<String> getGeometryNames(String filePath) throws IOException {
        List<String> result = new ArrayList<>();
        try (CSVParser parser = CSVParser.parse(new File(filePath), StandardCharsets.UTF_8, csvFormat)) {
            for (CSVRecord record : parser) {
                result.add(record.get("Name"));
            }

            return result;
        }
    }

    /**
     * Extracts CEA outputs on energy demands and return them as CEAOutputData
     * @param directoryPath path to directory containing all the CSV files on energy demand outputs
     * @param ceaOutputData CEAOutputData containing information on timestamps and building IRIs
     * @param geometryName geometry name as used in CEA simulations
     * @param id corresponding id to building IRI
     */
    public static void extractDemand(String directoryPath, CEAOutputData ceaOutputData, String geometryName, Integer id) throws IOException {
        List<List<Double>> gridList = ceaOutputData.GridConsumption;
        List<List<Double>> electricityList = ceaOutputData.ElectricityConsumption;
        List<List<Double>> heatList = ceaOutputData.HeatingConsumption;
        List<List<Double>> coolList = ceaOutputData.CoolingConsumption;

        String fileName = geometryName + ".csv";

        try (CSVParser parser = CSVParser.parse(new File(directoryPath + FS + fileName), StandardCharsets.UTF_8, csvFormat)) {
            int i = 0;

            for (CSVRecord record : parser) {
                gridList.get(id).set(i, gridList.get(id).get(i) + Double.parseDouble(record.get(GRID)));
                electricityList.get(id).set(i, electricityList.get(id).get(i) + Double.parseDouble(record.get(ELECTRICITY)));
                heatList.get(id).set(i, heatList.get(id).get(i) + Double.parseDouble(record.get(HEATING)));
                coolList.get(id).set(i, coolList.get(id).get(i) + Double.parseDouble(record.get(COOLING)));
                i++;
            }
        }
    }

    /**
     * Extracts CEA outputs on solar potentials and return them as CEAOutputData
     * @param directoryPath path to directory containing all the CSV files on solar potential outputs
     * @param ceaOutputData CEAOutputData containing information on timestamps and building IRIs
     * @param geometryName geometry name as used in CEA simulations
     * @param id corresponding id to building IRI
     */
    public static void extractSolarSupply(String directoryPath, CEAOutputData ceaOutputData, String geometryName, Integer id) throws IOException {
        List<List<Double>> pvRoof = ceaOutputData.PVRoofSupply;
        List<List<Double>> pvNorth = ceaOutputData.PVWallNorthSupply;
        List<List<Double>> pvSouth = ceaOutputData.PVWallSouthSupply;
        List<List<Double>> pvWest = ceaOutputData.PVWallWestSupply;
        List<List<Double>> pvEast = ceaOutputData.PVWallEastSupply;
        List<List<Double>> pvtPlateERoof = ceaOutputData.PVTPlateRoofESupply;
        List<List<Double>> pvtPlateENorth = ceaOutputData.PVTPlateWallNorthESupply;
        List<List<Double>> pvtPlateESouth = ceaOutputData.PVTPlateWallSouthESupply;
        List<List<Double>> pvtPlateEWest = ceaOutputData.PVTPlateWallWestESupply;
        List<List<Double>> pvtPlateEEast = ceaOutputData.PVTPlateWallEastESupply;
        List<List<Double>> pvtPlateQRoof = ceaOutputData.PVTPlateRoofQSupply;
        List<List<Double>> pvtPlateQNorth = ceaOutputData.PVTPlateWallNorthQSupply;
        List<List<Double>> pvtPlateQSouth = ceaOutputData.PVTPlateWallSouthQSupply;
        List<List<Double>> pvtPlateQWest = ceaOutputData.PVTPlateWallWestQSupply;
        List<List<Double>> pvtPlateQEast = ceaOutputData.PVTPlateWallEastQSupply;
        List<List<Double>> pvtTubeERoof = ceaOutputData.PVTTubeRoofESupply;
        List<List<Double>> pvtTubeENorth = ceaOutputData.PVTTubeWallNorthESupply;
        List<List<Double>> pvtTubeESouth = ceaOutputData.PVTTubeWallSouthESupply;
        List<List<Double>> pvtTubeEWest = ceaOutputData.PVTTubeWallWestESupply;
        List<List<Double>> pvtTubeEEast = ceaOutputData.PVTTubeWallEastESupply;
        List<List<Double>> pvtTubeQRoof = ceaOutputData.PVTTubeRoofQSupply;
        List<List<Double>> pvtTubeQNorth = ceaOutputData.PVTTubeWallNorthQSupply;
        List<List<Double>> pvtTubeQSouth = ceaOutputData.PVTTubeWallSouthQSupply;
        List<List<Double>> pvtTubeQWest = ceaOutputData.PVTTubeWallWestQSupply;
        List<List<Double>> pvtTubeQEast = ceaOutputData.PVTTubeWallEastQSupply;
        List<List<Double>> scPlateRoof = ceaOutputData.ThermalPlateRoofSupply;
        List<List<Double>> scPlateNorth = ceaOutputData.ThermalPlateWallNorthSupply;
        List<List<Double>> scPlateSouth = ceaOutputData.ThermalPlateWallSouthSupply;
        List<List<Double>> scPlateWest = ceaOutputData.ThermalPlateWallWestSupply;
        List<List<Double>> scPlateEast = ceaOutputData.ThermalPlateWallEastSupply;
        List<List<Double>> scTubeRoof = ceaOutputData.ThermalTubeRoofSupply;
        List<List<Double>> scTubeNorth = ceaOutputData.ThermalTubeWallNorthSupply;
        List<List<Double>> scTubeSouth = ceaOutputData.ThermalTubeWallSouthSupply;
        List<List<Double>> scTubeWest = ceaOutputData.ThermalTubeWallWestSupply;
        List<List<Double>> scTubeEast = ceaOutputData.ThermalTubeWallEastSupply;


        for (Map.Entry<String, List<String>> entry : SOLAR_SUPPLY.entrySet()) {
            String generator = entry.getKey();
            if (generator.contains("PVT")) {
                generator = "PVT";
            }
            String generatorType = entry.getKey();
            List<String> supplies = entry.getValue();
            String fileName = geometryName + "_" + generatorType + ".csv";

            try (CSVParser parser = CSVParser.parse(new File(directoryPath + FS + fileName), StandardCharsets.UTF_8, csvFormat)) {
                int i = 0;
                for (CSVRecord record : parser) {
                    for (String supply : supplies) {
                        String roofColumn = generator + "_" + ROOF_SUPPLY.replace("supply", supply);
                        String northColumn = generator + "_" + NORTH_SUPPLY.replace("supply", supply);
                        String southColumn = generator + "_" + SOUTH_SUPPLY.replace("supply", supply);
                        String westColumn = generator + "_" + WEST_SUPPLY.replace("supply", supply);
                        String eastColumn = generator + "_" + EAST_SUPPLY.replace("supply", supply);

                        Double roof = Double.parseDouble(record.get(roofColumn));
                        Double north = Double.parseDouble(record.get(northColumn));
                        Double south = Double.parseDouble(record.get(southColumn));
                        Double west = Double.parseDouble(record.get(westColumn));
                        Double east = Double.parseDouble(record.get(eastColumn));

                        // pv results
                        if (generatorType.equals("PV")) {
                            pvRoof.get(id).set(i, pvRoof.get(id).get(i) + roof);
                            pvNorth.get(id).set(i, pvNorth.get(id).get(i) + north);
                            pvSouth.get(id).set(i, pvSouth.get(id).get(i) + south);
                            pvWest.get(id).set(i, pvWest.get(id).get(i) + west);
                            pvEast.get(id).set(i, pvEast.get(id).get(i) + east);
                        }
                        // plate solar collector results
                        else if (generatorType.equals("SC_FP")) {
                            scPlateRoof.get(id).set(i, scPlateRoof.get(id).get(i) + roof);
                            scPlateNorth.get(id).set(i, scPlateNorth.get(id).get(i) + north);
                            scPlateSouth.get(id).set(i, scPlateSouth.get(id).get(i) + south);
                            scPlateWest.get(id).set(i, scPlateWest.get(id).get(i) + west);
                            scPlateEast.get(id).set(i, scPlateEast.get(id).get(i) + east);
                        }
                        // tube solar collector results
                        else if (generatorType.equals("SC_ET")) {
                            scTubeRoof.get(id).set(i, scTubeRoof.get(id).get(i) + roof);
                            scTubeNorth.get(id).set(i, scTubeNorth.get(id).get(i) + north);
                            scTubeSouth.get(id).set(i, scTubeSouth.get(id).get(i) + south);
                            scTubeWest.get(id).set(i, scTubeWest.get(id).get(i) + west);
                            scTubeEast.get(id).set(i, scTubeEast.get(id).get(i) + east);
                        }
                        // plate pvt results
                        else if (generatorType.equals("PVT_FP")) {
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
        }
    }

    /**
     * Extracts CEA outputs on solar generator areas and return them as CEAOutputData
     * @param filePath path to CSV file containing the areas of solar generators
     * @param ceaOutputData CEAOutputData containing information on building IRIs
     */
    public static void extractArea(String filePath, CEAOutputData ceaOutputData) throws IOException {
        int size = ceaOutputData.iris.size();

        List<Double> roofResult = new ArrayList<>(Collections.nCopies(size, 0.0));
        List<Double> northResult = new ArrayList<>(Collections.nCopies(size, 0.0));
        List<Double> southResult = new ArrayList<>(Collections.nCopies(size, 0.0));
        List<Double> westResult = new ArrayList<>(Collections.nCopies(size, 0.0));
        List<Double> eastResult = new ArrayList<>(Collections.nCopies(size, 0.0));

        try (CSVParser parser = CSVParser.parse(new File(filePath), StandardCharsets.UTF_8, csvFormat)) {
            for (CSVRecord record : parser) {
                Pattern pattern = Pattern.compile(PATTERN);
                Matcher matcher = pattern.matcher(record.get("Name"));
                matcher.matches();
                int id = Integer.parseInt(matcher.group(1));

                roofResult.set(id, roofResult.get(id) + Double.parseDouble(record.get(ROOF_AREA)));
                northResult.set(id, northResult.get(id) + Double.parseDouble(record.get(NORTH_AREA)));
                southResult.set(id, southResult.get(id) + Double.parseDouble(record.get(SOUTH_AREA)));
                westResult.set(id, westResult.get(id) + Double.parseDouble(record.get(WEST_AREA)));
                eastResult.set(id, eastResult.get(id) + Double.parseDouble(record.get(EAST_AREA)));
            }

            ceaOutputData.RoofSolarSuitableArea = roofResult;
            ceaOutputData.NorthWallSolarSuitableArea = northResult;
            ceaOutputData.SouthWallSolarSuitableArea = southResult;
            ceaOutputData.WestWallSolarSuitableArea = westResult;
            ceaOutputData.EastWallSolarSuitableArea = eastResult;
        }
    }

    /**
     * Initialise CEAOutputData as list of list of zeros
     * @param ceaOutputData CEAOutputData for initialisation
     */
    private static void initialiseCEAOutputTS(CEAOutputData ceaOutputData) {
        int iriSize = ceaOutputData.iris.size();
        int timeSize = ceaOutputData.times.size();
        
        ceaOutputData.GridConsumption = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.ElectricityConsumption = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.HeatingConsumption = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.CoolingConsumption = initialiseZeroList(iriSize, timeSize);
        
        ceaOutputData.PVRoofSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVWallSouthSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVWallNorthSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVWallEastSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVWallWestSupply = initialiseZeroList(iriSize, timeSize);

        ceaOutputData.PVTPlateRoofESupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTPlateWallSouthESupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTPlateWallNorthESupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTPlateWallEastESupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTPlateWallWestESupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTPlateRoofQSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTPlateWallSouthQSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTPlateWallNorthQSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTPlateWallEastQSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTPlateWallWestQSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTTubeRoofESupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTTubeWallSouthESupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTTubeWallNorthESupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTTubeWallEastESupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTTubeWallWestESupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTTubeRoofQSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTTubeWallSouthQSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTTubeWallNorthQSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTTubeWallEastQSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.PVTTubeWallWestQSupply = initialiseZeroList(iriSize, timeSize);

        ceaOutputData.ThermalPlateRoofSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.ThermalPlateWallSouthSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.ThermalPlateWallNorthSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.ThermalPlateWallEastSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.ThermalPlateWallWestSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.ThermalTubeRoofSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.ThermalTubeWallSouthSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.ThermalTubeWallNorthSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.ThermalTubeWallEastSupply = initialiseZeroList(iriSize, timeSize);
        ceaOutputData.ThermalTubeWallWestSupply = initialiseZeroList(iriSize, timeSize);
    }

    /**
     * Creates and returns a list of list of zeros
     * @param outerSize size of outer list
     * @param innerSize size of inner list
     * @return a list of list of zeros
     */
    private static List<List<Double>> initialiseZeroList(Integer outerSize, Integer innerSize) {
        return IntStream.range(0, outerSize)
                .mapToObj(i -> new ArrayList<>(Collections.nCopies(innerSize, 0.0)))
                .collect(Collectors.toList());
    }

}
