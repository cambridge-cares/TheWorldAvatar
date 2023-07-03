package uk.ac.cam.cares.jps.agent.ceatasks;

import kong.unirest.HttpResponse;
import kong.unirest.Unirest;
import kong.unirest.UnirestException;
import org.apache.http.HttpException;
import org.apache.http.protocol.HTTP;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import com.google.gson.Gson;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.OffsetDateTime;
import java.util.*;
import java.lang.Process;

public class RunCEATask implements Runnable {
    private final ArrayList<CEAInputData> inputs;
    private final ArrayList<String> uris;
    private final URI endpointUri;
    private final int threadNumber;
    private final String crs;
    private final byte[] terrain;
    private Double weather_lat;
    private Double weather_lon;
    private Double weather_elevation;
    private Double weather_offset;
    public static final String CTYPE_JSON = "application/json";
    private Boolean stop = false;
    private Boolean noSurroundings = false;
    private Boolean noWeather = true;
    private Boolean noTerrain = true;
    private static final String DATA_FILE = "datafile.txt";
    private static final String SURROUNDINGS_FILE = "surroundingdata.txt";
    private static final String WEATHERTIMES_FILE = "weathertimes.txt";
    private static final String WEATHERDATA_FILE = "weatherdata.txt";
    private static final String TEMPTERRAIN_FILE = "tempterrain.tif";
    private static final String TERRAIN_FILE = "terrain.tif";
    private static final String SHAPEFILE_SCRIPT = "create_shapefile.py";
    private static final String TYPOLOGY_SCRIPT = "create_typologyfile.py";
    private static final String TERRAIN_SCRIPT = "validate_tif_file.py";
    private static final String WEATHER_SCRIPT = "create_weatherfile.py";
    private static final String WORKFLOW_YML = "workflow_reference_weather.yml";
    private static final String WORKFLOW_YML1 = "workflow1_main.yml";
    private static final String WORKFLOW_YML2 = "workflow2_pvt_tube.yml";
    private static final String CREATE_WORKFLOW_SCRIPT = "create_cea_workflow.py";
    private static final String FS = System.getProperty("file.separator");
    private Map<String, ArrayList<String>> solarSupply = new HashMap<>();

    public RunCEATask(ArrayList<CEAInputData> buildingData, URI endpointUri, ArrayList<String> uris, int thread, String crs, byte[] terrain) {
        this.inputs = buildingData;
        this.endpointUri = endpointUri;
        this.uris = uris;
        this.threadNumber = thread;
        this.crs = crs;
        this.terrain = terrain;
        setSolarSupply();
    }

    public void stop() {
        stop = true;
    }

    public Process runProcess(ArrayList<String> args) {
        ProcessBuilder builder = new ProcessBuilder(args);
        builder.redirectOutput(ProcessBuilder.Redirect.INHERIT);
        builder.redirectErrorStream(true);


        // starting the process
        try {
            Process p = builder.start();
            BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()));
            int ch;
            while ((ch = br.read()) != -1)
                System.out.println((char)ch);
            br.close();
            int exitVal = p.waitFor();
            System.out.println("Process exitValue: " + exitVal);
            return p;

        } catch ( IOException | InterruptedException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Recursively deletes contents of a directory
     * @param file given directory
     */
    public void deleteDirectoryContents(File file)
    {
        for (File subFile : file.listFiles()) {
            // if it is a subfolder recursively call function to empty it
            if (subFile.isDirectory()) {
                deleteDirectoryContents(subFile);
            }
            subFile.delete();
        }
    }

    /**
     * Extracts areas from excel files and add to output data, then delete excel files
     * @param tmpDir temporary directory path
     * @param result output data
     * @return output data
     */
    public CEAOutputData extractArea(String tmpDir, CEAOutputData result) {
        String line = "";
        String splitBy = ",";
        String solarDir = tmpDir + FS + "testProject" + FS + "testScenario" + FS + "outputs" + FS + "data" + FS + "potentials" + FS + "solar" + FS;
        boolean flag = false;

        try{
            for (String generatorType : solarSupply.keySet()) {
                String generator = generatorType;

                if (generatorType.contains("PVT")) {generator = "PVT";}
                
                //parsing a CSV file into BufferedReader class constructor
                FileReader solar = new FileReader(solarDir + generatorType + "_total_buildings.csv");
                BufferedReader solarFile = new BufferedReader(solar);
                ArrayList<String[]> solarColumns = new ArrayList<>();

                while ((line = solarFile.readLine()) != null)   //returns a Boolean value
                {
                    String[] rows = line.split(splitBy);    // use comma as separator
                    solarColumns.add(rows);
                }

                for (int n = 0; n < solarColumns.get(0).length; n++) {
                    if (solarColumns.get(0)[n].equals(generator + "_roofs_top_m2")) {
                        for (int m = 1; m < solarColumns.size(); m++) {
                            result.RoofSolarSuitableArea.add(solarColumns.get(m)[n]);
                        }
                        flag = true;
                    } else if (solarColumns.get(0)[n].equals(generator + "_walls_south_m2")) {
                        for (int m = 1; m < solarColumns.size(); m++) {
                            result.SouthWallSolarSuitableArea.add(solarColumns.get(m)[n]);
                        }
                        flag = true;
                    } else if (solarColumns.get(0)[n].equals(generator + "_walls_north_m2")) {
                        for (int m = 1; m < solarColumns.size(); m++) {
                            result.NorthWallSolarSuitableArea.add(solarColumns.get(m)[n]);
                        }
                        flag = true;
                    } else if (solarColumns.get(0)[n].equals(generator + "_walls_east_m2")) {
                        for (int m = 1; m < solarColumns.size(); m++) {
                            result.EastWallSolarSuitableArea.add(solarColumns.get(m)[n]);
                        }
                        flag = true;
                    } else if (solarColumns.get(0)[n].equals(generator + "_walls_west_m2")) {
                        for (int m = 1; m < solarColumns.size(); m++) {
                            result.WestWallSolarSuitableArea.add(solarColumns.get(m)[n]);
                        }
                        flag = true;
                    }
                }

                solarFile.close();
                solar.close();

                if (flag) {break;}
            }
        } catch (IOException e) {
            File file = new File(tmpDir);
            deleteDirectoryContents(file);
            file.delete();
            e.printStackTrace();
            throw new JPSRuntimeException("There are no CEA outputs, CEA encountered an error");
        }

        result.targetUrl=endpointUri.toString();
        result.iris=uris;
        File file = new File(tmpDir);
        deleteDirectoryContents(file);
        file.delete();

        return result;
    }

    /**
     * Extracts time series data from excel files and add to output data
     * @param tmpDir temporary directory path
     * @return output data
     */
    public CEAOutputData extractTimeSeriesOutputs(String tmpDir) {
        String line = "";
        String splitBy = ",";
        String projectDir = tmpDir+FS+"testProject";
        CEAOutputData result = new CEAOutputData();
        boolean getTimes = true;

        try{
            for(int i=0; i<inputs.size(); i++){
                FileReader demand;
                if(i<10){
                    demand = new FileReader(projectDir+FS+"testScenario"+FS+"outputs"+FS+"data"+FS+"demand"+FS+"B00"+i+".csv");
                }
                else if(i<100){
                    demand = new FileReader(projectDir+FS+"testScenario"+FS+"outputs"+FS+"data"+FS+"demand"+FS+"B0"+i+".csv");
                }
                else{
                    demand = new FileReader(projectDir+FS+"testScenario"+FS+"outputs"+FS+"data"+FS+"demand"+FS+"B"+i+".csv");
                }
                BufferedReader demand_file = new BufferedReader(demand);
                ArrayList<String[]> demand_columns = new ArrayList<>();
                ArrayList<String> grid_results = new ArrayList<>();
                ArrayList<String> heating_results = new ArrayList<>();
                ArrayList<String> cooling_results = new ArrayList<>();
                ArrayList<String> electricity_results = new ArrayList<>();

                while ((line = demand_file.readLine()) != null)   //returns a Boolean value
                {
                    String[] rows = line.split(splitBy);    // use comma as separator
                    demand_columns.add(rows);
                }
                for(int n=0; n<demand_columns.get(0).length; n++) {
                    if (demand_columns.get(0)[n].equals("GRID_kWh")) {
                        for (int m = 1; m < demand_columns.size(); m++) {
                            grid_results.add(demand_columns.get(m)[n]);
                        }
                    } else if (demand_columns.get(0)[n].equals("QH_sys_kWh")) {
                        for (int m = 1; m < demand_columns.size(); m++) {
                            heating_results.add(demand_columns.get(m)[n]);
                        }
                    } else if (demand_columns.get(0)[n].equals("QC_sys_kWh")) {
                        for (int m = 1; m < demand_columns.size(); m++) {
                            cooling_results.add(demand_columns.get(m)[n]);
                        }
                    } else if (demand_columns.get(0)[n].equals("E_sys_kWh")) {
                        for (int m = 1; m < demand_columns.size(); m++) {
                            electricity_results.add(demand_columns.get(m)[n]);
                        }
                    }
                }
                demand_file.close();
                demand.close();

                String solar;

                if(i<10){
                    solar = projectDir+FS+"testScenario"+FS+"outputs"+FS+"data"+FS+"potentials"+FS+"solar"+FS+"B00"+i;
                }
                else if(i<100){
                    solar = projectDir+FS+"testScenario"+FS+"outputs"+FS+"data"+FS+"potentials"+FS+"solar"+FS+"B0"+i;
                }
                else{
                    solar = projectDir+FS+"testScenario"+FS+"outputs"+FS+"data"+FS+"potentials"+FS+"solar"+FS+"B"+i;
                }

                for (Map.Entry<String, ArrayList<String>> entry: solarSupply.entrySet()){
                    result = extractSolarSupply(result, entry.getKey(), entry.getValue(), splitBy, solar + "_" + entry.getKey().toString() + ".csv", tmpDir, getTimes);
                    getTimes = false;
                }

                result.GridConsumption.add(grid_results);
                result.ElectricityConsumption.add(electricity_results);
                result.HeatingConsumption.add(heating_results);
                result.CoolingConsumption.add(cooling_results);
            }
        } catch ( IOException e) {
            File file = new File(tmpDir);
            deleteDirectoryContents(file);
            file.delete();
            e.printStackTrace();
            throw new JPSRuntimeException("There are no CEA outputs, CEA encountered an error");
        }
        return result;
    }

    /**
     * Extracts potential energy data of solar energy generators
     * @param result CEAOutputData to store the CEA outputs
     * @param generatorType type of solar energy generator
     * @param supplyTypes types of potential energy that generatorType can generate
     * @param dataSeparator separator of the CEA output csv files
     * @param solarFile file name of the csv storing the output data for generatorType
     * @param tmpDir root directory of CEA files
     * @param getTimes whether to extract timestamps
     * @return CEAOutputData with the potential energy data of solar energy generators
     */
    public CEAOutputData extractSolarSupply(CEAOutputData result, String generatorType, List<String> supplyTypes, String dataSeparator, String solarFile, String tmpDir, Boolean getTimes) {
        String line;
        String supply;
        String generator = generatorType;
        List<String> timestamps = new ArrayList();

        if (generatorType.contains("PVT")) {generator = "PVT";}
        
        try {
            FileReader solar = new FileReader(solarFile);

            BufferedReader solar_file = new BufferedReader(solar);
            ArrayList<String[]> solar_columns = new ArrayList<>();

            while ((line = solar_file.readLine()) != null)   //returns a Boolean value
            {
                String[] rows = line.split(dataSeparator);    // use comma as separator
                solar_columns.add(rows);
            }

            for (int i = 0; i < supplyTypes.size(); i++) {
                ArrayList<String> roof_results = new ArrayList<>();
                ArrayList<String> wall_south_results = new ArrayList<>();
                ArrayList<String> wall_north_results = new ArrayList<>();
                ArrayList<String> wall_east_results = new ArrayList<>();
                ArrayList<String> wall_west_results = new ArrayList<>();

                supply = supplyTypes.get(i);

                for (int n = 0; n < solar_columns.get(0).length; n++) {
                    if (getTimes && solar_columns.get(0)[n].equals("Date")) {
                        for (int m = 1; m < solar_columns.size(); m++) {
                            timestamps.add(solar_columns.get(m)[n].replaceAll("\\s", "T"));
                        }
                    } else if (solar_columns.get(0)[n].equals(generator + "_roofs_top_" + supply + "_kWh")) {
                        for (int m = 1; m < solar_columns.size(); m++) {
                            roof_results.add(solar_columns.get(m)[n]);
                        }
                    } else if (solar_columns.get(0)[n].equals(generator + "_walls_south_" + supply + "_kWh")) {
                        for (int m = 1; m < solar_columns.size(); m++) {
                            wall_south_results.add(solar_columns.get(m)[n]);
                        }
                    } else if (solar_columns.get(0)[n].equals(generator + "_walls_north_" + supply + "_kWh")) {
                        for (int m = 1; m < solar_columns.size(); m++) {
                            wall_north_results.add(solar_columns.get(m)[n]);
                        }
                    } else if (solar_columns.get(0)[n].equals(generator + "_walls_west_" + supply + "_kWh")) {
                        for (int m = 1; m < solar_columns.size(); m++) {
                            wall_west_results.add(solar_columns.get(m)[n]);
                        }
                    } else if (solar_columns.get(0)[n].equals(generator + "_walls_east_" + supply + "_kWh")) {
                        for (int m = 1; m < solar_columns.size(); m++) {
                            wall_east_results.add(solar_columns.get(m)[n]);
                        }
                    }
                }

                result = addSolarSupply(result, generatorType, "roof", supply, roof_results);
                result = addSolarSupply(result, generatorType, "wall_north", supply, wall_north_results);
                result = addSolarSupply(result, generatorType, "wall_south", supply, wall_south_results);
                result = addSolarSupply(result, generatorType, "wall_west", supply, wall_west_results);
                result = addSolarSupply(result, generatorType, "wall_east", supply, wall_east_results);

                if (getTimes) {
                    result.times = timestamps;
                }
            }

            solar_file.close();
            solar.close();

            return result;
        }
        catch ( IOException e) {
            File file = new File(tmpDir);
            deleteDirectoryContents(file);
            file.delete();
            e.printStackTrace();
            throw new JPSRuntimeException("There are no CEA outputs, CEA encountered an error");
        }
    }

    /**
     * Returns output data to CEA Agent via http POST request
     * @param output output data
     */
    public void returnOutputs(CEAOutputData output) {
        try {
            String JSONOutput = new Gson().toJson(output);
            if (!JSONOutput.isEmpty()) {
                HttpResponse<?> response = Unirest.post(endpointUri.toString())
                        .header(HTTP.CONTENT_TYPE, CTYPE_JSON)
                        .body(JSONOutput)
                        .socketTimeout(300000)
                        .asEmpty();
                int responseStatus = response.getStatus();
                if (responseStatus != HttpURLConnection.HTTP_OK) {
                    throw new HttpException(endpointUri + " " + responseStatus);
                }
            }

        } catch ( HttpException | UnirestException e) {
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Converts input data (except for surrounding) to CEA into text file to be read by the Python scripts
     * @param dataInputs ArrayList of the CEA input data
     * @param directory_path directory path
     * @param file_path path to store data file, excluding surrounding data
     * @param surrounding_path path to store surrounding data file
     * @param weather_path path to store weather data file
     */
    private void dataToFile(ArrayList<CEAInputData> dataInputs, String directory_path, String file_path, String surrounding_path, String weatherTimes_path, String weather_path) {
        // parse input data to JSON
        String dataString = "[";
        String weatherTimes = "";
        String weatherData = "";
        ArrayList<CEAInputData> surroundings = new ArrayList<>();

        for (int i = 0; i < dataInputs.size(); i++) {
            if (!(dataInputs.get(i).getSurrounding() == null)) {surroundings.addAll(dataInputs.get(i).getSurrounding());}
            if (noWeather && !(dataInputs.get(i).getWeather() == null) && !(dataInputs.get(i).getWeatherTimes() == null)) {
                noWeather = false;
                List<OffsetDateTime> times = dataInputs.get(i).getWeatherTimes();
                List<Map<String, Integer>> timeMap = new ArrayList<>();

                for (int j = 0; j < times.size(); j++){
                    OffsetDateTime offsetDateTime = times.get(j);
                    Map<String, Integer> map = new HashMap<>();
                    map.put("year", offsetDateTime.getYear());
                    map.put("month", offsetDateTime.getMonthValue());
                    map.put("day", offsetDateTime.getDayOfMonth());
                    map.put("hour", offsetDateTime.getHour());
                    map.put("minute", offsetDateTime.getMinute());
                    timeMap.add(map);
                }

                weatherTimes += new Gson().toJson(timeMap);
                weatherData += new Gson().toJson(dataInputs.get(i).getWeather());
                weather_lat = dataInputs.get(i).getWeatherMetaData().get(0);
                weather_lon = dataInputs.get(i).getWeatherMetaData().get(1);
                weather_elevation = dataInputs.get(i).getWeatherMetaData().get(2);
                weather_offset = dataInputs.get(i).getWeatherMetaData().get(3);
            }
            Map<String, Object> tempMap = new HashMap<>();
            tempMap.put("geometry", dataInputs.get(i).getGeometry());
            tempMap.put("height", dataInputs.get(i).getHeight());
            tempMap.put("usage", dataInputs.get(i).getUsage());
            dataString += new Gson().toJson(tempMap);
            if(i!=dataInputs.size()-1) dataString += ", ";
        }
        dataString+="]";

        File dir = new File(directory_path);
        if (!dir.exists() && !dir.mkdirs()) {
            throw new JPSRuntimeException(new FileNotFoundException(directory_path));
        }

        // write building geometry data to file_path
        try {
            BufferedWriter f_writer = new BufferedWriter(new FileWriter(file_path));
            f_writer.write(dataString);
            f_writer.close();
        } catch (IOException e) {
            throw new JPSRuntimeException(e);
        }

        if (!noWeather) {
            // write timestamps of weather data to weatherTimes_path
            try {
                BufferedWriter f_writer = new BufferedWriter(new FileWriter(weatherTimes_path));
                f_writer.write(weatherTimes);
                f_writer.close();
            } catch (IOException e) {
                throw new JPSRuntimeException(e);
            }

            // write weather data to weather_path
            try {
                BufferedWriter f_writer = new BufferedWriter(new FileWriter(weather_path));
                f_writer.write(weatherData);
                f_writer.close();
            } catch (IOException e) {
                throw new JPSRuntimeException(e);
            }
        }

        // if there is surrounding data, call dataToFile to store surrounding data as a temporary text file
        if (surroundings.isEmpty()){
            noSurroundings = true;
        }
        else{
            dataToFile(surroundings, directory_path, surrounding_path);
        }
    }

    /**
     * Converts surrounding data into text file to be read by the Python scripts
     * @param dataInputs ArrayList of the CEA input data
     * @param directory_path directory path
     * @param file_path path to store data file, excluding surrounding data
     */
    private void dataToFile(ArrayList<CEAInputData> dataInputs, String directory_path, String file_path) {
        //Parse input data to JSON
        String dataString = "[";

        for (int i = 0; i < dataInputs.size(); i++) {
            Map<String, String> tempMap = new HashMap<>();
            tempMap.put("geometry", dataInputs.get(i).getGeometry());
            tempMap.put("height", dataInputs.get(i).getHeight());
            dataString += new Gson().toJson(tempMap);
            if(i!=dataInputs.size()-1) dataString += ", ";
        }
        dataString+="]";

        File dir = new File(directory_path);
        if (!dir.exists() && !dir.mkdirs()) {
            throw new JPSRuntimeException(new FileNotFoundException(directory_path));
        }

        try {
            BufferedWriter f_writer = new BufferedWriter(new FileWriter(file_path));
            f_writer.write(dataString);
            f_writer.close();
        } catch (IOException e) {
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Writes bytes of raster data to TIF
     * @param rasterData byte array of raster data
     * @param path path of the TIF file to write to
     */
    private void bytesToTIFF(byte[] rasterData, String path) {
        try{
            Path filePath = Path.of(path);
            Files.write(filePath, rasterData);
        }
        catch (IOException e)
        {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Add time series data on solar energy generator potential energy to CEAOutputData
     * @param result CEAOutputData to store the CEA outputs
     * @param generatorType type of solar energy generator
     * @param generatorLocation location of the solar energy generator
     * @param supplyType type of potential energy of data
     * @param data time series data of the potential energy for the solar energy generator
     * @return CEAOutputData with the specified potential energy added
     */
    public CEAOutputData addSolarSupply(CEAOutputData result, String generatorType, String generatorLocation, String supplyType, ArrayList<String> data) {
        if (generatorType.equals("PVT_FP")) {
            if (supplyType.equals("E")) {
                if (generatorLocation.contains("roof")) {
                    result.PVTPlateRoofESupply.add(data);
                } else if (generatorLocation.contains("north")) {
                    result.PVTPlateWallNorthESupply.add(data);
                } else if (generatorLocation.contains("south")) {
                    result.PVTPlateWallSouthESupply.add(data);
                } else if (generatorLocation.contains("west")) {
                    result.PVTPlateWallWestESupply.add(data);
                } else if (generatorLocation.contains("east")) {
                    result.PVTPlateWallEastESupply.add(data);
                }
            } else if (supplyType.equals("Q")) {
                if (generatorLocation.contains("roof")) {
                    result.PVTPlateRoofQSupply.add(data);
                } else if (generatorLocation.contains("north")) {
                    result.PVTPlateWallNorthQSupply.add(data);
                } else if (generatorLocation.contains("south")) {
                    result.PVTPlateWallSouthQSupply.add(data);
                } else if (generatorLocation.contains("west")) {
                    result.PVTPlateWallWestQSupply.add(data);
                } else if (generatorLocation.contains("east")) {
                    result.PVTPlateWallEastQSupply.add(data);
                }
            }
        } else if (generatorType.equals("PVT_ET")) {
            if (supplyType.equals("E")) {
                if (generatorLocation.contains("roof")) {
                    result.PVTTubeRoofESupply.add(data);
                } else if (generatorLocation.contains("north")) {
                    result.PVTTubeWallNorthESupply.add(data);
                } else if (generatorLocation.contains("south")) {
                    result.PVTTubeWallSouthESupply.add(data);
                } else if (generatorLocation.contains("west")) {
                    result.PVTTubeWallWestESupply.add(data);
                } else if (generatorLocation.contains("east")) {
                    result.PVTTubeWallEastESupply.add(data);
                }
            } else if (supplyType.equals("Q")) {
                if (generatorLocation.contains("roof")) {
                    result.PVTTubeRoofQSupply.add(data);
                } else if (generatorLocation.contains("north")) {
                    result.PVTTubeWallNorthQSupply.add(data);
                } else if (generatorLocation.contains("south")) {
                    result.PVTTubeWallSouthQSupply.add(data);
                } else if (generatorLocation.contains("west")) {
                    result.PVTTubeWallWestQSupply.add(data);
                } else if (generatorLocation.contains("east")) {
                    result.PVTTubeWallEastQSupply.add(data);
                }
            }
        } else if (generatorType.equals("PV")) {
            if (generatorLocation.contains("roof")) {
                result.PVRoofSupply.add(data);
            } else if (generatorLocation.contains("north")) {
                result.PVWallNorthSupply.add(data);
            } else if (generatorLocation.contains("south")) {
                result.PVWallSouthSupply.add(data);
            } else if (generatorLocation.contains("west")) {
                result.PVWallWestSupply.add(data);
            } else if (generatorLocation.contains("east")) {
                result.PVWallEastSupply.add(data);
            }
        } else if (generatorType.equals("SC_FP")) {
            if (generatorLocation.contains("roof")) {
                result.ThermalPlateRoofSupply.add(data);
            } else if (generatorLocation.contains("north")) {
                result.ThermalPlateWallNorthSupply.add(data);
            } else if (generatorLocation.contains("south")) {
                result.ThermalPlateWallSouthSupply.add(data);
            } else if (generatorLocation.contains("west")) {
                result.ThermalPlateWallWestSupply.add(data);
            } else if (generatorLocation.contains("east")) {
                result.ThermalPlateWallEastSupply.add(data);
            }
        } else if (generatorType.equals("SC_ET")) {
            if (generatorLocation.contains("roof")) {
                result.ThermalTubeRoofSupply.add(data);
            } else if (generatorLocation.contains("north")) {
                result.ThermalTubeWallNorthSupply.add(data);
            } else if (generatorLocation.contains("south")) {
                result.ThermalTubeWallSouthSupply.add(data);
            } else if (generatorLocation.contains("west")) {
                result.ThermalTubeWallWestSupply.add(data);
            } else if (generatorLocation.contains("east")) {
                result.ThermalTubeWallEastSupply.add(data);
            }
        }

        return result;
    }

    /**
     * Sets solarSupply with the keys being the type of solar energy generator, and the values being the type of energy that the generator can generate
     */
    private void setSolarSupply() {
        ArrayList<String> EQ = new ArrayList<>();
        ArrayList<String> E = new ArrayList<>();
        ArrayList<String> Q = new ArrayList<>();

        EQ.add("E");
        EQ.add("Q");
        E.add("E");
        Q.add("Q");

        solarSupply.put("PV", E);
        solarSupply.put("PVT_FP", EQ);
        solarSupply.put("PVT_ET", EQ);
        solarSupply.put("SC_FP", Q);
        solarSupply.put("SC_ET", Q);
    }

    /**
     * Renames CEA outputs files on PVT since CEA uses the same file names for both plate PVT and tube PVT
     * @param solarDir directory that stores CEA output files on solar energy generators
     * @param PVTType the type of PVT used, FP for plate, and ET for tube
     */
    protected void renamePVT(String solarDir, String PVTType) {
        File dir = new File(solarDir);

        for (final File f : dir.listFiles()) {
            if (f.getAbsolutePath().contains("PVT") && !f.getAbsolutePath().contains("FP") && !f.getAbsolutePath().contains("ET")) {
                File newFile = new File(f.getAbsolutePath().replace("PVT", "PVT_" + PVTType));
                f.renameTo(newFile);
            }
        }
    }

    @Override
    public void run() {
        while (!stop) {

            try {
                String strTmp = System.getProperty("java.io.tmpdir");
                if (strTmp.endsWith(FS)) {
                    strTmp += "thread_" + threadNumber;
                }
                else{
                    strTmp += FS + "thread_" + threadNumber;
                }

                String OS = System.getProperty("os.name").toLowerCase();
                ArrayList<String> args = new ArrayList<>();
                ArrayList<String> args1 = new ArrayList<>();
                ArrayList<String> args2 = new ArrayList<>();
                ArrayList<String> args3 = new ArrayList<>();
                ArrayList<String> args4 = new ArrayList<>();
                ArrayList<String> args5 = new ArrayList<>();
                ArrayList<String> args6 = new ArrayList<>();
                ArrayList<String> args7 = new ArrayList<>();
                ArrayList<String> args8 = new ArrayList<>();
                ArrayList<String> args9 = new ArrayList<>();
                ArrayList<String> args10 = new ArrayList<>();
                
                String workflowPath = strTmp + FS + WORKFLOW_YML;
                String workflowPath1 = strTmp + FS + WORKFLOW_YML1;
                String workflowPath2 = strTmp + FS + WORKFLOW_YML2;
                String data_path = strTmp + FS + DATA_FILE;
                String surroundings_path = strTmp + FS + SURROUNDINGS_FILE;
                String weatherTimes_path = strTmp + FS + WEATHERTIMES_FILE;
                String weatherData_path = strTmp + FS + WEATHERDATA_FILE;
                String tempTerrain_path = strTmp + FS + TEMPTERRAIN_FILE;

                dataToFile(this.inputs, strTmp, data_path, surroundings_path, weatherTimes_path, weatherData_path);

                if (this.terrain != null) {
                    bytesToTIFF(this.terrain, tempTerrain_path);
                    noTerrain = false;
                }

                String surroundingsFlag = noSurroundings ? "1" : "0";
                String weatherFlag = noWeather ? "1" : "0";
                String terrainFlag = noTerrain ? "1" : "0";

                if(OS.contains("win")){
                    String f_path;

                    args.add("cmd.exe");
                    args.add("/C");
                    f_path = new File(Objects.requireNonNull(getClass().getClassLoader().getResource(SHAPEFILE_SCRIPT)).toURI()).getAbsolutePath();
                    args.add("conda activate cea && python " + f_path + " " + data_path + " " + strTmp + " " + crs + " zone.shp");

                    args1.add("cmd.exe");
                    args1.add("/C");
                    f_path = new File(Objects.requireNonNull(getClass().getClassLoader().getResource(SHAPEFILE_SCRIPT)).toURI()).getAbsolutePath();
                    args1.add("conda activate cea && python " + f_path + " " + surroundings_path + " " + strTmp + " " + crs + " surroundings.shp");

                    args2.add("cmd.exe");
                    args2.add("/C");
                    f_path = new File(Objects.requireNonNull(getClass().getClassLoader().getResource(TYPOLOGY_SCRIPT)).toURI()).getAbsolutePath();
                    args2.add("conda activate cea && python " + f_path + " " + data_path + " " + strTmp);

                    args3.add("cmd.exe");
                    args3.add("/C");
                    f_path = new File(Objects.requireNonNull(getClass().getClassLoader().getResource(TERRAIN_SCRIPT)).toURI()).getAbsolutePath();
                    args3.add("conda activate cea && python " + f_path + " " + tempTerrain_path + " " + strTmp + " " + TERRAIN_FILE);

                    args4.add("cmd.exe");
                    args4.add("/C");
                    args4.add("conda activate cea && ");
                    args4.add("python");
                    args4.add(new File(
                            Objects.requireNonNull(getClass().getClassLoader().getResource(CREATE_WORKFLOW_SCRIPT)).toURI()).getAbsolutePath());
                    args4.add(new File(
                            Objects.requireNonNull(getClass().getClassLoader().getResource(WORKFLOW_YML)).toURI()).getAbsolutePath());
                    args4.add(WORKFLOW_YML);
                    args4.add(strTmp);
                    args4.add("null");
                    args4.add("null");
                    args4.add("null");

                    args5.add("cmd.exe");
                    args5.add("/C");
                    args5.add("conda activate cea && cea workflow --workflow " + workflowPath);

                    if (!noWeather) {
                        args6.add("cmd.exe");
                        args6.add("/C");
                        f_path = new File(Objects.requireNonNull(getClass().getClassLoader().getResource(WEATHER_SCRIPT)).toURI()).getAbsolutePath();
                        String defaultEPW_path = strTmp + FS + "testProject" + FS + "testScenario" + FS + "inputs" + FS + "weather" + FS + "weather.epw";
                        args6.add("conda activate cea && python " + f_path + " " + weatherTimes_path + " " + weatherData_path + " " + weather_lat + " " + weather_lon + " " + weather_elevation + " " + weather_offset  + " " + strTmp + " " + "weather.epw" + " " + defaultEPW_path);
                    }
                    
                    args7.add("cmd.exe");
                    args7.add("/C");
                    args7.add("conda activate cea && ");
                    args7.add("python");
                    args7.add(new File(
                            Objects.requireNonNull(getClass().getClassLoader().getResource(CREATE_WORKFLOW_SCRIPT)).toURI()).getAbsolutePath());
                    args7.add(new File(
                            Objects.requireNonNull(getClass().getClassLoader().getResource(WORKFLOW_YML1)).toURI()).getAbsolutePath());
                    args7.add(WORKFLOW_YML1);
                    args7.add(strTmp);
                    args7.add(surroundingsFlag);
                    args7.add(weatherFlag);
                    args7.add(terrainFlag);

                    args8.add("cmd.exe");
                    args8.add("/C");
                    args8.add("conda activate cea && cea workflow --workflow " + workflowPath1);

                    args9.add("cmd.exe");
                    args9.add("/C");
                    args9.add("conda activate cea && ");
                    args9.add("python");
                    args9.add(new File(
                            Objects.requireNonNull(getClass().getClassLoader().getResource(CREATE_WORKFLOW_SCRIPT)).toURI()).getAbsolutePath());
                    args9.add(new File(
                            Objects.requireNonNull(getClass().getClassLoader().getResource(WORKFLOW_YML2)).toURI()).getAbsolutePath());
                    args9.add(WORKFLOW_YML2);
                    args9.add(strTmp);
                    args9.add("null");
                    args9.add("null");
                    args9.add("null");

                    args10.add("cmd.exe");
                    args10.add("/C");
                    args10.add("conda activate cea && cea workflow --workflow " + workflowPath2);
                }
                else {
                    String shapefile = FS + "target" + FS + "classes" + FS + SHAPEFILE_SCRIPT;
                    String typologyfile = FS + "target" + FS + "classes" + FS + TYPOLOGY_SCRIPT;
                    String weatherfile = FS + "target" + FS + "classes" + FS + WEATHER_SCRIPT;
                    String terrainScript = FS + "target" + FS + "classes" + FS + TERRAIN_SCRIPT;
                    String defaultEPW = strTmp + FS + "testProject" + FS + "testScenario" + FS + "inputs" + FS + "weather" + FS + "weather.epw";
                    String createWorkflowFile = FS + "target" + FS + "classes" + FS + CREATE_WORKFLOW_SCRIPT;
                    String workflowFile = FS + "target" + FS + "classes" + FS +  WORKFLOW_YML;
                    String workflowFile1 = FS + "target" + FS + "classes" + FS + WORKFLOW_YML1;
                    String workflowFile2 = FS + "target" + FS + "classes" + FS + WORKFLOW_YML2;

                    args.add("/bin/bash");
                    args.add("-c");
                    args.add("export PROJ_LIB=/venv/share/lib && python " + shapefile + " " + data_path + " " +strTmp+ " " + crs + " zone.shp");

                    args1.add("/bin/bash");
                    args1.add("-c");
                    args1.add("export PROJ_LIB=/venv/share/lib && python " + shapefile + " " + surroundings_path + " " +strTmp+ " " + crs + " surroundings.shp");

                    args2.add("/bin/bash");
                    args2.add("-c");
                    args2.add("export PROJ_LIB=/venv/share/lib && python " + typologyfile + " " + data_path + " " + strTmp);

                    args3.add("/bin/bash");
                    args3.add("-c");
                    args3.add("export PROJ_LIB=/venv/share/lib && python " + terrainScript + " " + tempTerrain_path + " " + strTmp + " " + TERRAIN_FILE);

                    args4.add("/bin/bash");
                    args4.add("-c");
                    args4.add("export PROJ_LIB=/venv/share/lib && python " + createWorkflowFile + " " + workflowFile + " " + WORKFLOW_YML + " " + strTmp + " " + "null" + " " + "null" + " " + "null");

                    args5.add("/bin/bash");
                    args5.add("-c");
                    args5.add("export PATH=/venv/bin:/venv/cea/bin:/venv/Daysim:$PATH && source /venv/bin/activate && cea workflow --workflow " + workflowPath);

                    if (!noWeather) {
                        args6.add("/bin/bash");
                        args6.add("-c");
                        args6.add("export PROJ_LIB=/venv/share/lib && python " + weatherfile + " " + weatherTimes_path + " " + weatherData_path + " " + weather_lat + " " + weather_lon + " " + weather_elevation + " " + weather_offset  + " " + strTmp + " " + "weather.epw" + " " + defaultEPW);
                    }
                    
                    args7.add("/bin/bash");
                    args7.add("-c");
                    args7.add("export PROJ_LIB=/venv/share/lib && python " + createWorkflowFile + " " + workflowFile1 + " " + WORKFLOW_YML1 + " " + strTmp + " " + surroundingsFlag + " " + weatherFlag + " " + terrainFlag);

                    args8.add("/bin/bash");
                    args8.add("-c");
                    args8.add("export PATH=/venv/bin:/venv/cea/bin:/venv/Daysim:$PATH && source /venv/bin/activate && cea workflow --workflow " + workflowPath1);

                    args9.add("/bin/bash");
                    args9.add("-c");
                    args9.add("export PROJ_LIB=/venv/share/lib && python " + createWorkflowFile + " " + workflowFile2 + " " + WORKFLOW_YML2 + " " + strTmp + " " + "null" + " " + "null" + " " + "null");

                    args10.add("/bin/bash");
                    args10.add("-c");
                    args10.add("export PATH=/venv/bin:/venv/cea/bin:/venv/Daysim:$PATH && source /venv/bin/activate && cea workflow --workflow " + workflowPath2);
                }

                // create the shapefile process and run
                runProcess(args);
                // if there are surrounding data, create the shapefile process for surroundings and run
                if (!noSurroundings) {runProcess(args1);}
                // create the typology file process and run
                runProcess(args2);
                // create python script and run to create validate terrain file
                // (ST_Clip can clip inside pixel, which results in nodata value at the border of the result; the python script trims out the nodata value due to ST_Clip)
                if (!noTerrain) {runProcess(args3);}

                // if there are weather data retrieved, create EPW file from the retrieved weather data
                if (!noWeather) {
                    // create the workflow process to get CEA default weather
                    runProcess(args4);
                    // run workflow.yml for CEA to get default weather file
                    runProcess(args5);

                    // create the weather file process and run
                    runProcess(args6);

                    // delete the temporary CEA files that were used to create weather file
                    File file = new File(strTmp + FS + "testProject");
                    deleteDirectoryContents(file);
                    file.delete();
                }

                // create the workflow process and run
                runProcess(args7);

                // CEA output file names for PVT plate collectors and PVT tube collectors are the same, so one PVT collector type has to be run first then the output files renamed before running the other PVT collector type
                // run workflow that runs all CEA scripts with PVT plate collectors
                runProcess(args8);

                // rename PVT output files to PVT plate
                renamePVT(strTmp+FS+"testProject"+FS+"testScenario"+FS+"outputs"+FS+"data"+FS+"potentials"+FS+"solar", "FP");

                // create workflow process for PVT tube collectors
                runProcess(args9);
                // run CEA for PVT tube collectors
                runProcess(args10);

                // rename PVT output files to PVT tube
                renamePVT(strTmp+FS+"testProject"+FS+"testScenario"+FS+"outputs"+FS+"data"+FS+"potentials"+FS+"solar", "ET");

                CEAOutputData result = extractTimeSeriesOutputs(strTmp);
                returnOutputs(extractArea(strTmp,result));
            } catch ( NullPointerException | URISyntaxException e) {
                e.printStackTrace();
                throw new JPSRuntimeException(e);
            } finally {
                stop();
            }
        }
    }
}
