package uk.ac.cam.cares.jps.agent.cea.tasks;

import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.agent.cea.data.CEAMetaData;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.agent.cea.data.CEABuildingData;
import uk.ac.cam.cares.jps.agent.cea.data.CEAOutputData;

import kong.unirest.HttpResponse;
import kong.unirest.Unirest;
import kong.unirest.UnirestException;
import org.apache.http.HttpException;
import org.apache.http.protocol.HTTP;
import com.google.gson.Gson;
import org.json.JSONObject;

import org.locationtech.jts.geom.Geometry;
import java.io.*;
import java.net.HttpURLConnection;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.OffsetDateTime;
import java.util.*;

public class RunCEATask implements Runnable {
    private final ArrayList<CEABuildingData> inputs;
    private final ArrayList<String> uris;
    private final int threadNumber;
    private final String crs;
    private final String database;
    private final CEAMetaData metaData;
    private final String solarProperties;
    private final String endpointUri;
    private Double weatherLat;
    private Double weatherLon;
    private Double weatherElevation;
    private Double weatherOffset;
    public static final String CTYPE_JSON = "application/json";
    private boolean stop = false;
    private boolean noSurroundings = true;
    private boolean noWeather = true;
    private boolean noTerrain = true;
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
    private static final String WORKFLOW_YML_WEATHER = "workflow_reference_weather.yml"; // YML file name for historical
                                                                                         // weather workflow
    private static final String WORKFLOW_YML_MAIN = "workflow1_main.yml"; // YML file name for main workflow
    private static final String WORKFLOW_YML_PVT = "workflow2_pvt_tube.yml"; // YML file name for PVT workflow
    private static final String CREATE_WORKFLOW_SCRIPT = "create_cea_workflow.py";
    private static final String FS = System.getProperty("file.separator");
    private static final String PROJECT_NAME = "testProject";
    private static final String SCENARIO_NAME = "testScenario";
    private static final String CEA_OUTPUT_DATA_DIRECTORY = PROJECT_NAME + FS + SCENARIO_NAME + FS + "outputs" + FS
            + "data";

    private CEAOutputUpdater updater;
    private static final Gson GSON_INSTANCE = new Gson();

    public RunCEATask(ArrayList<CEABuildingData> buildingData, CEAMetaData ceaMetaData, ArrayList<String> uris,
            int thread, String crs, String ceaDatabase, CEAOutputUpdater updater, JSONObject solar, String endpointUri) {
        this.inputs = buildingData;
        this.uris = uris;
        this.threadNumber = thread;
        this.crs = crs;
        this.metaData = ceaMetaData;
        this.database = ceaDatabase;
        this.updater = updater;
        this.solarProperties = solar == null ? "null" : "\'" + solar.toString() + "\'";
        this.endpointUri = endpointUri;
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
                System.out.println((char) ch);
            br.close();
            int exitVal = p.waitFor();
            System.out.println("Process exitValue: " + exitVal);
            return p;

        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Recursively deletes contents of a directory
     * 
     * @param file given directory
     */
    public void deleteDirectoryContents(File file) {
        for (File subFile : file.listFiles()) {
            // if it is a subfolder recursively call function to empty it
            if (subFile.isDirectory()) {
                deleteDirectoryContents(subFile);
            }
            subFile.delete();
        }
    }

    public void cleanUp(String path) {
        File file = new File(path);
        deleteDirectoryContents(file);
        file.delete();
    }

    /**
     * Returns output data to CEA Agent via http POST request
     * 
     * @param output output data
     */
    public void returnOutputs(CEAOutputData output) {
        try {
            String jsonOutput = GSON_INSTANCE.toJson(output);
            if (!jsonOutput.isEmpty()) {
                HttpResponse<?> response = Unirest.post(endpointUri)
                        .header(HTTP.CONTENT_TYPE, CTYPE_JSON)
                        .body(jsonOutput)
                        .socketTimeout(300000)
                        .asEmpty();
                int responseStatus = response.getStatus();
                if (responseStatus != HttpURLConnection.HTTP_OK) {
                    throw new HttpException(endpointUri + " " + responseStatus);
                }
            }

        } catch (HttpException | UnirestException e) {
            throw new JPSRuntimeException(e);
        }
    }

    /**

    /**
     * Writes building geometry and usage data to text file to be read by the Python
     * scripts
     * 
     * @param dataInputs    ArrayList of the CEA input data
     * @param directoryPath directory path
     * @param filePath      path to store data file
     */
    private void parseBuildingData(ArrayList<CEABuildingData> dataInputs, String directoryPath, String filePath) {
        // parse input data to JSON
        String dataString = "[";

        for (int i = 0; i < dataInputs.size(); i++) {
            Map<String, Object> tempMap = new HashMap<>();
            tempMap.put("geometry", geometriesToStrings(dataInputs.get(i).getGeometry().getFootprint()));
            tempMap.put("height", dataInputs.get(i).getGeometry().getHeight());
            tempMap.put("usage", dataInputs.get(i).getUsage());
            tempMap.put("id", i);
            dataString += new Gson().toJson(tempMap);
            if (i != dataInputs.size() - 1) {
                dataString += ", ";
            }
        }
        dataString += "]";

        File dir = new File(directoryPath);

        if (!dir.exists() && !dir.mkdirs()) {
            throw new JPSRuntimeException(new FileNotFoundException(directoryPath));
        }

        // write building geometry data to filePath
        try {
            BufferedWriter fWriter = new BufferedWriter(new FileWriter(filePath));
            fWriter.write(dataString);
            fWriter.close();
        } catch (IOException e) {
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Parses geometry objects to WKT strings and return the strings as a list
     * 
     * @param geometries list of geometry objects
     * @return list of WKT strings of the geometry objects in geometries
     */
    private List<String> geometriesToStrings(List<Geometry> geometries) {
        List<String> result = new ArrayList<>();

        for (Geometry geometry : geometries) {
            result.add(geometry.toString());
        }

        return result;
    }

    /**
     * Writes surrounding data into text file to be read by create_shapefile.py for
     * shapefile creation
     * 
     * @param surroundings  list of CEAGeometryData of the surrounding buildings
     * @param directoryPath directory path
     * @param filePath      path to store data file
     */
    private void parseSurroundings(List<CEAGeometryData> surroundings, String directoryPath, String filePath) {
        // Parse input data to JSON
        String dataString = "[";

        if (!surroundings.isEmpty()) {
            noSurroundings = false;

            for (int i = 0; i < surroundings.size(); i++) {
                Map<String, Object> tempMap = new HashMap<>();
                tempMap.put("geometry", geometriesToStrings(surroundings.get(i).getFootprint()));
                tempMap.put("height", surroundings.get(i).getHeight());
                dataString += new Gson().toJson(tempMap);
                if (i != surroundings.size() - 1) {
                    dataString += ", ";
                }
            }

            dataString += "]";

            File dir = new File(directoryPath);

            if (!dir.exists() && !dir.mkdirs()) {
                throw new JPSRuntimeException(new FileNotFoundException(directoryPath));
            }

            try {
                BufferedWriter fWriter = new BufferedWriter(new FileWriter(filePath));
                fWriter.write(dataString);
                fWriter.close();
            } catch (IOException e) {
                throw new JPSRuntimeException(e);
            }
        }
    }

    /**
     * Writes weather data into text file to be read by create_weatherfile.py for
     * EPW file creation
     * 
     * @param weatherTimes     timestamps of weather data
     * @param weather          weather data
     * @param weatherMetaData  weather meta data
     * @param weatherTimesPath path to store
     * @param weatherPath      path to store weather data files
     */
    private void parseWeather(List<OffsetDateTime> weatherTimes, Map<String, List<Double>> weather,
            List<Double> weatherMetaData, String weatherTimesPath, String weatherPath) {
        if (weatherTimes != null) {
            List<Map<String, Integer>> timeMap = new ArrayList<>();

            noWeather = false;

            for (int i = 0; i < weatherTimes.size(); i++) {
                OffsetDateTime offsetDateTime = weatherTimes.get(i);
                Map<String, Integer> map = new HashMap<>();
                map.put("year", offsetDateTime.getYear());
                map.put("month", offsetDateTime.getMonthValue());
                map.put("day", offsetDateTime.getDayOfMonth());
                map.put("hour", offsetDateTime.getHour());
                map.put("minute", offsetDateTime.getMinute());
                timeMap.add(map);
            }

            String times = new Gson().toJson(timeMap);
            String weatherData = new Gson().toJson(weather);
            weatherLat = weatherMetaData.get(0);
            weatherLon = weatherMetaData.get(1);
            weatherElevation = weatherMetaData.get(2);
            weatherOffset = weatherMetaData.get(3);

            // write timestamps of weather data to weatherTimesPath
            try {
                BufferedWriter fWriter = new BufferedWriter(new FileWriter(weatherTimesPath));
                fWriter.write(times);
                fWriter.close();
            } catch (IOException e) {
                throw new JPSRuntimeException(e);
            }

            // write weather data to weatherPath
            try {
                BufferedWriter fWriter = new BufferedWriter(new FileWriter(weatherPath));
                fWriter.write(weatherData);
                fWriter.close();
            } catch (IOException e) {
                throw new JPSRuntimeException(e);
            }
        }
    }

    /**
     * Writes bytes of raster data to TIF
     * 
     * @param rasterData byte array of raster data
     * @param path       path of the TIF file to write to
     */
    private void bytesToTIFF(byte[] rasterData, String path) {
        if (rasterData != null) {
            noTerrain = false;
            try {
                Path filePath = Path.of(path);
                Files.write(filePath, rasterData);
            } catch (IOException e) {
                e.printStackTrace();
                throw new JPSRuntimeException(e);
            }
        }
    }

    /**
     * Renames CEA outputs files on PVT since CEA uses the same file names for both
     * plate PVT and tube PVT
     * 
     * @param solarDir directory that stores CEA output files on solar energy
     *                 generators
     * @param pvtType  the type of PVT used, FP for plate, and ET for tube
     */
    protected void renamePVT(String solarDir, String pvtType) {
        File dir = new File(solarDir);

        for (final File f : dir.listFiles()) {
            if (f.getAbsolutePath().contains("PVT") && !f.getAbsolutePath().contains("FP")
                    && !f.getAbsolutePath().contains("ET")) {
                File newFile = new File(f.getAbsolutePath().replace("PVT", "PVT_" + pvtType));
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
                } else {
                    strTmp += FS + "thread_" + threadNumber;
                }

                String workflowPathWeather = strTmp + FS + WORKFLOW_YML_WEATHER;
                String workflowPathMain = strTmp + FS + WORKFLOW_YML_MAIN;
                String workflowPathPVT = strTmp + FS + WORKFLOW_YML_PVT;
                String dataPath = strTmp + FS + DATA_FILE;
                String surroundingsPath = strTmp + FS + SURROUNDINGS_FILE;
                String weatherTimesPath = strTmp + FS + WEATHERTIMES_FILE;
                String weatherDataPath = strTmp + FS + WEATHERDATA_FILE;
                String tempTerrainPath = strTmp + FS + TEMPTERRAIN_FILE;

                // write building geometry and usage data to temporary text file
                parseBuildingData(this.inputs, strTmp, dataPath);
                // write surrounding buildings' geometry data to temporary text file
                parseSurroundings(this.metaData.getSurrounding(), strTmp, surroundingsPath);
                // write weather data to temporary text file
                parseWeather(this.metaData.getWeatherTimes(), this.metaData.getWeather(),
                        this.metaData.getWeatherMetaData(), weatherTimesPath, weatherDataPath);
                // convert terrain bytes data to TIFF
                bytesToTIFF(this.metaData.getTerrain(), tempTerrainPath);

                String surroundingsFlag = noSurroundings ? "1" : "0";
                String weatherFlag = noWeather ? "1" : "0";
                String terrainFlag = noTerrain ? "1" : "0";

                // argument preparation
                ArrayList<String> argsExec = new ArrayList<>();
                String cmdPrefix = "";
                String cmdPython = "";
                String cmdCEA = cmdPrefix + "cea workflow --workflow ";
                String defaultEPW = strTmp + FS + PROJECT_NAME + FS + SCENARIO_NAME + FS + "inputs" + FS + "weather"
                        + FS + "weather.epw";
                String os = System.getProperty("os.name").toLowerCase();
                String shapefileScript = getResourcePath(SHAPEFILE_SCRIPT, os);
                String typologyScript = getResourcePath(TYPOLOGY_SCRIPT, os);
                String terrainScript = getResourcePath(TERRAIN_SCRIPT, os);
                String weatherScript = getResourcePath(WEATHER_SCRIPT, os);
                String createWorkflowScript = getResourcePath(CREATE_WORKFLOW_SCRIPT, os);
                String refWorkflowWeather = getResourcePath(WORKFLOW_YML_WEATHER, os);
                String refWorkflowMain = getResourcePath(WORKFLOW_YML_MAIN, os);
                String refWorkflowPVT = getResourcePath(WORKFLOW_YML_PVT, os);

                if (os.contains("win")) {
                    argsExec.addAll(List.of("cmd.exe", "/C"));
                    cmdPrefix = "conda activate cea && ";
                    cmdPython = cmdPrefix + "python ";
                } else {
                    argsExec.addAll(List.of("/bin/bash", "-c"));
                    cmdPrefix = "export PROJ_LIB=/opt/conda/share/proj && export PATH=/opt/conda/bin:$PATH && ";
                    cmdPython = cmdPrefix + "/opt/conda/bin/python3 ";
                }

                ArrayList<String> argsCreateZone = getArgument(argsExec,
                        cmdPython + shapefileScript + " " + dataPath + " " + strTmp + " " + crs + " zone.shp");
                ArrayList<String> argsCreateSurrounding = getArgument(argsExec, cmdPython + shapefileScript + " "
                        + surroundingsPath + " " + strTmp + " " + crs + " surroundings.shp");
                ArrayList<String> argsCreateTypology = getArgument(argsExec,
                        cmdPython + typologyScript + " " + dataPath + " " + strTmp);
                ArrayList<String> argsCreateTerrain = getArgument(argsExec,
                        cmdPython + terrainScript + " " + tempTerrainPath + " " + strTmp + " " + TERRAIN_FILE);
                ArrayList<String> argsCreateDefaultWeatherWorkflow = getArgument(argsExec,
                        cmdPython + createWorkflowScript + " " + refWorkflowWeather + " " + WORKFLOW_YML_WEATHER + " "
                                + strTmp + " " + "null" + " " + "null" + " " + "null" + " " + database + " null");
                ArrayList<String> argsRunDefaultWeatherWorkflow = getArgument(argsExec, cmdCEA + workflowPathWeather);
                ArrayList<String> argsCreateWeather = getArgument(argsExec,
                        cmdPython + weatherScript + " " + weatherTimesPath + " " + weatherDataPath + " " + weatherLat
                                + " " + weatherLon + " " + weatherElevation + " " + weatherOffset + " " + strTmp + " "
                                + "weather.epw" + " " + defaultEPW);
                ArrayList<String> argsCreateMainWorkflow = getArgument(argsExec,
                        cmdPython + createWorkflowScript + " " + refWorkflowMain + " " + WORKFLOW_YML_MAIN + " "
                                + strTmp + " " + surroundingsFlag + " " + weatherFlag + " " + terrainFlag + " "
                                + database + " " + solarProperties);
                ArrayList<String> argsRunMainWorkflow = getArgument(argsExec, cmdCEA + workflowPathMain);
                ArrayList<String> argsCreatePVTWorkflow = getArgument(argsExec,
                        cmdPython + createWorkflowScript + " " + refWorkflowPVT + " " + WORKFLOW_YML_PVT + " " + strTmp
                                + " " + "null" + " " + "null" + " " + "null" + " " + database + " " + solarProperties);
                ArrayList<String> argsRunPVTWorkflow = getArgument(argsExec, cmdCEA + workflowPathPVT);

                try {
                    // create the shapefile process and run
                    runProcess(argsCreateZone);
                    // if there are surrounding data, create the shapefile process for surroundings
                    // and run
                    if (!noSurroundings) {
                        runProcess(argsCreateSurrounding);
                    }
                    // create the typology file process and run
                    runProcess(argsCreateTypology);
                    // create python script and run to create validate terrain file
                    // (ST_Clip can clip inside pixel, which results in nodata value at the border
                    // of the result; the python script trims out the nodata value due to ST_Clip)
                    if (!noTerrain) {
                        runProcess(argsCreateTerrain);
                    }

                    // if there are weather data retrieved, create EPW file from the retrieved
                    // weather data
                    if (!noWeather) {
                        // create the workflow process to get CEA default weather
                        runProcess(argsCreateDefaultWeatherWorkflow);
                        // run workflow_reference_weather.yml for CEA to get default weather file
                        runProcess(argsRunDefaultWeatherWorkflow);

                        // create the weather file process and run
                        runProcess(argsCreateWeather);

                        // delete the temporary CEA files that were used to create weather file
                        cleanUp(strTmp + FS + PROJECT_NAME);
                    }

                    // create the workflow process and run
                    runProcess(argsCreateMainWorkflow);

                    // CEA output file names for PVT plate collectors and PVT tube collectors are
                    // the same, so one PVT collector type has to be run first then the output files
                    // renamed before running the other PVT collector type

                    // run workflow that runs all CEA scripts with PVT plate collectors
                    runProcess(argsRunMainWorkflow);

                    String pathOutputCEA = strTmp + FS + CEA_OUTPUT_DATA_DIRECTORY;
                    String pathOutputPVT = pathOutputCEA + FS + "potentials" + FS + "solar";

                    // rename PVT output files to PVT plate
                    renamePVT(pathOutputPVT, "FP");

                    // create workflow process for PVT tube collectors
                    runProcess(argsCreatePVTWorkflow);
                    // run CEA for PVT tube collectors
                    runProcess(argsRunPVTWorkflow);

                    // rename PVT output files to PVT tube
                    renamePVT(pathOutputPVT, "ET");

                    CEAOutputData result = CEAOutputHandler.extractCEAOutputs(pathOutputCEA, this.uris);
                    if (this.endpointUri==null) {
                        updater.updateCEA(result);
                    } else {
                        returnOutputs(result);
                    };
                    cleanUp(strTmp);
                } catch (NullPointerException | IOException e) {
                    cleanUp(strTmp);
                    e.printStackTrace();
                    throw new JPSRuntimeException("There are no CEA outputs, CEA encountered an error");
                }
            } catch (NullPointerException | URISyntaxException e) {
                e.printStackTrace();
                throw new JPSRuntimeException(e);
            } finally {
                stop();
            }
        }
    }

    private String getResourcePath(String filename, String operatingSystem) throws URISyntaxException {
        if (operatingSystem.contains("win")) {
            return new File(Objects.requireNonNull(getClass().getClassLoader().getResource(filename)).toURI())
                    .getAbsolutePath();
        } else {
            return FS + "target" + FS + "classes" + FS + filename;
        }
    }

    private ArrayList<String> getArgument(ArrayList<String> argsExec, String customArgument) {
        ArrayList<String> args = new ArrayList<>(argsExec);
        args.add(customArgument);
        return args;
    }

}
