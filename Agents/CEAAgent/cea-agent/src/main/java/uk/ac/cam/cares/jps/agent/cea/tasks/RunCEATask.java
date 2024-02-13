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

import org.locationtech.jts.geom.Geometry;
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
    private final ArrayList<CEABuildingData> inputs;
    private final ArrayList<String> uris;
    private final URI endpointUri;
    private final int threadNumber;
    private final String crs;
    private final CEAMetaData metaData;
    private Double weather_lat;
    private Double weather_lon;
    private Double weather_elevation;
    private Double weather_offset;
    public static final String CTYPE_JSON = "application/json";
    private Boolean stop = false;
    private Boolean noSurroundings = true;
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
    private static final String PROJECT_NAME = "testProject";
    private static final String SCENARIO_NAME = "testScenario";
    private static final String CEA_OUTPUT_DATA_DIRECTORY = PROJECT_NAME + FS + SCENARIO_NAME + FS + "outputs" + FS + "data";

    public RunCEATask(ArrayList<CEABuildingData> buildingData, CEAMetaData ceaMetaData, URI endpointUri, ArrayList<String> uris, int thread, String crs) {
        this.inputs = buildingData;
        this.endpointUri = endpointUri;
        this.uris = uris;
        this.threadNumber = thread;
        this.crs = crs;
        this.metaData = ceaMetaData;
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
    public void deleteDirectoryContents(File file) {
        for (File subFile : file.listFiles()) {
            // if it is a subfolder recursively call function to empty it
            if (subFile.isDirectory()) {
                deleteDirectoryContents(subFile);
            }
            subFile.delete();
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
     * Writes building geometry and usage data to text file to be read by the Python scripts
     * @param dataInputs ArrayList of the CEA input data
     * @param directory_path directory path
     * @param file_path path to store data file
     */
    private void parseBuildingData(ArrayList<CEABuildingData> dataInputs, String directory_path, String file_path) {
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
    }

    /**
     * Parses geometry objects to WKT strings and return the strings as a list
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
     * Writes surrounding data into text file to be read by create_shapefile.py for shapefile creation
     * @param surroundings list of CEAGeometryData of the surrounding buildings
     * @param directory_path directory path
     * @param file_path path to store data file
     */
    private void parseSurroundings(List<CEAGeometryData> surroundings, String directory_path, String file_path) {
        //Parse input data to JSON
        String dataString = "[";

        if (surroundings != null) {
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
    }

    /**
     * Writes weather data into text file to be read by create_weatherfile.py for EPW file creation
     * @param weatherTimes timestamps of weather data
     * @param weather weather data
     * @param weatherMetaData weather meta data
     * @param weatherTimes_path path to store
     * @param weather_path path to store weather data files
     */
    private void parseWeather(List<OffsetDateTime> weatherTimes, Map<String, List<Double>> weather, List<Double> weatherMetaData, String weatherTimes_path, String weather_path) {
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
            weather_lat = weatherMetaData.get(0);
            weather_lon = weatherMetaData.get(1);
            weather_elevation = weatherMetaData.get(2);
            weather_offset = weatherMetaData.get(3);

            // write timestamps of weather data to weatherTimes_path
            try {
                BufferedWriter f_writer = new BufferedWriter(new FileWriter(weatherTimes_path));
                f_writer.write(times);
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
    }

    /**
     * Writes bytes of raster data to TIF
     * @param rasterData byte array of raster data
     * @param path path of the TIF file to write to
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

                // write building geometry and usage data to temporary text file
                parseBuildingData(this.inputs, strTmp, data_path);
                // write surrounding buildings' geometry data to temporary text file
                parseSurroundings(this.metaData.getSurrounding(), strTmp, surroundings_path);
                // write weather data to temporary text file
                parseWeather(this.metaData.getWeatherTimes(), this.metaData.getWeather(), this.metaData.getWeatherMetaData(), weatherTimes_path, weatherData_path);
                // convert terrain bytes data to TIFF
                bytesToTIFF(this.metaData.getTerrain(), tempTerrain_path);

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
                        String defaultEPW_path = strTmp + FS + PROJECT_NAME + FS + SCENARIO_NAME + FS + "inputs" + FS + "weather" + FS + "weather.epw";
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
                    String defaultEPW = strTmp + FS + PROJECT_NAME + FS + SCENARIO_NAME + FS + "inputs" + FS + "weather" + FS + "weather.epw";
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
                    // run workflow_reference_weather.yml for CEA to get default weather file
                    runProcess(args5);

                    // create the weather file process and run
                    runProcess(args6);

                    // delete the temporary CEA files that were used to create weather file
                    File file = new File(strTmp + FS + PROJECT_NAME);
                    deleteDirectoryContents(file);
                    file.delete();
                }

                // create the workflow process and run
                runProcess(args7);

                // CEA output file names for PVT plate collectors and PVT tube collectors are the same, so one PVT collector type has to be run first then the output files renamed before running the other PVT collector type
                // run workflow that runs all CEA scripts with PVT plate collectors
                runProcess(args8);

                // rename PVT output files to PVT plate
                renamePVT(strTmp + FS + CEA_OUTPUT_DATA_DIRECTORY + FS + "potentials" + FS + "solar", "FP");

                // create workflow process for PVT tube collectors
                runProcess(args9);
                // run CEA for PVT tube collectors
                runProcess(args10);

                // rename PVT output files to PVT tube
                renamePVT(strTmp + FS + CEA_OUTPUT_DATA_DIRECTORY + FS + "potentials" + FS + "solar", "ET");

                try {
                    CEAOutputData result = CEAOutputHandler.extractCEAOutputs(strTmp + FS + CEA_OUTPUT_DATA_DIRECTORY, this.uris);
                    System.out.println("CEA ran successfully.");
                    returnOutputs(result);
                }
                catch (IOException e) {
                    File file = new File(strTmp);
                    deleteDirectoryContents(file);
                    file.delete();
                    e.printStackTrace();
                    throw new JPSRuntimeException("There are no CEA outputs, CEA encountered an error");
                }
            }
            catch ( NullPointerException | URISyntaxException e) {
                e.printStackTrace();
                throw new JPSRuntimeException(e);
            }
            finally {
                stop();
            }
        }
    }
}
