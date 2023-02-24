package com.cmclinnovations.aermod;

import java.io.BufferedWriter;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.io.IOUtils;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;

import com.cmclinnovations.aermod.objects.Ship;
import com.cmclinnovations.aermod.objects.WeatherData;

import uk.ac.cam.cares.jps.base.util.CRSTransformer;

public class Aermod {
    private static final Logger LOGGER = LogManager.getLogger(Aermod.class);
    private Path aermetDirectory;
    private Path aermodDirectory;
    private Path simulationDirectory;
    private static final String AERMET_INPUT = "aermet_input.inp";

    public Aermod(Path simulationDirectory) {
        this.simulationDirectory = simulationDirectory;
        LOGGER.info("Creating directory for simulation files");
        simulationDirectory.toFile().mkdirs();

        this.aermetDirectory = simulationDirectory.resolve("aermet");
        aermetDirectory.toFile().mkdir();

        this.aermodDirectory = simulationDirectory.resolve("aermod");
        aermodDirectory.toFile().mkdir();
    }

    int create144File(WeatherData weatherData) {
        String windSpeed = String.valueOf(weatherData.getWindSpeedInKnots());
        while (windSpeed.length() <2) {
            windSpeed = "0" + windSpeed;
        }
        if (windSpeed.length() != 2) {
            LOGGER.error("Invalid wind speed value {}", windSpeed);
            return 1;
        }

        String windDirection = String.valueOf(weatherData.getWindDirectionInTensOfDegrees());
        if (windDirection.length() != 2) {
            LOGGER.error("Invalid wind direction value {}", windDirection);
            return 1;
        }

        // unsure how strict the format is, just follow blindly at the moment
        String temperature = String.valueOf(weatherData.getTemperatureInFahrenheit());
        if (temperature.length() < 3) {
            temperature = "0" + temperature;
        }
        if (temperature.length() != 3) {
            LOGGER.error("Invalid temperature value {}", temperature);
            return 1;
        }

        String humidity = String.valueOf(weatherData.getHumidityAsPercentage());
        if (humidity.length() == 1) {
            humidity = "00" + humidity;
        } else if (humidity.length() == 2) {
            humidity = "0" + humidity;
        } 
        if (humidity.length() != 3) {
            LOGGER.error("Invalid humidity value {}", humidity);
            return 1;
        }

        String cloudCover = String.valueOf(weatherData.getCloudCoverAsInteger());
        if (cloudCover.length() != 1) {
            LOGGER.error("Invalid cloud cover value {}", cloudCover);
        }

        String templateContent;
        try (InputStream inputStream = getClass().getClassLoader().getResourceAsStream("weather_template.144")) {
            templateContent = IOUtils.toString(inputStream, StandardCharsets.UTF_8);

            templateContent = templateContent.replace("WS", windSpeed).replace("WD", windDirection).replace("TEM", temperature)
            .replace("HUM", humidity).replace("C", cloudCover);
        } catch (IOException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed to read weather_template.144 file");
            return 1;
        }

        return writeToFile(aermetDirectory.resolve("weather_template.144"), templateContent);
    }

    /**
     * return value 0 = success
     * 1 = possible error
     * @return
     */
    int runAermet() {
        // first copy soundings and FSL files
        try (InputStream is = getClass().getClassLoader().getResourceAsStream(AERMET_INPUT)) {
            Files.copy(is, aermetDirectory.resolve(AERMET_INPUT));
        } catch (IOException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed to copy {}", AERMET_INPUT);
            return 1;
        }

        try (InputStream is = getClass().getClassLoader().getResourceAsStream("raob_soundings15747.FSL")) {
            Files.copy(is, aermetDirectory.resolve("raob_soundings15747.FSL"));
        } catch (IOException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed to copy raob_soundings15747.FSL");
            return 1;
        }

        try {
            Process process = Runtime.getRuntime().exec(new String[]{EnvConfig.AERMET_EXE, AERMET_INPUT}, null, aermetDirectory.toFile());
            if (process.waitFor() != 0) {
                return 1;
            }
        } catch (IOException e) {
            LOGGER.error("Error executing aermet");
            LOGGER.error(e.getMessage());
            return 1;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            LOGGER.error("Error executing aermet");
            LOGGER.error(e.getMessage());
            return 1;
        }

        // check if outputs are generated
        File surfaceFile = aermodDirectory.resolve("AERMET_SURF.SFC").toFile();
        File upperFile = aermodDirectory.resolve("AERMET_UPPER.PFL").toFile();

        if (surfaceFile.exists() && upperFile.exists()) {
            LOGGER.info("aermet is executed successfully");
            return 0;
        } else {
            LOGGER.error("aermet may not have completed successfully");
            return 1;
        }
    }

    int createPointsFile(List<Ship> ships, int simulationSrid) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < ships.size(); i++) {
            Ship ship = ships.get(i);
            String stkId = "S" + i;

            String originalSrid = "EPSG:" + ship.getLocation().getSrid();
            double[] xyOriginal = {ship.getLocation().getX(), ship.getLocation().getY()};
            double[] xyTransformed = CRSTransformer.transform(originalSrid, "EPSG:" + simulationSrid, xyOriginal);

            double area = Math.PI * Math.pow(ship.getChimney().getDiameter()/2, 2); // m2
            double density = ship.getChimney().getMixtureDensityInKgm3(); // kg/m3
            double velocity = ship.getChimney().getFlowrateSO2InKgs() / area / density; // m/s

            double massFlowrateInGs = ship.getChimney().getFlowrateSO2InKgs() * 1000;

            sb.append(String.format("SO LOCATION %s POINT %f %f %f",stkId, xyTransformed[0], xyTransformed[1], ship.getChimney().getHeight()));
            sb.append(System.lineSeparator());
            sb.append(String.format("SO SRCPARAM %s %f %f %f %f %f", stkId, 
            massFlowrateInGs, ship.getChimney().getHeight(), ship.getChimney().getMixtureTemperatureInKelvin(), velocity, ship.getChimney().getDiameter()));
            sb.append(System.lineSeparator());
        }
        return writeToFile(aermodDirectory.resolve("shipSources.dat"), sb.toString());
    }
    
    private int writeToFile(Path path, String content) {
        try (BufferedWriter writer = Files.newBufferedWriter(path)) {
            LOGGER.info("Writing file: {}", path);
            writer.write(content);
            return 0;
        } catch (IOException e) {
            String errmsg = "Failed to write " + path.getFileName();
            LOGGER.error(errmsg);
            LOGGER.error(e.getMessage());
            return 1;
        }
    }

    public int createAermodInputFile(Geometry scope, int nx, int ny, int srid) {
        List<Double> xDoubles = new ArrayList<>();
        List<Double> yDoubles = new ArrayList<>();

        String originalSRID = "EPSG:" + scope.getSRID();
        
        for (int i = 0; i < scope.getCoordinates().length; i++) {
            double[] xyTransformed = CRSTransformer.transform(originalSRID, "EPSG:" + srid, scope.getCoordinates()[i].x, scope.getCoordinates()[i].y);

            xDoubles.add(xyTransformed[0]);
            yDoubles.add(xyTransformed[1]);
        }

        double xMax = Collections.max(xDoubles);
        double xMin = Collections.min(xDoubles);
        double yMax = Collections.max(yDoubles);
        double yMin = Collections.min(yDoubles);

        double dy = (yMax - yMin) / ny;
        double dx = (xMax - xMin) / nx;
        
        String templateContent;
        try (InputStream inputStream = getClass().getClassLoader().getResourceAsStream("aermod_input.inp")) {
            templateContent = IOUtils.toString(inputStream, StandardCharsets.UTF_8);
            String simGrid = String.format("%f %d %f %f %d %f", xMin, nx, dx, yMin, ny, dy);
            templateContent = templateContent.replace("REPLACED_BY_AERMOD_AGENT", simGrid);
        } catch (IOException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed to read aermod_input.inp file");
            return 1;
        }

        return writeToFile(aermodDirectory.resolve("aermod_input.inp"), templateContent);
    }

    int runAermod() {
        try {
            Process process = Runtime.getRuntime().exec(new String[]{EnvConfig.AERMOD_EXE, "aermod_input.inp"}, null, aermodDirectory.toFile());
            if (process.waitFor() != 0) {
                return 1;
            }
        } catch (IOException e) {
            return 0;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return 0;
        }
        return 0;
    }

    String uploadToFileServer() {
        // upload to file server via HTTP POST

        MultipartEntityBuilder multipartBuilder = MultipartEntityBuilder.create();
        multipartBuilder.addPart("dispersionMatrix", new FileBody(aermodDirectory.resolve("1HR_PLOTFILE.DAT").toFile()));
        multipartBuilder.addPart("buildingOutput",new FileBody(aermodDirectory.resolve("buildings.dat").toFile()));

        HttpPost httpPost = new HttpPost(EnvConfig.FILE_SERVER + simulationDirectory.getFileName() + "/");
        httpPost.setEntity(multipartBuilder.build());

        String auth = "fs_user" + ":" + "fs_pass"; // default credentials for the file server container
        String encodedAuth = Base64.getEncoder().encodeToString(auth.getBytes());
        httpPost.setHeader("Authorization", "Basic " + encodedAuth);

        try (CloseableHttpClient httpClient = HttpClients.createDefault(); CloseableHttpResponse response = httpClient.execute(httpPost);) {
            if (response.getFirstHeader("dispersionMatrix") == null) {
                throw new RuntimeException("Header from file server is null");
            } else {
                String fileURL = response.getFirstHeader("dispersionMatrix").getValue();
                LOGGER.info("File URL at file server: {}", fileURL);
                return fileURL;
            }
        } catch (IOException e) {
            LOGGER.error("File upload failed");
            LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

    /**
     * executes get request from python-service to postprocess
     */
    public JSONObject getGeoJSON(String outputFileURL, int srid) {
        URI httpGet;
        try {
            URIBuilder builder = new URIBuilder(EnvConfig.PYTHON_SERVICE_URL);
            builder.setParameter("dispersionMatrix", outputFileURL);
            builder.setParameter("srid", String.valueOf(srid));
            httpGet = builder.build();
        } catch (URISyntaxException e) {
            LOGGER.error("Failed at building URL");
            throw new RuntimeException(e);
        }

        try (CloseableHttpClient httpClient = HttpClients.createDefault();
            CloseableHttpResponse httpResponse = httpClient.execute(new HttpGet(httpGet))) {
            String result = EntityUtils.toString(httpResponse.getEntity());
            return new JSONObject(result);
        } catch (IOException e) {
            LOGGER.error("Failed at making connection with python service");
            throw new RuntimeException(e);
        } catch (JSONException e) {
            LOGGER.error("Failed to parse result from python service for aermod geojson");
            throw new RuntimeException(e);
        }
    }

    int createDataJson(String shipLayerName, String dispersionLayerName) {
        // wms endpoints template without the layer name
        String shipWms = EnvConfig.GEOSERVER_URL + "/dispersion/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile" +
            "&bbox={bbox-epsg-3857}" + String.format("&layers=%s:%s", EnvConfig.GEOSERVER_WORKSPACE, shipLayerName);

        String dispWms = EnvConfig.GEOSERVER_URL + "/dispersion/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=image/png&transparent=true" +
            "&bbox={bbox-epsg-3857}" + String.format("&layers=%s:%s", EnvConfig.GEOSERVER_WORKSPACE, dispersionLayerName);

        JSONObject group = new JSONObject();
        group.put("name", "Plymouth"); // hardcoded
        group.put("stack", "http://featureInfoAgent");

        // sources
        JSONArray sources = new JSONArray();
        JSONObject shipSource = new JSONObject();
        shipSource.put("id", "ship-source");
        shipSource.put("type", "vector");
        shipSource.put("tiles", new JSONArray().put(shipWms));

        JSONObject dispersionSource = new JSONObject();
        dispersionSource.put("id", "dispersion-source");
        dispersionSource.put("type", "raster");
        dispersionSource.put("tiles", new JSONArray().put(dispWms));

        sources.put(shipSource).put(dispersionSource);
        group.put("sources", sources);

        // layers
        JSONArray layers = new JSONArray();
        JSONObject shipLayer = new JSONObject();
        shipLayer.put("id", "ships-layer");
        shipLayer.put("type", "circle");
        shipLayer.put("name", "Ships");
        shipLayer.put("source", "ship-source");
        shipLayer.put("source-layer", shipLayerName);
        shipLayer.put("minzoom", 4);
        shipLayer.put("layout", new JSONObject().put("visibility", "visible"));

        JSONObject dispersionLayer = new JSONObject();
        dispersionLayer.put("type", "raster");
        dispersionLayer.put("name", "Dispersion");
        dispersionLayer.put("source", "dispersion-source");
        dispersionLayer.put("source-layer", dispersionLayerName);
        dispersionLayer.put("minzoom", 4);
        dispersionLayer.put("layout", new JSONObject().put("visibility", "visible"));

        layers.put(dispersionLayer).put(shipLayer);
        group.put("layers", layers);

        JSONObject data = new JSONObject();
        data.put("name", "All Data");
        data.put("groups", new JSONArray().put(group));

        File dataJson = Paths.get(EnvConfig.VIS_FOLDER, "data.json").toFile();
        
        try {
            Files.deleteIfExists(dataJson.toPath());
        } catch(IOException e) {
            LOGGER.error("Failed to delete file");
            return 1;
        }

        return writeToFile(dataJson.toPath(), data.toString(4));
    }

    /**
     * required for current way of writing into a volume shared by visualisation container
     * without this the visualisation container cannot access the file
     * @return
     */
    int modifyDataFilePermissions() {
        try {
            Process process = Runtime.getRuntime().exec(new String[]{"chmod", "a+rwx", "data.json"}, null, new File(EnvConfig.VIS_FOLDER));
            if (process.waitFor() != 0) {
                return 1;
            }
            return 0;
        } catch (IOException e) {
            return 0;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return 0;
        }
    }

    /**
     * this file sets the centre of the map on first load
     */
    int createVisSettingsFile(Point centroid) {
        JSONObject start = new JSONObject();
        start.put("center", new JSONArray().put(centroid.getX()).put(centroid.getY()));
        start.put("zoom", 10.5);
        
        JSONObject search = new JSONObject();
        search.put("Name", "name");
        search.put("ID", "id");

        JSONObject overall = new JSONObject();
        overall.put("start", start);
        overall.put("search", search);

        File settingsJson = Paths.get(EnvConfig.VIS_FOLDER, "settingsJson.json").toFile();
        try {
            Files.deleteIfExists(settingsJson.toPath());
        } catch(IOException e) {
            LOGGER.error("Failed to delete settings.json");
            return 1;
        }

        return writeToFile(settingsJson.toPath(), overall.toString(4));
    }
}
