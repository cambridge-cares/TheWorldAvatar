package com.cmclinnovations.aermod;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collections;
import java.util.List;
import org.apache.commons.io.IOUtils;
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
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;

import com.cmclinnovations.aermod.objects.Building;
import com.cmclinnovations.aermod.objects.PointSource;
import com.cmclinnovations.aermod.objects.StaticPointSource;
import com.cmclinnovations.aermod.objects.WeatherData;

import uk.ac.cam.cares.jps.base.util.CRSTransformer;

public class Aermod {
    private static final Logger LOGGER = LogManager.getLogger(Aermod.class);

    private Path aermapDirectory;
    private Path bpipprmDirectory;
    private Path aermetDirectory;
    private Path aermodDirectory;
    private Path simulationDirectory;
    private static final String AERMET_INPUT = "aermet.inp";

    public Aermod(Path simulationDirectory) {
        this.simulationDirectory = simulationDirectory;
        LOGGER.info("Creating directory for simulation files");
        simulationDirectory.toFile().mkdirs();

        this.aermapDirectory = simulationDirectory.resolve("aermap");
        aermapDirectory.toFile().mkdir();

        this.bpipprmDirectory = simulationDirectory.resolve("bpipprm");
        bpipprmDirectory.toFile().mkdir();

        this.aermetDirectory = simulationDirectory.resolve("aermet");
        aermetDirectory.toFile().mkdir();

        this.aermodDirectory = simulationDirectory.resolve("aermod");
        aermodDirectory.toFile().mkdir();
    }

    /* Write out data to BPIPPRM input file and run this program. */
    public int createBPIPPRMInput(List<StaticPointSource> pointSources, List<Building> buildings, int simulationSrid) {

        List<String> frontmatter = new ArrayList<>();
        frontmatter.add("\'BPIPPRM test run\' \n");
        frontmatter.add("\'p\' \n");
        frontmatter.add("\' METERS    \'  1.0  \n");
        frontmatter.add("\'UTMY \'  0.0 \n");

        StringBuilder sb = new StringBuilder();

        for (String st : frontmatter) {
            sb.append(st);
        }

        int numberBuildings = buildings.size();
        sb.append(numberBuildings + " \n");
        for (int i = 0; i < numberBuildings; i++) {
            Building build = buildings.get(i);
            String inputLine = "\'Build" + i + "\' " + "1 " + build.getElevation() + " \n";
            sb.append(inputLine);
            LinearRing base = build.getFootprint();
            String originalSrid = "EPSG:" + base.getSRID();
            inputLine = base.getNumPoints() + " " + build.getHeight() + " \n";
            sb.append(inputLine);

            Coordinate[] baseCoords = base.getCoordinates();
            for (Coordinate coord : baseCoords) {
                double[] xyOriginal = { coord.x, coord.y };
                double[] xyTransformed = CRSTransformer.transform(originalSrid, "EPSG:" + simulationSrid, xyOriginal);
                inputLine = xyTransformed[0] + " " + xyTransformed[1] + " \n";
                sb.append(inputLine);

            }
        }

        int numberSources = pointSources.size();
        sb.append(numberSources + " \n");
        for (int i = 0; i < numberSources; i++) {
            StaticPointSource ps = pointSources.get(i);
            String originalSrid = "EPSG:" + ps.getLocation().getSRID();
            double[] xyOriginal = { ps.getLocation().getX(), ps.getLocation().getY() };
            double[] xyTransformed = CRSTransformer.transform(originalSrid, "EPSG:" + simulationSrid, xyOriginal);
            String inputLine = "\'S" + i + "\'" + " " + ps.getElevation() + " " +
                    ps.getHeight() + " " + xyTransformed[0] + " " + xyTransformed[1] + " \n";
            sb.append(inputLine);
        }
        return writeToFile(bpipprmDirectory.resolve("bpipprm.inp"), sb.toString());

    }

    public int runBPIPPRM() {
        try {
            Process process = Runtime.getRuntime().exec(
                    new String[] { EnvConfig.BPIPPRM_EXE, "bpipprm.inp", "building.dat", "buildings_summary.dat" },
                    null, bpipprmDirectory.toFile());
            if (process.waitFor() != 0) {
                return 1;
            }
            BufferedReader stdInput = new BufferedReader(new InputStreamReader(process.getInputStream()));

            BufferedReader stdError = new BufferedReader(new InputStreamReader(process.getErrorStream()));

            // Read the output from the command
            LOGGER.info("Here is the standard output of BPIPPRM:\n");
            String s = null;
            while ((s = stdInput.readLine()) != null) {
                LOGGER.info(s);
            }

            // Read any errors from the attempted command
            LOGGER.info("Here is the standard error of BPIPPRM (if any):\n");
            while ((s = stdError.readLine()) != null) {
                LOGGER.info(s);
            }
        } catch (IOException e) {
            return 0;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return 0;
        }
        return 0;
    }

    public int createAERMODBuildingsInput() {

        StringBuilder sb = new StringBuilder();

        Path filepath = bpipprmDirectory.resolve("building.dat");

        try {
            BufferedReader reader = new BufferedReader(new FileReader(filepath.toString()));
            String line = reader.readLine();
            while (line != null) {
                line = line.stripLeading();
                if (line.length() > 2 && line.substring(0, 2).equals("SO"))
                    sb.append(line + "\n");
                line = reader.readLine();
            }
            reader.close();
            LOGGER.info(sb.toString());
        } catch (IOException e) {
            LOGGER.error(e.getMessage());
        }

        return writeToFile(aermodDirectory.resolve("buildings.dat"), sb.toString());
    }

    public JSONObject createStaticPointSourcesJSON(List<StaticPointSource> pointSources) {

        // define a list of (longitude, latitude) coordinates
        List<List<Double>> lonlatCoordinates = new ArrayList<>();

        for (int i = 0; i < pointSources.size(); i++) {
            StaticPointSource sps = pointSources.get(i);
            String originalSrid = "EPSG:" + sps.getLocation().getSRID();
            double[] xyOriginal = { sps.getLocation().getX(), sps.getLocation().getY() };
            double[] xyTransformed = CRSTransformer.transform(originalSrid, "EPSG:4326", xyOriginal);
            List<Double> tmp = Arrays.asList(xyTransformed[0], xyTransformed[1]);
            lonlatCoordinates.add(tmp);
        }

        // create a JSONObject that represents a GeoJSON Feature Collection
        JSONObject featureCollection = new JSONObject();
        featureCollection.put("type", "FeatureCollection");
        JSONArray features = new JSONArray();

        // loop through the coordinates and add them as GeoJSON Points to the Feature
        // Collection
        for (List<Double> coordinate : lonlatCoordinates) {
            JSONObject geometry = new JSONObject();
            geometry.put("type", "Point");
            geometry.put("coordinates", new JSONArray(coordinate));
            JSONObject feature = new JSONObject();
            feature.put("type", "Feature");
            feature.put("geometry", geometry);
            features.put(feature);
        }
        featureCollection.put("features", features);

        LOGGER.info("Uploading plant items GeoJSON to PostGIS");
        return featureCollection;

    }

    public int createAERMODReceptorInput(Polygon scope, int nx, int ny, int simulationSrid) {

        List<Double> xDoubles = new ArrayList<>();
        List<Double> yDoubles = new ArrayList<>();

        for (int i = 0; i < scope.getCoordinates().length; i++) {

            String originalSrid = "EPSG:4326";
            double[] xyOriginal = { scope.getCoordinates()[i].x, scope.getCoordinates()[i].y };
            double[] xyTransformed = CRSTransformer.transform(originalSrid, "EPSG:" + simulationSrid, xyOriginal);

            xDoubles.add(xyTransformed[0]);
            yDoubles.add(xyTransformed[1]);
        }

        double xlo = Collections.min(xDoubles);
        double xhi = Collections.max(xDoubles);
        double ylo = Collections.min(yDoubles);
        double yhi = Collections.max(yDoubles);

        double dx = (xhi - xlo) / nx;
        double dy = (yhi - ylo) / ny;

        StringBuilder sb = new StringBuilder("RE GRIDCART POL1 STA \n");
        String rec = String.format("                 XYINC %f %d %f %f %d %f", xlo, nx, dx, ylo, ny, dy);
        sb.append(rec + " \n");
        sb.append("RE GRIDCART POL1 END \n");

        return writeToFile(aermodDirectory.resolve("receptor.dat"), sb.toString());
    }

    String addLeadingZero(String variable, int length) {
        while (variable.length() < length) {
            variable = "0" + variable;
        }
        return variable;
    }

    int create144File(WeatherData weatherData) {

        String windSpeed = String.valueOf(weatherData.getWindSpeedInKnots());
        windSpeed = addLeadingZero(windSpeed, 2);
        if (windSpeed.length() != 2) {
            LOGGER.error("Invalid wind speed value {}", windSpeed);
            return 1;
        }

        String windDirection = String.valueOf(weatherData.getWindDirectionInTensOfDegrees());
        windDirection = addLeadingZero(windDirection, 2);
        if (windDirection.length() != 2) {
            LOGGER.error("Invalid wind direction value {}", windDirection);
            return 1;
        }

        // unsure how strict the format is, just follow blindly at the moment
        String temperature = String.valueOf(weatherData.getTemperatureInFahrenheit());
        temperature = addLeadingZero(temperature, 3);
        if (temperature.length() < 3) {
            temperature = "0" + temperature;
        }
        if (temperature.length() != 3) {
            LOGGER.error("Invalid temperature value {}", temperature);
            return 1;
        }

        String humidity = String.valueOf(weatherData.getHumidityAsPercentage());
        humidity = addLeadingZero(humidity, 3);
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

            templateContent = templateContent.replace("WS", windSpeed).replace("WD", windDirection)
                    .replace("TEM", temperature)
                    .replace("HUM", humidity).replace("C", cloudCover);
        } catch (IOException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed to read weather_template.144 file");
            return 1;
        }

        return writeToFile(aermetDirectory.resolve("weather_template.144"), templateContent);
    }

    public int createAermetInput(Polygon scope) {

        double lat = scope.getCentroid().getCoordinate().getY();
        double lon = scope.getCentroid().getCoordinate().getX();

        String latSuffix = "N";
        String lonSuffix = "E";
        int direction = -1;
        if (lat < 0)
            latSuffix = "S";
        if (lon < 0) {
            lonSuffix = "W";
            direction = 1;
        }
        lat = Math.abs(lat);
        lon = Math.abs(lon);
        int offset = (int) Math.round(direction * lon * 24 / 360);
        String location = String.format("%f%s %f%s", lat, latSuffix, lon, lonSuffix);
        String locationOffset = String.format("%f%s %f%s %d", lat, latSuffix, lon, lonSuffix, offset);
        String newLine = System.getProperty("line.separator");

        StringBuilder sb = new StringBuilder("JOB \n");
        sb.append("   REPORT aermet_report.txt \n")
                .append("   MESSAGES aermet_message.txt \n")
                .append(newLine)
                .append("UPPERAIR \n")
                .append("   DATA raob_soundings15747.FSL FSL \n")
                .append("   EXTRACT  UAIR_EXTRACT.TXT \n")
                .append("   QAOUT    UAIR_QAOUT.TXT \n")
                .append("   XDATES    2022/9/23 TO 2022/9/23 \n")
                .append("   LOCATION  99999  " + locationOffset + " \n")
                .append(newLine)
                .append("SURFACE \n")
                .append("   DATA      weather_template.144 CD144 \n")
                .append("   EXTRACT   SURFACE_EXTRACT.TXT \n")
                .append("   QAOUT   SURFACE_QAOUT.TXT \n")
                .append("   XDATES    2022/9/23 TO 2022/9/23 \n")
                .append("   LOCATION  99999  " + location + "\n")
                .append(newLine)
                .append("METPREP \n")
                .append("   OUTPUT ../aermod/AERMET_SURF.SFC \n")
                .append("   PROFILE ../aermod/AERMET_UPPER.PFL \n")
                .append("   NWS_HGT WIND 6.7 \n")
                .append("   METHOD REFLEVEL SUBNWS \n")
                .append("   METHOD WIND_DIR RANDOM \n")
                .append("   XDATES    2022/9/23 TO 2022/9/23 \n")
                .append("   FREQ_SECT ANNUAL 1 \n")
                .append("   SECTOR 1 0 360 \n")
                .append("   SITE_CHAR 1 1 0.16 2.0 1.5 \n");

        return writeToFile(aermetDirectory.resolve("aermet.inp"), sb.toString());

    }

    /**
     * return value 0 = success
     * 1 = possible error
     * 
     * @return
     */
    int runAermet(Polygon scope) {

        // first copy soundings and FSL files
        // try (InputStream is =
        // getClass().getClassLoader().getResourceAsStream(AERMET_INPUT)) {
        // Files.copy(is, aermetDirectory.resolve(AERMET_INPUT));
        // } catch (IOException e) {
        // LOGGER.error(e.getMessage());
        // LOGGER.error("Failed to copy {}", AERMET_INPUT);
        // return 1;
        // }

        createAermetInput(scope);

        try (InputStream is = getClass().getClassLoader().getResourceAsStream("raob_soundings15747.FSL")) {
            Files.copy(is, aermetDirectory.resolve("raob_soundings15747.FSL"));
        } catch (IOException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed to copy raob_soundings15747.FSL");
            return 1;
        }

        try {
            Process process = Runtime.getRuntime().exec(new String[] { EnvConfig.AERMET_EXE, AERMET_INPUT }, null,
                    aermetDirectory.toFile());
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

    int createPointsFile(List<PointSource> pointSources, int simulationSrid) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < pointSources.size(); i++) {

            PointSource ps = pointSources.get(i);
            String stkId = "S" + i;

            String originalSrid = "EPSG:" + ps.getLocation().getSRID();
            double[] xyOriginal = { ps.getLocation().getX(), ps.getLocation().getY() };
            double[] xyTransformed = CRSTransformer.transform(originalSrid, "EPSG:" + simulationSrid, xyOriginal);

            double area = Math.PI * Math.pow(ps.getDiameter() / 2, 2); // m2
            double density = ps.getMixtureDensityInKgm3(); // kg/m3
            double velocity = ps.getFlowrateSO2InGramsPerS() / 1000 / area / density; // m/s

            double massFlowrateInGs = ps.getFlowrateNOxInGramsPerS();

            sb.append(String.format("SO LOCATION %s POINT %f %f %f", stkId, xyTransformed[0], xyTransformed[1],
                    ps.getHeight()));
            sb.append(System.lineSeparator());
            sb.append(String.format("SO SRCPARAM %s %f %f %f %f %f", stkId,
                    massFlowrateInGs, ps.getHeight(), ps.getMixtureTemperatureInKelvin(),
                    velocity, ps.getDiameter()));
            sb.append(System.lineSeparator());
        }
        return writeToFile(aermodDirectory.resolve("points.so"), sb.toString());
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
            double[] xyTransformed = CRSTransformer.transform(originalSRID, "EPSG:" + srid, scope.getCoordinates()[i].x,
                    scope.getCoordinates()[i].y);

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
        try (InputStream inputStream = getClass().getClassLoader().getResourceAsStream("aermod.inp")) {
            templateContent = IOUtils.toString(inputStream, StandardCharsets.UTF_8);
            String simGrid = String.format("%f %d %f %f %d %f", xMin, nx, dx, yMin, ny, dy);
            templateContent = templateContent.replace("REPLACED_BY_AERMOD_AGENT", simGrid);
        } catch (IOException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed to read aermod.inp file");
            return 1;
        }

        return writeToFile(aermodDirectory.resolve("aermod.inp"), templateContent);
    }

    int runAermod(String aermodInputFile) {

        File tempFile = new File(aermodDirectory.resolve(aermodInputFile).toString());

        if (!tempFile.exists()) {
            try (InputStream inputStream = getClass().getClassLoader().getResourceAsStream(aermodInputFile)) {
                Files.copy(inputStream, aermodDirectory.resolve(aermodInputFile));
            } catch (IOException e) {
                LOGGER.error(e.getMessage());
                LOGGER.error("Failed to copy the data file" + aermodInputFile);
                return 1;
            }
        }

        try {
            Process process = Runtime.getRuntime().exec(new String[] { EnvConfig.AERMOD_EXE, aermodInputFile }, null,
                    aermodDirectory.toFile());
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

    String uploadToFileServer(String filename) {
        // upload to file server via HTTP POST

        MultipartEntityBuilder multipartBuilder = MultipartEntityBuilder.create();
        multipartBuilder.addPart("dispersionMatrix", new FileBody(aermodDirectory.resolve(filename).toFile()));

        HttpPost httpPost = new HttpPost(EnvConfig.FILE_SERVER + simulationDirectory.getFileName() + "/");
        httpPost.setEntity(multipartBuilder.build());

        String auth = "fs_user" + ":" + "fs_pass"; // default credentials for the file server container
        String encodedAuth = Base64.getEncoder().encodeToString(auth.getBytes());
        httpPost.setHeader("Authorization", "Basic " + encodedAuth);

        try (CloseableHttpClient httpClient = HttpClients.createDefault();
                CloseableHttpResponse response = httpClient.execute(httpPost);) {
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
    public JSONObject getGeoJSON(String endPoint, String outputFileURL, int srid, double height) {
        URI httpGet;
        try {
            URIBuilder builder = new URIBuilder(endPoint);
            builder.setParameter("dispersionMatrix", outputFileURL);
            builder.setParameter("srid", String.valueOf(srid));
            builder.setParameter("height", String.valueOf(height));
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
            LOGGER.error(outputFileURL);
            throw new RuntimeException(e);
        }
    }

    JSONObject getBuildingsGeoJSON(List<Building> buildings) {
        JSONObject featureCollection = new JSONObject();
        featureCollection.put("type", "FeatureCollection");

        JSONArray features = new JSONArray();
        buildings.stream().forEach(building -> {
            JSONObject feature = new JSONObject();
            feature.put("type", "Feature");

            JSONObject properties = new JSONObject();
            properties.put("color", "#666666");
            properties.put("opacity", 0.66);
            properties.put("base", 0);
            properties.put("height", building.getHeight());
            feature.put("properties", properties);

            JSONObject geometry = new JSONObject();
            geometry.put("type", "Polygon");
            JSONArray coordinates = new JSONArray();

            JSONArray footprintPolygon = new JSONArray();
            String srid = building.getSrid();
            for (Coordinate coordinate : building.getFootprint().getCoordinates()) {
                JSONArray point = new JSONArray();
                double[] xyOriginal = { coordinate.getX(), coordinate.getY() };
                double[] xyTransformed = CRSTransformer.transform(srid, "EPSG:4326", xyOriginal);
                point.put(xyTransformed[0]).put(xyTransformed[1]);
                footprintPolygon.put(point);
            }
            coordinates.put(footprintPolygon);
            geometry.put("coordinates", coordinates);

            feature.put("geometry", geometry);
            features.put(feature);
        });

        featureCollection.put("features", features);

        return featureCollection;
    }

    int createDataJson(String shipLayerName, List<String> dispersionLayerNames, String plantsLayerName,
            String elevationLayerName, JSONObject buildingsGeoJSON) {
        // wms endpoints template without the layer name
        String shipWms = EnvConfig.GEOSERVER_URL
                + "/dispersion/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile"
                +
                "&bbox={bbox-epsg-3857}" + String.format("&layers=%s:%s", EnvConfig.GEOSERVER_WORKSPACE, shipLayerName);

        String dispWms = EnvConfig.GEOSERVER_URL
                + "/dispersion/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=image/png&transparent=true"
                +
                "&bbox={bbox-epsg-3857}" + String.format("&layers=%s:%s", EnvConfig.GEOSERVER_WORKSPACE, "PLACEHOLDER");

        String plantWms = EnvConfig.GEOSERVER_URL
                + "/dispersion/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile"
                +
                "&bbox={bbox-epsg-3857}"
                + String.format("&layers=%s:%s", EnvConfig.GEOSERVER_WORKSPACE, plantsLayerName);

        String elevWms = EnvConfig.GEOSERVER_URL
                + "/dispersion/wms?service=WMS&version=1.1.0&request=GetMap&width=256&height=256&srs=EPSG:3857&format=image/png&transparent=true"
                +
                "&bbox={bbox-epsg-3857}"
                + String.format("&layers=%s:%s", EnvConfig.GEOSERVER_WORKSPACE, elevationLayerName);

        JSONObject group = new JSONObject();
        group.put("name", "Aermod Simulation"); // hardcoded
        group.put("stack", "http://localhost:3838");

        // sources
        JSONArray sources = new JSONArray();
        JSONObject shipSource = new JSONObject();
        shipSource.put("id", "ship-source");
        shipSource.put("type", "vector");
        shipSource.put("tiles", new JSONArray().put(shipWms));

        JSONObject plantSource = new JSONObject();
        plantSource.put("id", "plant-source");
        plantSource.put("type", "vector");
        plantSource.put("tiles", new JSONArray().put(plantWms));

        for (int i = 0; i < dispersionLayerNames.size(); i++) {
            JSONObject dispersionSource = new JSONObject();
            dispersionSource.put("id", "dispersion-source_" + dispersionLayerNames.get(i));
            dispersionSource.put("type", "raster");
            dispersionSource.put("tiles",
                    new JSONArray().put(dispWms.replace("PLACEHOLDER", dispersionLayerNames.get(i))));
            sources.put(dispersionSource);
        }

        JSONObject elevationSource = new JSONObject();
        elevationSource.put("id", "elevation-source");
        elevationSource.put("type", "raster");
        elevationSource.put("tiles", new JSONArray().put(elevWms));

        sources.put(shipSource).put(plantSource).put(elevationSource);
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

        JSONObject plantsLayer = new JSONObject();
        plantsLayer.put("id", "plants-layer");
        plantsLayer.put("type", "circle");
        plantsLayer.put("name", "Chemical Plant Items");
        plantsLayer.put("source", "plant-source");
        plantsLayer.put("source-layer", plantsLayerName);
        plantsLayer.put("minzoom", 4);
        plantsLayer.put("layout", new JSONObject().put("visibility", "visible"));

        for (int i = 0; i < dispersionLayerNames.size(); i++) {
            JSONObject dispersionLayer = new JSONObject();
            dispersionLayer.put("id", dispersionLayerNames.get(i));
            dispersionLayer.put("type", "raster");
            dispersionLayer.put("name", dispersionLayerNames.get(i));
            dispersionLayer.put("source", "dispersion-source_" + dispersionLayerNames.get(i));
            dispersionLayer.put("source-layer", dispersionLayerNames.get(i));
            dispersionLayer.put("minzoom", 4);
            dispersionLayer.put("layout", new JSONObject().put("visibility", "visible"));
            layers.put(dispersionLayer);
        }

        JSONObject elevationLayer = new JSONObject();
        elevationLayer.put("id", "elevation-layer");
        elevationLayer.put("type", "raster");
        elevationLayer.put("name", "Elevation");
        elevationLayer.put("source", "elevation-source");
        elevationLayer.put("source-layer", elevationLayerName);
        elevationLayer.put("minzoom", 4);
        elevationLayer.put("layout", new JSONObject().put("visibility", "visible"));

        layers.put(shipLayer).put(plantsLayer).put(elevationLayer);
        group.put("layers", layers);

        JSONObject data = new JSONObject();
        data.put("name", "All Data");
        data.put("groups", new JSONArray().put(group));

        File dataJson = Paths.get(EnvConfig.VIS_FOLDER, "data.json").toFile();

        try {
            Files.deleteIfExists(dataJson.toPath());
        } catch (IOException e) {
            LOGGER.error("Failed to delete file");
            return 1;
        }

        if (!buildingsGeoJSON.isEmpty()) {
            String geoJsonFilename = "buildings.geojson";
            Path buildingsGeoJSONPath = Paths.get(EnvConfig.VIS_FOLDER, geoJsonFilename);
            writeToFile(buildingsGeoJSONPath, buildingsGeoJSON.toString(4));
            modifyFilePermissions(buildingsGeoJSONPath.toString());

            JSONObject buildingsSource = new JSONObject();
            buildingsSource.put("id", "buildings-source");
            buildingsSource.put("type", "geojson");
            buildingsSource.put("data", Paths.get(geoJsonFilename).toString());

            JSONObject buildingsLayer = new JSONObject();
            buildingsLayer.put("name", "Buildings");
            buildingsLayer.put("type", "fill-extrusion");
            buildingsLayer.put("source", "buildings-source");
            buildingsLayer.put("layout", new JSONObject().put("visibility", "visible"));
            JSONObject paint = new JSONObject();
            paint.put("fill-extrusion-color", new JSONArray().put("get").put("color"));
            paint.put("fill-extrusion-height", new JSONArray().put("get").put("height"));
            paint.put("fill-extrusion-opacity", 0.66);
            paint.put("fill-extrusion-base", new JSONArray().put("get").put("base"));
            buildingsLayer.put("paint", paint);

            sources.put(buildingsSource);
            layers.put(buildingsLayer);
        }

        return writeToFile(dataJson.toPath(), data.toString(4));
    }

    /**
     * required for current way of writing into a volume shared by visualisation
     * container
     * without this the visualisation container cannot access the file
     * 
     * @return
     */
    int modifyFilePermissions(String filename) {
        try {
            Process process = Runtime.getRuntime().exec(new String[] { "chmod", "a+rwx", filename }, null,
                    new File(EnvConfig.VIS_FOLDER));
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

        File settingsJson = Paths.get(EnvConfig.VIS_FOLDER, "settings.json").toFile();
        try {
            Files.deleteIfExists(settingsJson.toPath());
        } catch (IOException e) {
            LOGGER.error("Failed to delete settings.json");
            return 1;
        }

        return writeToFile(settingsJson.toPath(), overall.toString(4));
    }
}
