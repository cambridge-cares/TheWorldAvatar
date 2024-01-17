package com.cmclinnovations.aermod;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collections;
import java.util.List;
import java.util.Scanner;
import java.util.UUID;
import java.util.stream.Collectors;

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
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.Polygon;

import com.cmclinnovations.aermod.objects.Building;
import com.cmclinnovations.aermod.objects.PointSource;
import com.cmclinnovations.aermod.objects.Pollutant;
import com.cmclinnovations.aermod.objects.StaticPointSource;
import com.cmclinnovations.aermod.objects.WeatherData;
import com.cmclinnovations.aermod.objects.Pollutant.PollutantType;
import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.GDALTranslateOptions;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;

import uk.ac.cam.cares.jps.base.util.CRSTransformer;

public class Aermod {
    private static final Logger LOGGER = LogManager.getLogger(Aermod.class);

    private Path aermapDirectory;
    private Path bpipprmDirectory;
    private Path aermetDirectory;
    private Path aermodDirectory;
    private Path rasterDirectory;
    private static final String AERMET_INPUT = "aermet.inp";

    public Aermod(Path simulationDirectory) {
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

        rasterDirectory = simulationDirectory.resolve("raster");
        rasterDirectory.toFile().mkdir();
    }

    /* Write out data to BPIPPRM input file and run this program. */
    public void createBPIPPRMInput(List<PointSource> pointSources, List<Building> buildings, int simulationSrid) {
        StringBuilder sb = new StringBuilder();
        sb.append("\'BPIPPRM test run\' ");
        sb.append(System.lineSeparator());
        sb.append("\'p\' ");
        sb.append(System.lineSeparator());
        sb.append("\' METERS    \'  1.0  ");
        sb.append(System.lineSeparator());
        sb.append("\'UTMY \'  0.0 ");
        sb.append(System.lineSeparator());

        int numberBuildings = buildings.size();
        sb.append(numberBuildings).append(System.lineSeparator());
        for (int i = 0; i < numberBuildings; i++) {
            Building build = buildings.get(i);
            String inputLine = "\'Build" + i + "\' " + "1 " + build.getElevation();
            sb.append(inputLine).append(System.lineSeparator());
            LinearRing base = build.getFootprint();
            String originalSrid = "EPSG:" + base.getSRID();
            inputLine = base.getNumPoints() + " " + build.getHeight();
            sb.append(inputLine).append(System.lineSeparator());

            Coordinate[] baseCoords = base.getCoordinates();
            for (Coordinate coord : baseCoords) {
                double[] xyOriginal = { coord.x, coord.y };
                double[] xyTransformed = CRSTransformer.transform(originalSrid, "EPSG:" + simulationSrid, xyOriginal);
                inputLine = xyTransformed[0] + " " + xyTransformed[1];
                sb.append(inputLine).append(System.lineSeparator());
            }
        }

        int numberSources = pointSources.size();
        sb.append(numberSources).append(System.lineSeparator());
        for (int i = 0; i < numberSources; i++) {
            PointSource ps = pointSources.get(i);
            String originalSrid = "EPSG:" + ps.getLocation().getSRID();
            double[] xyOriginal = { ps.getLocation().getX(), ps.getLocation().getY() };
            double[] xyTransformed = CRSTransformer.transform(originalSrid, "EPSG:" + simulationSrid, xyOriginal);
            String inputLine = "\'S" + i + "\'" + " " + ps.getElevation() + " " +
                    ps.getHeight() + " " + xyTransformed[0] + " " + xyTransformed[1];
            sb.append(inputLine).append(System.lineSeparator());
        }
        writeToFile(bpipprmDirectory.resolve("bpipprm.inp"), sb.toString());
    }

    public void runBPIPPRM() {
        try {
            Process process = Runtime.getRuntime().exec(
                    new String[] { EnvConfig.BPIPPRM_EXE, "bpipprm.inp", "building.dat", "buildings_summary.dat" },
                    null, bpipprmDirectory.toFile());

            process.waitFor();

            BufferedReader stdInput = new BufferedReader(new InputStreamReader(process.getInputStream()));

            BufferedReader stdError = new BufferedReader(new InputStreamReader(process.getErrorStream()));

            // Read the output from the command
            LOGGER.info("Here is the standard output of BPIPPRM:");
            String s = null;
            while ((s = stdInput.readLine()) != null) {
                LOGGER.info(s);
            }

            // Read any errors from the attempted command
            LOGGER.info("Here is the standard error of BPIPPRM (if any):");
            while ((s = stdError.readLine()) != null) {
                LOGGER.info(s);
            }
        } catch (IOException e) {
            String errmsg = "Error running bpipprm";
            LOGGER.error(e.getMessage());
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg, e);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            String errmsg = "Error running bpipprm";
            LOGGER.error(e.getMessage());
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg, e);
        }
    }

    public void createAERMODBuildingsInput(boolean createEmptyFile) {
        StringBuilder sb = new StringBuilder();

        if (createEmptyFile) {
            sb.append(" ");
        } else {
            Path filepath = bpipprmDirectory.resolve("building.dat");

            try (BufferedReader reader = new BufferedReader(new FileReader(filepath.toString()));) {
                String line = reader.readLine();
                while (line != null) {
                    line = line.stripLeading();
                    if (line.length() > 2 && line.substring(0, 2).equals("SO"))
                        sb.append(line).append(System.lineSeparator());
                    line = reader.readLine();
                }
            } catch (IOException e) {
                String errmsg = "Error creating buildings input";
                LOGGER.error(e.getMessage());
                LOGGER.error(errmsg);
                throw new RuntimeException(errmsg, e);
            }
        }

        writeToFile(bpipprmDirectory.resolve("buildings.dat"), sb.toString());
    }

    void createStaticPointSourcesLayer(List<StaticPointSource> pointSources, long simulationTime,
            String derivationIri) {
        JSONObject featureCollection = new JSONObject();
        featureCollection.put("type", "FeatureCollection");
        JSONArray features = new JSONArray();

        pointSources.forEach(pointSource -> {
            String originalSrid = "EPSG:" + pointSource.getLocation().getSRID();
            double[] xyOriginal = { pointSource.getLocation().getX(), pointSource.getLocation().getY() };
            double[] xyTransformed = CRSTransformer.transform(originalSrid, "EPSG:4326", xyOriginal);

            JSONObject geometry = new JSONObject();
            geometry.put("type", "Point");
            geometry.put("coordinates", new JSONArray().put(xyTransformed[0]).put(xyTransformed[1]));
            JSONObject feature = new JSONObject();
            feature.put("type", "Feature");
            feature.put("geometry", geometry);

            JSONObject properties = new JSONObject();
            properties.put("iri", pointSource.getIri());
            properties.put("time", simulationTime);
            properties.put("derivation", derivationIri);

            if (pointSource.getLabel() != null) {
                properties.put("name", pointSource.getLabel());
            } else {
                properties.put("name", "Static point source");
            }

            feature.put("properties", properties);

            features.put(feature);
        });

        featureCollection.put("features", features);

        GDALClient gdalClient = GDALClient.getInstance();
        GeoServerClient geoServerClient = GeoServerClient.getInstance();

        gdalClient.uploadVectorStringToPostGIS(EnvConfig.DATABASE, EnvConfig.STATIC_SOURCE_TABLE,
                featureCollection.toString(), new Ogr2OgrOptions(), true);
        geoServerClient.createWorkspace(EnvConfig.GEOSERVER_WORKSPACE);
        geoServerClient.createPostGISLayer(EnvConfig.GEOSERVER_WORKSPACE, EnvConfig.DATABASE,
                EnvConfig.STATIC_SOURCE_TABLE, new GeoServerVectorSettings());
    }

    public void createAERMODReceptorInput(Polygon scope, int nx, int ny, int simulationSrid) {
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

        StringBuilder sb = new StringBuilder("RE GRIDCART POL1 STA ");
        sb.append(System.lineSeparator());
        String rec = String.format("                 XYINC %f %d %f %f %d %f", xlo, nx, dx, ylo, ny, dy);
        sb.append(rec).append(System.lineSeparator());
        sb.append("RE GRIDCART POL1 END ").append(System.lineSeparator());

        writeToFile(aermodDirectory.resolve("receptor.dat"), sb.toString());
    }

    public void createAERMODReceptorInput(Polygon scope, int nx, int ny, int simulationSrid,
            List<List<Double>> elevationValues) {
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

        StringBuilder sb = new StringBuilder("RE GRIDCART POL1 STA ");
        sb.append(System.lineSeparator());
        String rec = String.format("                 XYINC %f %d %f %f %d %f", xlo, nx, dx, ylo, ny, dy);
        sb.append(rec).append(System.lineSeparator());

        for (int i = 0; i < elevationValues.size(); i++) {
            List<String> rowInString = elevationValues.get(i).stream().map(v -> String.format("%.1f", v))
                    .collect(Collectors.toList());
            for (int j = 0; j < rowInString.size(); j += 6) {
                List<String> splittedRow;
                if (j + 6 > rowInString.size()) {
                    splittedRow = rowInString.subList(j, rowInString.size());
                } else {
                    splittedRow = rowInString.subList(j, j + 6);
                }
                String elev = String.format("                 ELEV %d %s", i + 1, String.join(" ", splittedRow));
                String hill = String.format("                 HILL %d %s", i + 1, String.join(" ", splittedRow));
                sb.append(elev).append(System.lineSeparator());
                sb.append(hill).append(System.lineSeparator());
            }
        }

        sb.append("RE GRIDCART POL1 END ").append(System.lineSeparator());

        writeToFile(aermodDirectory.resolve("receptor.dat"), sb.toString());
    }

    String addLeadingZero(String variable, int length) {
        while (variable.length() < length) {
            if (variable.startsWith("-")) {
                variable = "-" + 0 + variable.substring(1, variable.length());
            } else {
                variable = "0" + variable;
            }
        }
        return variable;
    }

    void create144File(WeatherData weatherData) {
        String windSpeed = String.valueOf(weatherData.getWindSpeedInKnots());
        windSpeed = addLeadingZero(windSpeed, 2);
        if (windSpeed.length() != 2) {
            String errmsg = "Invalid wind speed value " + windSpeed;
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg);
        }

        String windDirection = String.valueOf(weatherData.getWindDirectionInTensOfDegrees());
        windDirection = addLeadingZero(windDirection, 2);
        if (windDirection.length() != 2) {
            String errmsg = "Invalid wind direction value " + windDirection;
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg);
        }

        // unsure how strict the format is, just follow blindly at the moment
        String temperature = String.valueOf(weatherData.getTemperatureInFahrenheit());
        temperature = addLeadingZero(temperature, 3);
        if (temperature.length() < 3) {
            temperature = "0" + temperature;
        }
        if (temperature.length() != 3) {
            String errmsg = "Invalid temperature value " + temperature;
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg);
        }

        String humidity = String.valueOf(weatherData.getHumidityAsPercentage());
        humidity = addLeadingZero(humidity, 3);
        if (humidity.length() != 3) {
            String errmsg = "Invalid humidity value " + humidity;
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg);
        }

        String cloudCover = String.valueOf(weatherData.getCloudCoverAsInteger());
        if (cloudCover.length() != 1) {
            String errmsg = "Invalid cloud cover value " + cloudCover;
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg);
        }

        String templateContent;
        try (InputStream inputStream = getClass().getClassLoader().getResourceAsStream("weather_template.144")) {
            templateContent = IOUtils.toString(inputStream, StandardCharsets.UTF_8);

            templateContent = templateContent.replace("WS", windSpeed).replace("WD", windDirection)
                    .replace("TEM", temperature)
                    .replace("HUM", humidity).replace("C", cloudCover);
        } catch (IOException e) {
            String errmsg = "Failed to read weather_template.144 file";
            LOGGER.error(e.getMessage());
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg, e);
        }

        writeToFile(aermetDirectory.resolve("weather_template.144"), templateContent);
    }

    public void createAermetInput(Polygon scope) {
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
        // Get AERMET template
        String templateContent;
        try (InputStream is = getClass().getClassLoader().getResourceAsStream(AERMET_INPUT)) {
            templateContent = IOUtils.toString(is, StandardCharsets.UTF_8);
            templateContent = templateContent.replace("REPLACED_1", locationOffset);
            templateContent = templateContent.replace("REPLACED_2", location);
        } catch (IOException e) {
            String errmsg = "Failed to copy " + AERMET_INPUT;
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed to copy {}", AERMET_INPUT);
            throw new RuntimeException(errmsg, e);
        }

        writeToFile(aermetDirectory.resolve(AERMET_INPUT), templateContent);
    }

    void runAermet(Polygon scope) {
        createAermetInput(scope);

        try (InputStream is = getClass().getClassLoader().getResourceAsStream("raob_soundings15747.FSL")) {
            Files.copy(is, aermetDirectory.resolve("raob_soundings15747.FSL"));
        } catch (IOException e) {
            String errmsg = "Failed to copy raob_soundings15747.FSL";
            LOGGER.error(e.getMessage());
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg, e);
        }

        try {
            Process process = Runtime.getRuntime().exec(new String[] { EnvConfig.AERMET_EXE, AERMET_INPUT }, null,
                    aermetDirectory.toFile());
            process.waitFor();

            BufferedReader stdInput = new BufferedReader(new InputStreamReader(process.getInputStream()));

            BufferedReader stdError = new BufferedReader(new InputStreamReader(process.getErrorStream()));

            // Read the output from the command
            LOGGER.info("Here is the standard output of AERMET:");
            String s = null;
            while ((s = stdInput.readLine()) != null) {
                LOGGER.info(s);
            }

            // Read any errors from the attempted command
            LOGGER.info("Here is the standard error of AERMET (if any):");
            while ((s = stdError.readLine()) != null) {
                LOGGER.info(s);
            }

        } catch (IOException e) {
            String errmsg = "Error executing aermet";
            LOGGER.error(errmsg);
            LOGGER.error(e.getMessage());
            throw new RuntimeException(errmsg, e);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            String errmsg = "Error executing aermet";
            LOGGER.error(errmsg);
            LOGGER.error(e.getMessage());
            throw new RuntimeException(errmsg, e);
        }

        // check if outputs are generated
        File surfaceFile = aermetDirectory.resolve("AERMET_SURF.SFC").toFile();
        File upperFile = aermetDirectory.resolve("AERMET_UPPER.PFL").toFile();

        // Even if the output files were generated, they may be blank if AERMET did not
        // complete successfully.
        if (surfaceFile.exists() && surfaceFile.length() > 0 && upperFile.exists() && upperFile.length() > 0) {
            LOGGER.info("AERMET has completed successfully. ");
        } else {
            String errmsg = "aermet may not have completed successfully";
            LOGGER.error("aermet may not have completed successfully");
            throw new RuntimeException(errmsg);
        }
    }

    void createPointsFile(List<PointSource> pointSources, int simulationSrid, PollutantType pollutantType, int z) {
        StringBuilder sb = new StringBuilder();

        for (int i = 0; i < pointSources.size(); i++) {
            PointSource ps = pointSources.get(i);
            String stkId = "S" + i;

            String originalSrid = "EPSG:" + ps.getLocation().getSRID();
            double[] xyOriginal = { ps.getLocation().getX(), ps.getLocation().getY() };
            double[] xyTransformed = CRSTransformer.transform(originalSrid, "EPSG:" + simulationSrid, xyOriginal);

            double area = Math.PI * Math.pow(ps.getDiameter() / 2, 2); // m2
            double density = ps.getMixtureDensityInKgm3(); // kg/m3

            // for particles, assumed to be part of the air. Pollutant stream is part of a
            // much larger air stream
            double velocity = ps.getMixtureMassFlux() / EnvConfig.TARGET_EMISSION_VOLUME_FRACTION / area / density; // m/s

            double baseElevation = 0.0;
            if (ps.getClass() == StaticPointSource.class) {
                StaticPointSource pss = (StaticPointSource) ps;
                baseElevation = pss.getElevation();
            }

            sb.append(String.format("SO LOCATION %s POINT %f %f %f", stkId, xyTransformed[0], xyTransformed[1],
                    baseElevation));
            sb.append(System.lineSeparator());
            sb.append(String.format("SO SRCPARAM %s %f %f %f %f %f", stkId,
                    ps.getFlowrateInGramsPerS(pollutantType), ps.getHeight(), ps.getMixtureTemperatureInKelvin(),
                    velocity, ps.getDiameter()));
            sb.append(System.lineSeparator());
        }

        writeToFile(
                aermodDirectory.resolve(Pollutant.getPollutantLabel(pollutantType)).resolve(String.valueOf(z))
                        .resolve("points.so"),
                sb.toString());
    }

    private void writeToFile(Path path, String content) {
        try (BufferedWriter writer = Files.newBufferedWriter(path)) {
            LOGGER.info("Writing file: {}", path);
            writer.write(content);
        } catch (IOException e) {
            String errmsg = "Failed to write " + path.getFileName();
            LOGGER.error(errmsg);
            LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

    void runAermod(PollutantType pollutantType, int z) {
        String aermodInputFile = "aermod.inp";
        // modify height in aermod input file
        String templateContent;
        try (InputStream is = getClass().getClassLoader().getResourceAsStream(aermodInputFile)) {
            templateContent = IOUtils.toString(is, StandardCharsets.UTF_8);
            templateContent = templateContent.replace("FLAGPOLE 0.0", String.format("FLAGPOLE %d", z));
        } catch (IOException e) {
            String errmsg = "Failed to create aermod.inp";
            LOGGER.error(e.getMessage());
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg, e);
        }
        writeToFile(aermodDirectory.resolve(Pollutant.getPollutantLabel(pollutantType)).resolve(String.valueOf(z))
                .resolve(aermodInputFile), templateContent);

        try {
            Process process = Runtime.getRuntime().exec(new String[] { EnvConfig.AERMOD_EXE, aermodInputFile }, null,
                    aermodDirectory.resolve(Pollutant.getPollutantLabel(pollutantType)).resolve(String.valueOf(z))
                            .toFile());
            process.waitFor();

            BufferedReader stdInput = new BufferedReader(new InputStreamReader(process.getInputStream()));

            BufferedReader stdError = new BufferedReader(new InputStreamReader(process.getErrorStream()));

            // Read the output from the command
            LOGGER.info("Here is the standard output of AERMOD:");
            String s = null;
            while ((s = stdInput.readLine()) != null) {
                LOGGER.info(s);
            }

            // Read any errors from the attempted command
            LOGGER.info("Here is the standard error of AERMOD (if any):");
            while ((s = stdError.readLine()) != null) {
                LOGGER.info(s);
            }
        } catch (IOException e) {
            String errmsg = String.format("Error executing aermod for pollutant: %s and height: %f",
                    Pollutant.getPollutantLabel(pollutantType), z);
            LOGGER.error(errmsg);
            LOGGER.error(e.getMessage());
            throw new RuntimeException(errmsg, e);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            String errmsg = String.format("Error executing aermod for pollutant: %s and height: %f",
                    Pollutant.getPollutantLabel(pollutantType), z);
            LOGGER.error(errmsg);
            LOGGER.error(e.getMessage());
            throw new RuntimeException(errmsg, e);
        }
    }

    String uploadToFileServer(Path file) {
        // upload to file server via HTTP POST
        MultipartEntityBuilder multipartBuilder = MultipartEntityBuilder.create();
        multipartBuilder.addPart("dispersionMatrix", new FileBody(file.toFile()));

        HttpPost httpPost = new HttpPost(
                EnvConfig.FILE_SERVER + file.subpath(3, file.getNameCount() - 1).toFile() + "/");
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
            String errmsg = "File upload failed";
            LOGGER.error(errmsg);
            LOGGER.error(e.getMessage());
            throw new RuntimeException(errmsg, e);
        }
    }

    void createFileForRaster(String rasterFileName, PollutantType pollutantType, int z)
            throws FileNotFoundException {
        File concFile = aermodDirectory.resolve(Pollutant.getPollutantLabel(pollutantType)).resolve(String.valueOf(z))
                .resolve("averageConcentration.dat").toFile();
        List<String[]> xyzList = new ArrayList<>();

        try (Scanner scanner = new Scanner(concFile)) {
            // skip first 8 lines of comments/header
            for (int i = 0; i < 8; i++)
                scanner.nextLine();

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                String[] separatedData = line.substring(2).split("\\s+");
                String[] xyz = new String[3];
                xyz[0] = separatedData[0];
                xyz[1] = separatedData[1];
                xyz[2] = separatedData[2];
                xyzList.add(xyz);
            }
        }

        // write to raster folder
        StringBuilder sb = new StringBuilder();
        xyzList.forEach(xyz -> sb.append(String.join(" ", xyz)).append(System.lineSeparator()));
        String fileContent = sb.toString();

        writeToFile(rasterDirectory.resolve(rasterFileName), fileContent);
    }

    void uploadRasterToPostGIS(int simSrid, boolean append) {
        GDALClient gdalClient = GDALClient.getInstance();
        GDALTranslateOptions gdalTranslateOptions = new GDALTranslateOptions();
        gdalTranslateOptions.setSridIn("EPSG:" + simSrid);
        gdalClient.uploadRasterFilesToPostGIS(EnvConfig.DATABASE, "public", EnvConfig.DISPERSION_RASTER_TABLE,
                rasterDirectory.toString(), gdalTranslateOptions, append);
    }

    /**
     * executes get request from python-service to postprocess
     */
    public JSONObject getGeoJsonAndColourbar(String endPoint, String outputFileURL, int srid) {
        URI httpGet;
        try {
            URIBuilder builder = new URIBuilder(endPoint);
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

    void createSimulationSubDirectory(PollutantType pollutantType, int z) {
        aermodDirectory.resolve(Pollutant.getPollutantLabel(pollutantType)).resolve(String.valueOf(z)).toFile()
                .mkdirs();
    }

    void createDispersionLayer(JSONObject geoJSON, String derivationIri, PollutantType pollutantType,
            long simulationTime, int zValue) {
        JSONArray features = geoJSON.getJSONArray("features");
        for (int j = 0; j < features.length(); j++) {
            JSONObject feature = features.getJSONObject(j);
            JSONObject featureProperties = feature.getJSONObject("properties");
            featureProperties.put("derivation", derivationIri);
            featureProperties.put("pollutant", Pollutant.getPollutantIri(pollutantType));
            featureProperties.put("time", simulationTime);
            String uuid = UUID.randomUUID().toString();
            featureProperties.put("iri", QueryClient.PREFIX_DISP + uuid); // for geoserver
            featureProperties.put("uuid", uuid); // for ontop to construct IRI
            featureProperties.put("z", zValue);
        }

        GDALClient gdalClient = GDALClient.getInstance();
        gdalClient.uploadVectorStringToPostGIS(EnvConfig.DATABASE, EnvConfig.DISPERSION_CONTOURS_TABLE,
                geoJSON.toString(), new Ogr2OgrOptions(), true); // true = append

        GeoServerClient geoServerClient = GeoServerClient.getInstance();
        GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();

        UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
        virtualTable.setSql(String.format("select *, title as name from %s", EnvConfig.DISPERSION_CONTOURS_TABLE));
        virtualTable.setEscapeSql(true);
        virtualTable.setName("dispersion");
        virtualTable.addVirtualTableGeometry("wkb_geometry", "MultiPolygon", "4326");
        geoServerVectorSettings.setVirtualTable(virtualTable);

        geoServerClient.createPostGISLayer(EnvConfig.GEOSERVER_WORKSPACE, EnvConfig.DATABASE,
                EnvConfig.DISPERSION_CONTOURS_TABLE, geoServerVectorSettings);
    }

    void createElevationLayer(JSONObject geoJSON, String derivationIri) {
        JSONArray features = geoJSON.getJSONArray("features");
        for (int j = 0; j < features.length(); j++) {
            JSONObject feature = features.getJSONObject(j);
            JSONObject featureProperties = feature.getJSONObject("properties");
            featureProperties.put("derivation", derivationIri);
        }

        GDALClient gdalClient = GDALClient.getInstance();
        gdalClient.uploadVectorStringToPostGIS(EnvConfig.DATABASE, EnvConfig.ELEVATION_CONTOURS_TABLE,
                geoJSON.toString(), new Ogr2OgrOptions(), true); // true = append

        GeoServerClient geoServerClient = GeoServerClient.getInstance();
        GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();

        UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
        virtualTable.setSql(String.format("select *, title as name from %s", EnvConfig.ELEVATION_CONTOURS_TABLE));
        virtualTable.setEscapeSql(true);
        virtualTable.setName("elevation");
        virtualTable.addVirtualTableGeometry("wkb_geometry", "MultiPolygon", "4326");
        geoServerVectorSettings.setVirtualTable(virtualTable);

        geoServerClient.createPostGISLayer(EnvConfig.GEOSERVER_WORKSPACE, EnvConfig.DATABASE,
                EnvConfig.ELEVATION_CONTOURS_TABLE, geoServerVectorSettings);
    }
}
