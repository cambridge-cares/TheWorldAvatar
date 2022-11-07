package com.cmclinnovations.aermod;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.io.IOUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.locationtech.jts.geom.Geometry;

import com.cmclinnovations.aermod.objects.Ship;
import com.cmclinnovations.aermod.objects.WeatherData;

import uk.ac.cam.cares.jps.base.util.CRSTransformer;

public class Aermod {
    private static final Logger LOGGER = LogManager.getLogger(Aermod.class);
    private Path simulationDirectory;
    private Path aermetDirectory;
    private Path aermodDirectory;
    private static final String AERMET_INPUT = "aermet_input.inp";

    public Aermod(Path simulationDirectory) {
        LOGGER.info("Creating directory for simulation files");
        this.simulationDirectory = simulationDirectory;
        simulationDirectory.toFile().mkdirs();

        this.aermetDirectory = simulationDirectory.resolve("aermet");
        aermetDirectory.toFile().mkdir();

        this.aermodDirectory = simulationDirectory.resolve("aermod");
        aermodDirectory.toFile().mkdir();
    }

    int create144File(WeatherData weatherData) {
        String windSpeed = String.valueOf(weatherData.getWindSpeedInKnots());
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

    int createPointsFile(List<Ship> ships, String simulationSrid) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < ships.size(); i++) {
            Ship ship = ships.get(i);
            String stkId = "S" + i;

            String originalSrid = "EPSG:" + ship.getLocation().getSrid();
            double[] xyOriginal = {ship.getLocation().getX(), ship.getLocation().getY()};
            double[] xyTransformed = CRSTransformer.transform(originalSrid, simulationSrid, xyOriginal);

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

    public int createAermodInputFile(Geometry scope, int nx, int ny, String srid) {
        List<Double> xDoubles = new ArrayList<>();
        List<Double> yDoubles = new ArrayList<>();

        String originalSRID = "EPSG:" + scope.getSRID();
        
        for (int i = 0; i < scope.getCoordinates().length; i++) {
            double[] xyTransformed = CRSTransformer.transform(originalSRID, srid, scope.getCoordinates()[i].x, scope.getCoordinates()[i].y);

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
}
