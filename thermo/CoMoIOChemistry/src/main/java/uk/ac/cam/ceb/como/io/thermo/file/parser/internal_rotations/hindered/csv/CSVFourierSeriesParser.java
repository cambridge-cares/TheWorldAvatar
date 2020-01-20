/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.thermo.file.parser.internal_rotations.hindered.csv;

import com.cmclinnovations.io.file.parser.FileParser;
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.log4j.Logger;

import uk.ac.cam.ceb.como.math.fourier.series.FourierSeries;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesCoefficient;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesCoefficients;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesLimit;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesLimits;
import uk.ac.cam.ceb.como.math.fourier.series.onedimensional.classical.ClassicalOneDimensionalFourierSeries;

/**
 *
 * @author pb556
 */

public class CSVFourierSeriesParser extends FileParser<Map<String, FourierSeries>> {

    private final Map<String, FourierSeries> series = new HashMap<>();
    private final Logger logger = Logger.getLogger(this.getClass());

    @Override
    public void parse() throws Exception {
            if (getFile() == null) {
                logger.error("No input file defined.");
                return;
            }
        try {
            BufferedReader br = new BufferedReader(new FileReader(getFile()));
            String separator = ",";
            String line;
            int ctr = 1;
            while ((line = br.readLine()) != null) {
                logger.info("Processing line " + ctr);
                if (line.isEmpty()) {
                    ctr++;
                    continue;
                }
                // parse line
                String[] items = line.split(separator);
                if (items.length == 5) {
                    try {
                        String id = items[0].trim();
                        int order = Integer.parseInt(items[1].trim());
                        FourierSeriesLimits lowerLimits = parseLimits(getItems(items[2].trim()));
                        FourierSeriesLimits upperLimits = parseLimits(getItems(items[3].trim()));
                        FourierSeriesCoefficients coeffs = parseFourierCoefficients(getItems(items[4].trim()));
                        FourierSeries fs = produceFourierSeriesObject(order, lowerLimits, upperLimits, coeffs);
                        Map<Integer, FourierSeriesCoefficients> coefficients = fs.getParameters().getFourierSeriesCoefficients().getFourierCoefficientsByDimension();
                        boolean valid = true;
                        for (Integer dim : coefficients.keySet()) {
                            // check for each dimension!!!
                            if (order != coefficients.get(dim).size()) {
                                valid = false;
                                break;
                            }
                        }
                        if (!valid) {
                            logger.warn("Missing coefficients in line " + ctr + "!");
                        } else {
                            series.put(id, fs);
                        }
                    } catch (Exception e) {
                        logger.warn("Error in parsing line " + ctr + "! " + e.getMessage());
                    }
                } else {
                    logger.warn("Line " + ctr + " cannot be parsed: " + line);
                }
                ctr++;
            }
        } catch (Exception ex) {
            logger.error("File could not be read.");
        }
    }

    private FourierSeries produceFourierSeriesObject(int order, FourierSeriesLimits lowerLimits, FourierSeriesLimits upperLimits, FourierSeriesCoefficients coeffs) throws Exception {
        return new ClassicalOneDimensionalFourierSeries(order, coeffs, lowerLimits, upperLimits);
    }

    private List<String> getItems(String str) {
        Pattern p = Pattern.compile("\\[(.*?)\\]");
        Matcher m = p.matcher(str);
        List<String> data = new ArrayList<>();
        while (m.find()) {
            data.add(m.group().replace("[", "").replace("]", "").trim());
        }
        return data;
    }

    private FourierSeriesLimits parseLimits(List<String> str) {
        FourierSeriesLimits limits = new FourierSeriesLimits();
        for (String s : str) {
            String[] items = s.split(";");
            if (items.length == 2) {
                try {
                    limits.add(new FourierSeriesLimit(Integer.parseInt(items[0]), Double.parseDouble(items[1])));
                } catch (NumberFormatException nfe) {
                    logger.warn("Invalid limit definition '" + s + "' identified (NumberFormatException)!");
                    return null;
                }
            } else {
                logger.warn("Invalid limit definition '" + s + "' identified!");
                return null;
            }
        }
        return limits;
    }

    private FourierSeriesCoefficients parseFourierCoefficients(List<String> str) {
        FourierSeriesCoefficients coeffs = new FourierSeriesCoefficients();
        for (String s : str) {
            String[] items = s.split(";");
            if (items.length >= 3) {
                try {
                    int dim = Integer.parseInt(items[0]);
                    int index = Integer.parseInt(items[1]);
                    double[] values = new double[items.length - 2];
                    for (int i = 2; i < items.length; i++) {
                        values[i - 2] = Double.parseDouble(items[i].replace("{", "").replace("}", ""));
                    }
                    coeffs.add(new FourierSeriesCoefficient(index, dim, values));
                } catch (NumberFormatException nfe) {
                    logger.warn("Invalid coefficient definition '" + s + "' identified (NumberFormatException)!");
                    return null;
                }
            } else {
                logger.warn("Invalid coefficient definition '" + s + "' identified!");
                return null;
            }
        }
        return coeffs;
    }

    @Override
    public void clear() throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Map<String, FourierSeries> get() throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
