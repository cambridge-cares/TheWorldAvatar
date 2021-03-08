/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.thermo.file.parser.internal_rotations.hindered.csv;

import com.cmclinnovations.io.file.writer.FileWriter;
import com.cmclinnovations.io.writer.ExtendedWriterIntf;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import org.apache.log4j.Logger;

import uk.ac.cam.ceb.como.math.fourier.series.FourierSeries;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesCoefficient;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesCoefficients;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesLimit;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesLimits;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesParameters;
import uk.ac.cam.ceb.como.tools.file.writer.StringListWriter;
import uk.ac.cam.ceb.como.tools.file.writer.StringWriter;

/**
 *
 * @author pb556
 */

public class CSVFourierSeriesWriter extends FileWriter<Map<String, FourierSeries>> implements ExtendedWriterIntf<Map<String, FourierSeries>> {

    // period (lower and upper limit)
    // coefficients
    // optional: data points used for fitting
    private final Logger logger = Logger.getLogger(this.getClass());

    //private Map<String, FourierSeries> series = null;
    @Override
    public void write() throws Exception {
        if (getFile() == null) {
            logger.error("No output file defined.");
            return;
        }
        ArrayList<String> lines = new ArrayList<>();
        for (String id : getContent().keySet()) {
            lines.add(toString(id, getContent().get(id)));
        }
        StringListWriter writer = new StringListWriter();
        writer.setContent(lines);
        writer.overwrite(true);
        writer.set(getFile());
        writer.write();
    }

    @Override
    public void append() throws Exception {
        append(getContent());
    }

    public void append(Map<String, FourierSeries> series) throws Exception {
        for (String id : series.keySet()) {
            append(id, series.get(id));
        }
    }

    public void append(String id, FourierSeries series) throws Exception {
        StringWriter writer = new StringWriter();
        writer.overwrite(true);
        writer.set(getFile());
        if (getFile().exists()) {
            writer.setContent(System.getProperty("line.separator") + toString(id, series));
            writer.append();
        } else {
            writer.setContent(toString(id, series));
            writer.write();
        }
    }

    private String toString(String id, FourierSeries series) {
        String separator = ", ";
        FourierSeriesParameters parameters = series.getParameters();
        String lowerLimit = getLimit(parameters.getLowerLimits().getLimitsByDimension());
        String upperLimit = getLimit(parameters.getUpperLimits().getLimitsByDimension());
        String strCoefficients = "";
        FourierSeriesCoefficients allCoeffs = parameters.getFourierSeriesCoefficients();
        Map<Integer, FourierSeriesCoefficients> coeffsDim = allCoeffs.getFourierCoefficientsByDimension();
        for (Integer dim : coeffsDim.keySet()) {
            FourierSeriesCoefficients coeffs = coeffsDim.get(dim);
            for (FourierSeriesCoefficient c : coeffs) {
                double[] val = c.getValue();
                String strVal = "";
                if (val.length > 0) {
                    strVal += val[0];
                    for (int i = 1; i < val.length; i++) {
                        strVal += "; " + val[1];
                    }
                }
                strCoefficients += "[" + c.getDimension() + ";" + c.getIndex() + ";{" + strVal + "}]";
            }
        }
        return id + separator
                + parameters.getOrder() + separator
                + lowerLimit + separator
                + upperLimit + separator
                + strCoefficients;
    }

    private String getLimit(Map<Integer, FourierSeriesLimits> limitsByDim) {
        String str = "";
        for (Integer dim : limitsByDim.keySet()) {
            FourierSeriesLimits limits = limitsByDim.get(dim);
            for (FourierSeriesLimit limit : limits) {
                str += "[" + limit.getDimension() + ";" + limit.getLimit() + "]";
            }
        }
        return str;
    }

    @Override
    public void insert(int line) throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void clear() throws Exception {
        setContent(new HashMap<String, FourierSeries>());
    }

    @Override
    public void insert(int pos, int line) throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void delete() throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void delete(int pos) throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void delete(int start, int end) throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
