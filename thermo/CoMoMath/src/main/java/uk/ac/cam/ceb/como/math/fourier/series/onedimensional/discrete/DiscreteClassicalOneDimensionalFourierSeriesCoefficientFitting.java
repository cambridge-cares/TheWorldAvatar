/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.fourier.series.onedimensional.discrete;

import uk.ac.cam.ceb.como.math.data.DataPoint;
import uk.ac.cam.ceb.como.math.data.DataSpace;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesCoefficient;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesCoefficientFitting;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesCoefficientFittingException;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesCoefficients;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesLimits;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesParameters;
import uk.ac.cam.ceb.como.math.fourier.series.onedimensional.OneDimensionalFourierSeriesCoefficientFittingException;

/**
 *
 * @author pb556
 */
public class DiscreteClassicalOneDimensionalFourierSeriesCoefficientFitting extends FourierSeriesCoefficientFitting {

    protected DataSpace space;
    protected int dim = 1;
    
    // get the measurements in!!!
    public DiscreteClassicalOneDimensionalFourierSeriesCoefficientFitting(DataSpace space) throws FourierSeriesCoefficientFittingException {
        super();
        this.space = space;
        validateDataPoints();
    }
    
    public DiscreteClassicalOneDimensionalFourierSeriesCoefficientFitting(DataSpace space, int order) throws FourierSeriesCoefficientFittingException {
        super(order);
        this.space = space;
        validateDataPoints();
    }
    
    public DiscreteClassicalOneDimensionalFourierSeriesCoefficientFitting(DataSpace space, FourierSeriesLimits lower, FourierSeriesLimits upper) throws FourierSeriesCoefficientFittingException {
        super(lower, upper);
        this.space = space;
        validateDataPoints();
    }
    
    public DiscreteClassicalOneDimensionalFourierSeriesCoefficientFitting(DataSpace space, int order, FourierSeriesLimits lower, FourierSeriesLimits upper) throws FourierSeriesCoefficientFittingException {
        super(order, lower, upper);
        this.space = space;
        validateDataPoints();
    }
    
    private void validateDataPoints() throws FourierSeriesCoefficientFittingException {
        for (DataPoint p : space) {
            if(p.getCoordinate().length != dim) {
                throw new OneDimensionalFourierSeriesCoefficientFittingException("Invalid dimensionality.");
            }
        }
    }
    
    @Override
    public void fitFourierSeriesCoefficients() throws FourierSeriesCoefficientFittingException {
        // sort the values accordingly
        
        // used to determine the period in case it is not defined
        double min = Double.MAX_VALUE;
        double max = Double.MIN_VALUE;
        
        double[] coordinate = new double[space.size()];
        double[] value = new double[space.size()];
        for (int i = 0; i < space.size(); i++) {
            coordinate[i] = (Double) space.get(i).getCoordinate()[0];
            if (coordinate[i] < min) {
                min = coordinate[i];
            }
            if (coordinate[i] > max) {
                max = coordinate[i];
            }
            value[i] = (Double) space.get(i).getValue();
        }
        
        // order the array - TEST
        for (int i = 0; i < coordinate.length; i++) {
            for (int j = i + 1; j < coordinate.length; j++) {
                if (coordinate[j] < coordinate[i]) {
                    double k = coordinate[i];
                    coordinate[i] = coordinate[j];
                    coordinate[j] = k;
                    k = value[i];
                    value[i] = value[j];
                    value[j] = k;
                }
            }
        }
        
        double T = upper.getLimitsByDimension(1).get(0).getLimit() - lower.getLimitsByDimension(1).get(0).getLimit();
        
        // sort values
        for (int i = 0; i < coordinate.length; i++) {
            for (int j = i + 1; j < coordinate.length; j++) {
                if (coordinate[i] > coordinate[j]) {
                    double c = coordinate[i];
                    double v = value[i];
                    coordinate[i] = coordinate[j];
                    value[i] = value[j];
                    coordinate[j] = c;
                    value[j] = v;
                }
            }
        }
        
        // used to store the coefficient values
        double nDouble = (double) space.size();
        
        double[] a = new double[order];
        double[] b = new double[order];
        
        for (int i = 0; i < order; i++) {
            a[i] = 0.0;
            b[i] = 0.0;
            double iDouble = (double) i;
            for (int j = 0; j < value.length; j++) {
                double jDouble = (double) j;
                a[i] += value[j] * Math.cos(iDouble * jDouble * T / nDouble);
                b[i] += value[j] * Math.sin(iDouble * jDouble * T / nDouble);
            }
            a[i] = (2.0 * (max - min) * a[i]) / (T * nDouble);
            b[i] = (2.0 * (max - min) * b[i]) / (T * nDouble);
        }
        
        coeff = new FourierSeriesCoefficients();
        
        for (int k = 0; k < order; k++) {
            coeff.add(new FourierSeriesCoefficient(k, 1, new double[]{a[k], b[k]}));
            //this.coeff.addFourierCoefficient(k, a[k], b[k]);
            //System.out.println("a[" + k + "]=" + a[k] + "  b[" + k + "]=" + b[k]);
        }
        
        parameters = new FourierSeriesParameters(order, coeff, lower, upper);
    }
    
//    @Override
//    public void fitFourierCoefficients() throws FourierSeriesCoefficientFittingException {
//        // sort the values accordingly
//        
//        // used to determine the period in case it is not defined
//        double min = Double.MAX_VALUE;
//        double max = Double.MIN_VALUE;
//        
//        double[] coordinate = new double[space.size()];
//        double[] value = new double[space.size()];
//        for (int i = 0; i < space.size(); i++) {
//            coordinate[i] = (Double) space.get(i).getCoordinate()[0];
//            if (coordinate[i] < min) {
//                min = coordinate[i];
//            }
//            if (coordinate[i] > max) {
//                max = coordinate[i];
//            }
//            value[i] = (Double) space.get(i).getValue();
//        }
//        
//        // sort values
//        for (int i = 0; i < coordinate.length; i++) {
//            for (int j = i + 1; j < coordinate.length; j++) {
//                if (coordinate[i] > coordinate[j]) {
//                    double c = coordinate[i];
//                    double v = value[i];
//                    coordinate[i] = coordinate[j];
//                    value[i] = value[j];
//                    coordinate[j] = c;
//                    value[j] = v;
//                }
//            }
//        }
//        
//        // used to store the coefficient values
//        double[] a = new double[order];
//        double[] b = new double[order];
//        
//        for (int i = 0; i < value.length; i++) { // n samples
//            a[0] = a[0] + value[i];
//        }
//        
//        coeff = new FourierSeriesCoefficients();
//        
//        a[0] = a[0]/(value.length - 1);
//        
//        coeff.add(new FourierSeriesCoefficient(0, 1, new double[]{a[0], b[0]}));
//        //this.coeff.addFourierCoefficient(0, a[0], b[0]);
//        System.out.println("a[" + 0 + "]=" + a[0] + "  b[" + 0 + "]=" + b[0]);
//
//        for (int k = 1; k < this.order; k++) {
//            a[k] = 0.0;
//            b[k] = 0.0;
//            
//            for (int i = 0; i < value.length; i++) // n samples
//            {
//                a[k] = a[k] + value[i] * Math.cos((double) k * coordinate[i]);
//                b[k] = b[k] + value[i] * Math.sin((double) k * coordinate[i]);
//            }
//            a[k] = (2.0 / (value.length - 1)) * a[k];
//            b[k] = (2.0 / (value.length - 1)) * b[k];
//            
//            coeff.add(new FourierSeriesCoefficient(k, 1, new double[]{a[k], b[k]}));
//            //this.coeff.addFourierCoefficient(k, a[k], b[k]);
//            System.out.println("a[" + k + "]=" + a[k] + "  b[" + k + "]=" + b[k]);
//        }
//        
//        parameters = new FourierSeriesParameters(order, coeff, lower, upper);
//    }
    
}
