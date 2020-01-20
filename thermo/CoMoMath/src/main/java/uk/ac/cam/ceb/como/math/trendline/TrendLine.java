/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.trendline;

import java.util.Arrays;
import org.apache.commons.math.linear.MatrixUtils;
import org.apache.commons.math.stat.regression.OLSMultipleLinearRegression;
import org.xmlcml.euclid.RealMatrix;

/**
 *
 * @author pb556
 */
public abstract class TrendLine implements TrendLineIntf {
    
    RealMatrix coef = null; // will hold prediction coefs once we get values

    protected abstract double[] xVector(double x); // create vector of values from x
    protected abstract boolean logY(); // set true to predict log of y (note: y must be positive)

    @Override
    public void setValues(double[] y, double[] x) {
        if (x.length != y.length) {
            throw new IllegalArgumentException(String.format("The numbers of y and x values must be equal (%d != %d)",y.length,x.length));
        }
        double[][] xData = new double[x.length][]; 
        for (int i = 0; i < x.length; i++) {
            // the implementation determines how to produce a vector of predictors from a single x
            xData[i] = xVector(x[i]);
        }
        if(logY()) { // in some models we are predicting ln y, so we replace each y with ln y
            y = Arrays.copyOf(y, y.length); // user might not be finished with the array we were given
            for (int i = 0; i < x.length; i++) {
                y[i] = Math.log(y[i]);
            }
        }
        OLSMultipleLinearRegression ols = new OLSMultipleLinearRegression();
        ols.setNoIntercept(true); // let the implementation include a constant in xVector if desired
        ols.newSampleData(y, xData); // provide the data to the model
        coef = (RealMatrix) MatrixUtils.createColumnRealMatrix(ols.estimateRegressionParameters()); // get our coefs
    }

    @Override
    public double predict(double x) {
//        double yhat = coef.preMultiply(xVector(x))[0]; // apply coefs to xVector
//        if (logY()) yhat = (Math.exp(yhat)); // if we predicted ln y, we still need to get y
//        return yhat;
        return 0.0;
    }
}
