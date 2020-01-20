/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.integrator;

import org.apache.commons.math.ConvergenceException;
import org.apache.commons.math.FunctionEvaluationException;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.integration.UnivariateRealIntegrator;
import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.math.function.FunctionCalculationException;

/**
 *
 * @author pb556
 */
public class RombergIntegrator {

    private Function<Double, Double> f = null;
    private double a = 0.0;
    private double b = 0.0;
    private int n = 200;

    public void setFunction(Function<Double, Double> f) {
        this.f = f;
    }

    public void setLimit(double a, double b) {
        this.a = a;
        this.b = b;
    }

    public double getIntegral() throws FunctionCalculationException, ConvergenceException, FunctionEvaluationException {
        if ((a < b) && (f != null) && (n > 1)) {
            UnivariateRealFunction function = new WrapperFunction();
            org.apache.commons.math.analysis.integration.RombergIntegrator integrator = new org.apache.commons.math.analysis.integration.RombergIntegrator();
            //integrator.setMaximalIterationCount(31);

            //integrator.setMinimalIterationCount(15);
            //System.out.println(integrator.getMaximalIterationCount());
            //System.out.println(integrator.getMinimalIterationCount());
            double val = integrator.integrate(function, a, b);
            return val;
            //System.out.println("rel. accuracy: " + integrator.getRelativeAccuracy());
            //System.out.println("abs. accuracy: " + integrator.getAbsoluteAccuracy());

            //System.out.println(trapezoid(a, b, 1000000));
            //System.out.println(adaptive(a, b));

            //return adaptive(a, b);
        } else {
            return 0;
        }
    }
    private final static double EPSILON = 1E-30;

    /**
     * ********************************************************************
     * Standard normal distribution density function. Replace with any
     * sufficiently smooth function.
     * ********************************************************************
     */
    double fun(double x) {
        //return Math.exp(-x * x / 2) / Math.sqrt(2 * Math.PI);
        try {
            return f.f(x);
        } catch (FunctionCalculationException ex) {
        }
        return 0.0;
    }

    // trapezoid rule
    double trapezoid(double a, double b, int N) {
        double h = (b - a) / N;
        double sum = 0.5 * h * (fun(a) + fun(b));
        for (int k = 1; k < N; k++) {
            sum = sum + h * fun(a + h * k);
        }
        return sum;
    }

    // adaptive quadrature
    public double adaptive(double a, double b) {
        double h = b - a;
        double c = (a + b) / 2.0;
        double d = (a + c) / 2.0;
        double e = (b + c) / 2.0;
        double Q1 = h / 6 * (fun(a) + 4 * fun(c) + fun(b));
        double Q2 = h / 12 * (fun(a) + 4 * fun(d) + 2 * fun(c) + 4 * fun(e) + fun(b));
        if (Math.abs(Q2 - Q1) <= EPSILON) {
            return Q2 + (Q2 - Q1) / 15;
        } else {
            return adaptive(a, c) + adaptive(c, b);
        }
    }

    private class WrapperFunction implements UnivariateRealFunction {

        @Override
        public double value(double d) throws FunctionEvaluationException {
            try {
                return f.f(d);
            } catch (FunctionCalculationException ex) {
                throw new FunctionEvaluationException(ex, null);
            }
        }
    }
}
