/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.converter.nasa7.format;

import java.util.HashMap;
import java.util.Map;
import uk.ac.cam.ceb.como.thermo.property.ThermoState;

/**
 *
 * @author pb556
 */
public class Shomate {
    // similar as NASA7 format
    //private static final int numPolynomialCoeff = 7;
//    private double TMin = 20d;
//    private double TMid = 1000d;
//    private double TMax = 4000d;
//    private double[] coeffLow = new double[numPolynomialCoeff];
//    private double[] coeffHigh = new double[numPolynomialCoeff];
    //private int ctrPolynoms = 2;

    private Map<Integer, Double[]> polynomialCoefficients = null;
    private Map<Integer, Double[]> temperatures = null;
    private String id = null;
    private String comment = "";

    public Shomate(String id) {
        polynomialCoefficients = new HashMap<Integer, Double[]>();
        temperatures = new HashMap<Integer, Double[]>();
        this.id = id;
    }

    public Shomate(String id, Map<Integer, Double[]> polynomialCoefficients, Map<Integer, Double[]> temperatures) {
        this.polynomialCoefficients = polynomialCoefficients;
        this.temperatures = temperatures;
        this.id = id;
    }
    
    public void set(Map<Integer, Double[]> polynomialCoefficients, Map<Integer, Double[]> temperatures) {
        this.polynomialCoefficients = polynomialCoefficients;
        this.temperatures = temperatures;
    }
    
    public String getId() {
        return id;
    }
    
    public Map<Integer, Double[]> getCoefficients() {
        return polynomialCoefficients;
    }
    
    public Map<Integer, Double[]> getTemperatures() {
        return temperatures;
    }
    
    public void setComment(String comment) {
        this.comment = comment;
    }
    
    public String getComment() {
        return comment;
    }

    // check consistency - LATER!!!
    public ThermoState get(double temperature) {

        // check in which temp range it is
        int selectedOrder = -1;
        for (Integer order : temperatures.keySet()) {
            Double[] temp = temperatures.get(order);
            if (temperature <= temp[1] && temperature >= temp[0]) {
                selectedOrder = order;
                break;
            }
        }

        if (selectedOrder > 0 && polynomialCoefficients.containsKey(selectedOrder)) {
            return get(temperature, polynomialCoefficients.get(selectedOrder));
        }

        return null;
    }

    public ThermoState get(double temperature, Double[] coefficients) {
        if (coefficients.length >= 8) {
            double t = temperature / 1000.0;
            double Cp = coefficients[0] + coefficients[1] * t + coefficients[2] * t * t + coefficients[3] * t * t * t + coefficients[4] / t / t;
            double dH = coefficients[0] * t + coefficients[1] * t * t / 2.0 + coefficients[2] * t * t * t / 3.0 + coefficients[3] * t * t * t * t / 4.0 - coefficients[4] / t + coefficients[5] - coefficients[7];
            double S = Math.log(t) * coefficients[0] + coefficients[1] * t + coefficients[2] * t * t / 2.0 + coefficients[3] * t * t * t / 3.0 - coefficients[4] / (2.0 * t * t) + coefficients[6];
            ThermoState state = new ThermoState();
            state.Cp = Cp;
            state.delta_H = dH;
            state.S = S;
            state.T = temperature;
            return state;
        }
        return null;
    }
}
