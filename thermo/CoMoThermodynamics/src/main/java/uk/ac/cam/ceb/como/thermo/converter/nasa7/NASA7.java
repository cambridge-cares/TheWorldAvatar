package uk.ac.cam.ceb.como.thermo.converter.nasa7;

import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;
import javax.persistence.Column;
import javax.persistence.Transient;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.apache.commons.lang.ArrayUtils;
import org.eclipse.persistence.oxm.annotations.XmlPath;
import uk.ac.cam.ceb.como.chem.property.Phase;

/**
 *
 * @author Weerapong Phadungsuakanan
 */
public class NASA7 {

    private Phase phase = Phase.Gaseous;
    private static final int n_poly = 7;
    private double T_min = 20d;
    private double T_mid = 1000d;
    private double T_max = 4000d;
    private double[] a_low = new double[n_poly];
    private double[] a_high = new double[n_poly];
    private int polynomialCount = 2;
    
    public Phase getPhase() {
        return phase;
    }

    public void setPhase(Phase phase) {
        this.phase = phase;
    }

    public double[] getA_high() {
        return a_high;
    }

    public void setA_high(double[] a_high) {
        this.a_high = a_high;
    }

    public double[] getA_low() {
        return a_low;
    }

    public void setA_low(double[] a_low) {
        this.a_low = a_low;
    }

    /**
     * Get pointer to the coefficient array depending on temperature range. if T equals T_mid, the upper coefficients
     * are returned.
     *
     * @param T Temperature (K)
     * @return Coefficient array depending on temperature range.
     */
    public double[] getPolynomialArray(double T) {
        if ((T >= getTemperatureLowerBound()) && (T < getTemperatureMidPoint())) {
            return getA_low();
        } else if ((T >= getTemperatureMidPoint()) && (T <= getTemperatureUpperBound())) {
            return getA_high();
        } else {
            return null;
        }
    }

    public int getPolynomialCount() {
        return polynomialCount;
    }

    public void setPolynomialCount(int polynomialCount) {
        this.polynomialCount = polynomialCount;
    }

    /**
     * Get temperature lower bound (K).
     *
     * @return
     */
    public double getTemperatureLowerBound() {
        return T_min;
    }

    /**
     * Get temperature mid point (K).
     *
     * @return
     */
    public double getTemperatureMidPoint() {
        return T_mid;
    }

    /**
     * Get temperature upper bound (K).
     *
     * @return
     */
    public double getTemperatureUpperBound() {
        return T_max;
    }

    /**
     * Set temperature lower bound (K).
     *
     * @param T_min Temperature lower bound. Default is 20 K.
     */
    public void setTemperatureLowerBound(double T_min) {
        this.T_min = T_min;
    }

    /**
     * Set temperature mid point (K).
     *
     * @param T_mid Temperature mid point. Default is 1000 K.
     */
    public void setTemperatureMidPoint(double T_mid) {
        this.T_mid = T_mid;
    }

    /**
     * Set temperature upper bound (K).
     *
     * @param T_max Temperature upper bound. Default is 4000 K.
     */
    public void setTemperatureUpperBound(double T_max) {
        this.T_max = T_max;
    }

    /**
     * Get number of coefficients
     *
     * @return integer number of coefficients.
     */
    @Transient
    public int getNumCoefficients() {
        return n_poly;
    }

    /**
     * Get coefficient of low temperature range.
     *
     * @param i index of coefficient, 0-6 or 0-8.
     * @return Coeffient of index <code>i</code> of low temperature range.
     */
    public double getALow(int i) {
        return a_low[i];
    }

    /**
     * Get coefficient of high temperature range.
     *
     * @param i index of coefficient, 0-6 or 0-8.
     * @return Coeffient of index <code>i</code> of high temperature range.
     */
    public double getAHigh(int i) {
        return a_high[i];
    }

    /**
     * Set coefficient of low temperature range at index
     * <code>i</code>.
     *
     * @param i index of coefficient, 0-6 or 0-8.
     * @param coeff Coeffient of index <code>i</code> of low temperature range.
     */
    public void setALow(int i, double coeff) {
        a_low[i] = coeff;
        // Pre-calculate temperature exponents
        //double T = Constant.T_25C; double T2 = T*T; double T3 = T2*T;
        //double T4 = T3 * T; double T5 = T4 * T;
        //H298p15K = Constant.R * (a_low[0]*T + a_low[1]*T2/2 + a_low[2]*T3/3 + a_low[3]*T4/4 + a_low[4]*T5/5 + a_low[5]);
    }

    /**
     * Set coefficient of high temperature range at index
     * <code>i</code>.
     *
     * @param i index of coefficient, 0-6 or 0-8.
     * @param coeff Coeffient of index <code>i</code> of high temperature range.
     */
    public void setAHigh(int i, double coeff) {
        a_high[i] = coeff;
    }

    /**
     * Give a string description of the
     * <code>NASAPolynomial</code> class.
     *
     * @return String description of the <code>NASAPolynomial</code> object.
     */
    @Override
    public String toString() {
        String NASAString = "Thermochemistry (" + getPhase() + ") NASA 7-Cofficient Polynomial, T = "
                + formatTemperature(T_min) + " - "
                + formatTemperature(T_mid) + " - "
                + formatTemperature(T_max) + " K"
                + "\n";
        String coef = "coef : ";
        String low = "Low  : ";
        String high = "High : ";
        for (int i = 0; i < a_low.length; i++) {
            //coef += "[     a" + (i+1) + "      ] ";
            coef += "             a" + (i + 1) + " ";
            low += formatNASACoeff(a_low[i]) + " ";
            high += formatNASACoeff(a_high[i]) + " ";
        }
        NASAString += coef + "\n" + low + "\n" + high;
        return NASAString;
    }
    /**
     * Decimal format for temperature string output.
     */
    private static final DecimalFormat dfTemp = new DecimalFormat("####0.00");
    /**
     * Decimal format for NASA coefficient.
     */
    private static final DecimalFormat dfCoef = new DecimalFormat(" 0.00000000E00;-0.00000000E00");
    /**
     * Pattern matching for positive exponent.
     */
    private static final Pattern p = Pattern.compile("E\\d");

    /**
     * Format
     * <code>double</code> value for NASA coefficient output.
     *
     * @param val NASA coefficient.
     * @return <code>String</code> of NASA coefficient.
     */
    private static String formatNASACoeff(double val) {
        String num_str = dfCoef.format(val);
        return ((p.matcher(num_str).find()) ? num_str.replace("E", "E+") : num_str);
    }

    /**
     * Format
     * <code>double</code> value of temperature to proper string.
     *
     * @param T Temperature.
     * @return <code>String</code> of temperature.
     */
    private static String formatTemperature(double T) {
        return dfTemp.format(T);
    }

    /*
     * ================================================================================
     * WARNING: The getter and setter methods below here are to be used by JPA only.
     *
     * They should be modified if there are changes to the fields of this class.
     * ================================================================================
     */
    protected double getAl_1() {
        return a_low[0];
    }

    protected void setAl_1(double al_1) {
        this.a_low[0] = al_1;
    }

    protected double getAl_2() {
        return a_low[1];
    }

    protected void setAl_2(double al_2) {
        this.a_low[1] = al_2;
    }

    protected double getAl_3() {
        return a_low[2];
    }

    protected void setAl_3(double al_3) {
        this.a_low[2] = al_3;
    }

    protected double getAl_4() {
        return a_low[3];
    }

    protected void setAl_4(double al_4) {
        this.a_low[3] = al_4;
    }

    protected double getAl_5() {
        return a_low[4];
    }

    protected void setAl_5(double al_5) {
        this.a_low[4] = al_5;
    }

    protected double getAl_6() {
        return a_low[5];
    }

    protected void setAl_6(double al_6) {
        this.a_low[5] = al_6;
    }

    protected double getAl_7() {
        return a_low[6];
    }

    protected void setAl_7(double al_7) {
        this.a_low[6] = al_7;
    }

    protected double getAh_1() {
        return a_high[0];
    }

    protected void setAh_1(double ah_1) {
        this.a_high[0] = ah_1;
    }

    protected double getAh_2() {
        return a_high[1];
    }

    protected void setAh_2(double ah_2) {
        this.a_high[1] = ah_2;
    }

    protected double getAh_3() {
        return a_high[2];
    }

    protected void setAh_3(double ah_3) {
        this.a_high[2] = ah_3;
    }

    protected double getAh_4() {
        return a_high[3];
    }

    protected void setAh_4(double ah_4) {
        this.a_high[3] = ah_4;
    }

    protected double getAh_5() {
        return a_high[4];
    }

    protected void setAh_5(double ah_5) {
        this.a_high[4] = ah_5;
    }

    protected double getAh_6() {
        return a_high[5];
    }

    protected void setAh_6(double ah_6) {
        this.a_high[5] = ah_6;
    }

    protected double getAh_7() {
        return a_high[6];
    }

    protected void setAh_7(double ah_7) {
        this.a_high[6] = ah_7;
    }
    /*
     * ================================================================================
     * WARNING: The getter and setter methods below here are to be used by JAXB only.
     *
     * They should be modified if there are changes to the fields of this class.
     * ================================================================================
     */

    protected List<Double> getCoeffsAHigh() {
        if (polynomialCount == 1) {
            return null;
        } else if (polynomialCount == 2) {
            return Arrays.asList(ArrayUtils.toObject(a_high));
        } else {
            throw new RuntimeException("Number of coffiecient count cannot exceed 2 [" + polynomialCount + " were found].");
        }
    }

    protected void seCoeffstAHigh(List<Double> a_high) {
        if (a_high.size() != n_poly) {
            throw new RuntimeException("NASA polynomials require 7 coefficients but " + a_high.size() + " are found.");
        }
        System.arraycopy(ArrayUtils.toPrimitive(a_high.toArray(new Double[n_poly])), 0, this.a_high, 0, n_poly);
    }

    protected List<Double> getCoeffsALow() {
        return Arrays.asList(ArrayUtils.toObject(a_low));
    }

    protected void setCoeffsALow(List<Double> a_low) {
        if (a_low.size() != n_poly) {
            throw new RuntimeException("NASA polynomials require 7 coefficients but " + a_low.size() + " are found.");
        }
        System.arraycopy(ArrayUtils.toPrimitive(a_low.toArray(new Double[n_poly])), 0, this.a_low, 0, n_poly);
    }
}