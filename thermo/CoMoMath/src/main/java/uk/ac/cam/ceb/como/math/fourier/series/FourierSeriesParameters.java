/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.fourier.series;

/**
 *
 * @author pb556
 */
public class FourierSeriesParameters {

    protected FourierSeriesCoefficients coeff;
    protected FourierSeriesLimits lower;
    protected FourierSeriesLimits upper;
    protected int order;

    public FourierSeriesParameters(int order, FourierSeriesCoefficients coeff, FourierSeriesLimits lower, FourierSeriesLimits upper) {
        this.order = order;
        this.coeff = coeff;
        this.lower = lower;
        this.upper = upper;
    }

    public FourierSeriesCoefficients getFourierSeriesCoefficients() {
        return coeff;
    }

    public FourierSeriesLimits getLowerLimits() {
        return lower;
    }

    public FourierSeriesLimits getUpperLimits() {
        return upper;
    }

    public int getOrder() {
        return order;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof FourierSeriesParameters) {
            FourierSeriesParameters cmp = (FourierSeriesParameters) obj;
            boolean equal = cmp.getOrder() == getOrder();
            if (cmp.getLowerLimits() != null && getLowerLimits() != null) {
                equal &= getLowerLimits().equals(cmp.getLowerLimits());
            } else if (cmp.getLowerLimits() == null && getLowerLimits() == null) {
            } else {
                return false;
            }
            if (cmp.getUpperLimits() != null && getUpperLimits() != null) {
                equal &= getUpperLimits().equals(cmp.getUpperLimits());
            } else if (cmp.getUpperLimits() == null && getUpperLimits() == null) {
            } else {
                return false;
            }
            if (cmp.getFourierSeriesCoefficients() != null && getFourierSeriesCoefficients() != null) {
                equal &= getFourierSeriesCoefficients().equals(cmp.getFourierSeriesCoefficients());
            } else if (cmp.getFourierSeriesCoefficients() == null && getFourierSeriesCoefficients() == null) {
            } else {
                return false;
            }
            return equal;
        }
        return false;
    }
}
