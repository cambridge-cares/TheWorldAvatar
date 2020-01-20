/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.fourier.series;

/**
 *
 * @author pb556
 */
public class FourierSeriesCoefficient {

    protected int index;
    protected int dim;
    protected double[] val;

    public FourierSeriesCoefficient(int index, int dimension, double[] value) {
        this.index = index;
        dim = dimension;
        val = value;
    }

    public int getIndex() {
        return index;
    }

    public int getDimension() {
        return dim;
    }

    public double[] getValue() {
        return val;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof FourierSeriesCoefficient) {
            FourierSeriesCoefficient coefficient = (FourierSeriesCoefficient) obj;
            boolean equal = getDimension() == coefficient.getDimension() && getIndex() == coefficient.getIndex() && getValue().length == coefficient.getValue().length;
            if (!equal) {
                return equal;
            }
            for (int i = 0; i < getValue().length; i++) {
                equal &= (Math.abs(getValue()[i] - coefficient.getValue()[i]) < 0.0001);
            }
            return equal;
        }
        return false;
    }
}
