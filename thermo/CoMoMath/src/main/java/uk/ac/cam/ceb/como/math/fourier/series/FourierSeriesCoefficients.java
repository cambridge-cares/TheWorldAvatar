/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.fourier.series;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author pb556
 */
public class FourierSeriesCoefficients extends ArrayList<FourierSeriesCoefficient> {

    public FourierSeriesCoefficients getFourierCoefficientsByIndex(int index) {
        return get(index, new IndexSelector());
    }

    public FourierSeriesCoefficients getFourierCoefficientsByDimension(int dimension) {
        return get(dimension, new DimensionSelector());
    }

    public Map<Integer, FourierSeriesCoefficients> getFourierCoefficientsByIndex() {
        return get(new IndexSelector());
    }

    public Map<Integer, FourierSeriesCoefficients> getFourierCoefficientsByDimension() {
        return get(new DimensionSelector());
    }

    protected FourierSeriesCoefficients get(int value, Selector selector) {
        FourierSeriesCoefficients selected = new FourierSeriesCoefficients();
        for (int i = 0; i < this.size(); i++) {
            if (selector.get(this.get(i)) == value) {
                selected.add(this.get(i));
            }
        }
        return selected;
    }

    protected Map<Integer, FourierSeriesCoefficients> get(Selector selector) {
        Map<Integer, FourierSeriesCoefficients> selected = new HashMap<Integer, FourierSeriesCoefficients>();
        for (int i = 0; i < this.size(); i++) {
            if (!selected.containsKey(selector.get(this.get(i)))) {
                selected.put(selector.get(this.get(i)), new FourierSeriesCoefficients());
            }
            selected.get(selector.get(this.get(i))).add(this.get(i));
        }
        return selected;
    }

    protected interface Selector {

        public int get(FourierSeriesCoefficient coeff);
    }

    protected class IndexSelector implements Selector {

        @Override
        public int get(FourierSeriesCoefficient coeff) {
            return coeff.getIndex();
        }
    }

    protected class DimensionSelector implements Selector {

        @Override
        public int get(FourierSeriesCoefficient coeff) {
            return coeff.getDimension();
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof FourierSeriesCoefficients) {
            FourierSeriesCoefficients coefficients = (FourierSeriesCoefficients) obj;
            if (coefficients.size() != size()) {
                return false;
            }
            boolean equal = true;
            for (int i = 0; i < size(); i++) {
                int dim = get(i).getDimension();
                int index = get(i).getIndex();
                for (int j = 0; j < coefficients.size(); j++) {
                    if (coefficients.get(j).getDimension() == dim && coefficients.get(j).getIndex() == index) {
                        equal &= coefficients.get(i).equals(get(i));
                    }
                }
            }
            return equal;
        }
        return false;
    }
}
