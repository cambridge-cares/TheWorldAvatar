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
// better to use a map!!!
public class FourierSeriesLimits extends ArrayList<FourierSeriesLimit> {

    public FourierSeriesLimits getLimitsByDimension(int dimension) {
        FourierSeriesLimits selected = new FourierSeriesLimits();
        for (int i = 0; i < this.size(); i++) {
            if (this.get(i).getDimension() == dimension) {
                selected.add(this.get(i));
            }
        }
        return selected;
    }

    public Map<Integer, FourierSeriesLimits> getLimitsByDimension() {
        Map<Integer, FourierSeriesLimits> selected = new HashMap<Integer, FourierSeriesLimits>();
        for (int i = 0; i < this.size(); i++) {
            if (!selected.containsKey(this.get(i).getDimension())) {
                selected.put(this.get(i).getDimension(), new FourierSeriesLimits());
            }
            selected.get(this.get(i).getDimension()).add(this.get(i));
        }
        return selected;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof FourierSeriesLimits) {
            FourierSeriesLimits limits = (FourierSeriesLimits) obj;
            if (limits.size() != size()) {
                return false;
            }
            boolean equal = true;
            for (int i = 0; i < size(); i++) {
                int dim = get(i).getDimension();
                for (int j = 0; j < limits.size(); j++) {
                    if (limits.get(j).getDimension() == dim) {
                        equal &= limits.get(i).equals(get(i));
                    }
                }
            }
            return equal;
        }
        return false;
    }
}
