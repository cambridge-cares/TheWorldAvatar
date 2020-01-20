/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.paper.enthalpy.reduction.list_calculator;

import java.util.List;
import uk.ac.cam.ceb.como.math.average.Mean;

/**
 *
 * @author pb556
 */
public class MeanListCalculator implements ListCalculator<Double> {

    @Override
    public Double calculate(List<Double> values) {
        double[] array = new double[values.size()];
        for (int i = 0; i < values.size(); i++) {
            array[i] = values.get(i);
        }
        try {
            return new Mean().calculate(array);
        } catch (Exception ex) {
            return Double.MAX_VALUE;
        }
    }
}
