/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.data.fitting;

import uk.ac.cam.ceb.como.math.data.fitting.FittingException;
import uk.ac.cam.ceb.como.math.data.fitting.LinearLeastSquaresFitting;
import java.util.ArrayList;
import java.util.List;
import uk.ac.cam.ceb.como.math.function.FunctionCalculationException;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class LinearLeastSquaresFittingTest {
    @Test
    public void test1() throws FittingException, FunctionCalculationException {
        assert (validate(1, 1, 100, 0.1, 1));
        assert (validate(1, -1, 100, 0.1, 1));
        assert (validate(1, 0.002, 100, 0.1, 1));
        assert (validate(1, -0.002, 100, 0.1, 1));
    }

    // simple one dimensional function
    public boolean validate(double a, double b, int ctr, double delta, double e) throws FittingException, FunctionCalculationException {
        List<Double[]> xData = new ArrayList<Double[]>();
        List<Double[]> yData = new ArrayList<Double[]>();

        for (int i = 0; i < ctr; i++) {
            xData.add(new Double[]{i * delta});
            yData.add(new Double[]{a + (b * i * delta)});
        }

        LinearLeastSquaresFitting fitting = new LinearLeastSquaresFitting(xData, yData);
        boolean valid = true;
        for (int i = 0; i < ctr; i++) {
            xData.add(new Double[]{i * delta});
            yData.add(new Double[]{a + (b * i * delta)});
            double vA = a + (b * i * delta);
            double vB = fitting.f(new Double[]{i * delta})[0];
            double diff = Math.abs(a + (b * i * delta) - fitting.f(new Double[]{i * delta})[0]);
            System.out.println(diff + ": (" + vA + ", " + vB + ")");
            valid &= diff < e;
        }
        return valid;
    }
}
