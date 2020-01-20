/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.data.fitting;

import uk.ac.cam.ceb.como.math.data.fitting.PowerLawLeastSquaresFitting;
import uk.ac.cam.ceb.como.math.data.fitting.FittingException;
import java.util.ArrayList;
import java.util.List;
import uk.ac.cam.ceb.como.math.function.FunctionCalculationException;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class PowerLawLeastSquaresFittingTest {
    @Test
    public void test1() throws FittingException, FunctionCalculationException {
        assert (validate(1, 1, 100, 0.1, 1));
        assert (validate(1, -1, 100, 0.1, 1));
        assert (validate(2, 2, 100, 0.1, 1));
        assert (validate(2, -2, 100, 0.1, 1));
    }

    // simple one dimensional function
    public boolean validate(double a, double b, int ctr, double delta, double e) throws FittingException, FunctionCalculationException {
        List<Double[]> xData = new ArrayList<Double[]>();
        List<Double[]> yData = new ArrayList<Double[]>();

        for (int i = 1; i < ctr; i++) {
            xData.add(new Double[]{i * delta});
            yData.add(new Double[]{a * Math.pow(i * delta, b)});
        }

        PowerLawLeastSquaresFitting fitting = new PowerLawLeastSquaresFitting(xData, yData);
        boolean valid = true;
        for (int i = 1; i < ctr; i++) {
            xData.add(new Double[]{i * delta});
            yData.add(new Double[]{a * Math.pow(i * delta, b)});
            double vA = a * Math.pow(i * delta, b);
            double vB = fitting.f(new Double[]{i * delta})[0];
            double diff = Math.abs(a * Math.pow(i * delta, b) - fitting.f(new Double[]{i * delta})[0]);
            System.out.println(diff + ": (" + vA + ", " + vB + ")");
            valid &= diff < e;
        }
        return valid;
    }
}
