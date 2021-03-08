package uk.ac.cam.ceb.como.math.integrator;

import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.math.function.FunctionCalculationException;

/**
 * A Simpsom's integrator.
 * @author pb556
 */
public class SimpsonIntegrator {

    private Function<Double,Double> f = null;
    private double a = 0.0;
    private double b = 0.0;
    private int n = 200;

    public void setFunction(Function<Double,Double> f) {
        this.f = f;
    }

    public void setLimit(double a, double b) {
        this.a = a;
        this.b = b;
    }

    public double getIntegral() throws FunctionCalculationException {
        if ((a < b) && (f != null) && (n > 1)) {
            double sumfeven = 0;
            double sumfodd = 0;
            double len = (b - a);
            double h = len / (2 * n);
            double x_odd = a + h;
            double x_even = x_odd + h;
            for (int i = 0; i < n - 1; i++) {
                sumfodd += f.f(x_odd);
                sumfeven += f.f(x_even);
//                x_odd = a + len*((i+1.5)/n);
//                x_even = a + len*((i+1)/n);
                x_odd = x_even + h;
                x_even = x_odd + h;
            }
            // last odd;
            sumfodd += f.f(x_odd);
            // multiply by factors
            sumfeven *= 2;
            sumfodd *= 4;
            double integral = (h / 3) * (f.f(a) + sumfeven + sumfodd + f.f(b));
            return integral;
        } else {
            return 0;
        }
    }
}
