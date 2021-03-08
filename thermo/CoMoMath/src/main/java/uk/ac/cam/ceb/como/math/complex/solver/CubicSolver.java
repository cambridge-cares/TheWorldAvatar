package uk.ac.cam.ceb.como.math.complex.solver;

import uk.ac.cam.ceb.como.math.complex.Complex;

/**
 *
 * @author pb556
 */
public abstract class CubicSolver {

    public static double[] solveCubicEquation(double a, double b, double c, double d) {
        double q = 2 * b * b * b - 9 * a * b * c + 27 * a * a * d;
        double p2 = b * b - 3 * a * c;
        double p = q * q - 4 * p2 * p2 * p2;
        Complex P = new Complex(p, 0.0);
        P = P.squareRoot();
        Complex n2 = new Complex(2.0, 0.0);
        Complex Q = new Complex(q, 0.0);
        Complex S1 = Q.plus(P).div(n2);
        Complex S2 = Q.minus(P).div(n2);
        S1 = S1.cubicRoot();
        S2 = S2.cubicRoot();
        Complex cmb = new Complex(-b, 0.0);
        Complex cp3 = new Complex(0.5, Math.sqrt(3) / 2.0);
        Complex cm3 = cp3.conj();
        Complex c3a = new Complex(3 * a, 0.0);
        Complex x1 = cmb.minus(S1).minus(S2).div(c3a);
        Complex x2 = cmb.plus(cp3.times(S1)).plus(cm3.times(S2)).div(c3a);
        Complex x3 = cmb.plus(cm3.times(S1)).plus(cp3.times(S2)).div(c3a);
        double[] eig = new double[3];
        eig[0] = x1.real();
        eig[1] = x2.real();
        eig[2] = x3.real();
        return eig;
    }

    // find cubic solution to x^3 + bx^2 + cx + d = 0 where a = 1;
    public static double[] solveCubicEquation(double b, double c, double d) {
        double q = 2 * b * b * b - 9 * b * c + 27 * d;
        double p2 = b * b - 3 * c;
        double p = q * q - 4 * p2 * p2 * p2;
        Complex P = new Complex(p, 0.0);
        P = P.squareRoot();
        Complex n2 = new Complex(2.0, 0.0);
        Complex Q = new Complex(q, 0.0);
        Complex S1 = Q.plus(P).div(n2);
        Complex S2 = Q.minus(P).div(n2);
        S1 = S1.cubicRoot();
        S2 = S2.cubicRoot();
        Complex cmb = new Complex(-b, 0.0);
        Complex cp3 = new Complex(0.5, Math.sqrt(3) / 2.0);
        Complex cm3 = cp3.conj();
        Complex c3a = new Complex(3.0, 0.0);
        Complex x1 = cmb.minus(S1).minus(S2).div(c3a);
        Complex x2 = cmb.plus(cp3.times(S1)).plus(cm3.times(S2)).div(c3a);
        Complex x3 = cmb.plus(cm3.times(S1)).plus(cp3.times(S2)).div(c3a);
        double[] eig = new double[3];
        eig[0] = x1.real();
        eig[1] = x2.real();
        eig[2] = x3.real();
        return eig;
    }
}
