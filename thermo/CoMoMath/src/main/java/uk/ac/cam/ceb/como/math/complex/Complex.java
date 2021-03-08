package uk.ac.cam.ceb.como.math.complex;

/**
@author pb556
 */
public class Complex {

    private double a = 0.0;
    private double b = 0.0;

    /**
    Constructs the complex number z = a + i*b
    @param a Real part
    @param b Imaginary part
     */
    public Complex(double a, double b) {
        this.a = a;
        this.b = b;
    }

    /**
    Real part of this Complex number
    (the x-coordinate in rectangular coordinates).
    @return Re[z] where z is this Complex number.
     */
    public double real() {
        return a;
    }

    /**
    Imaginary part of this Complex number
    (the y-coordinate in rectangular coordinates).
    @return Im[z] where z is this Complex number.
     */
    public double imag() {
        return b;
    }

    /**
    Modulus of this Complex number
    (the distance from the origin in polar coordinates).
    @return |z| where z is this Complex number.
     */
    public double mod() {
        if (a != 0 || b != 0) {
            return Math.sqrt(a * a + b * b);
        } else {
            return 0.0;
        }
    }

    /**
    Argument of this Complex number
    (the angle in radians with the x-axis in polar coordinates).
    @return arg(z) where z is this Complex number.
     */
    public double arg() {
        return Math.atan2(b, a);
    }

    /**
    Complex conjugate of this Complex number
    (the conjugate of x+i*y is x-i*y).
    @return z-bar where z is this Complex number.
     */
    public Complex conj() {
        return new Complex(a, -b);
    }

    /**
    Addition of Complex numbers (doesn't change this Complex number).
    <br>(x+i*y) + (s+i*t) = (x+s)+i*(y+t).
    @param w is the number to add.
    @return z+w where z is this Complex number.
     */
    public Complex plus(Complex w) {
        return new Complex(a + w.real(), b + w.imag());
    }

    /**
    Subtraction of Complex numbers (doesn't change this Complex number).
    <br>(x+i*y) - (s+i*t) = (x-s)+i*(y-t).
    @param w is the number to subtract.
    @return z-w where z is this Complex number.
     */
    public Complex minus(Complex w) {
        return new Complex(a - w.real(), b - w.imag());
    }

    /**
    Complex multiplication (doesn't change this Complex number).
    @param w is the number to multiply by.
    @return z*w where z is this Complex number.
     */
    public Complex times(Complex w) {
        return new Complex(a * w.real() - b * w.imag(), a * w.imag() + b * w.real());
    }

    /**
    Division of Complex numbers (doesn't change this Complex number).
    <br>(x+i*y)/(s+i*t) = ((x*s+y*t) + i*(y*s-y*t)) / (s^2+t^2)
    @param w is the number to divide by
    @return new Complex number z/w where z is this Complex number
     */
    public Complex div(Complex w) {
        double den = Math.pow(w.mod(), 2);
        return new Complex((a * w.real() + b * w.imag()) / den, (b * w.real() - a * w.imag()) / den);
    }

    /**
    Complex exponential (doesn't change this Complex number).
    @return exp(z) where z is this Complex number.
     */
    public Complex exp() {
        return new Complex(Math.exp(a) * Math.cos(b), Math.exp(a) * Math.sin(b));
    }

    /**
    Principal branch of the Complex logarithm of this Complex number.
    (doesn't change this Complex number).
    The principal branch is the branch with -pi < arg <= pi.
    @return log(z) where z is this Complex number.
     */
    public Complex log() {
        return new Complex(Math.log(this.mod()), this.arg());
    }

    /**
    Complex square root (doesn't change this complex number).
    Computes the principal branch of the square root, which
    is the value with 0 <= arg < pi.
    @return sqrt(z) where z is this Complex number.
     */
    public Complex squareRoot() {
        double r = Math.sqrt(this.mod());
        double theta = this.arg() / 2;
        return new Complex(r * Math.cos(theta), r * Math.sin(theta));
    }

    /**
    Complex cubic root.
     */
    public Complex cubicRoot() {
        double r = Math.pow(this.mod(), 1.0 / 3.0);
        double theta = this.arg() / 3;
        return new Complex(r * Math.cos(theta), r * Math.sin(theta));
    }

//    // Real cosh function (used to compute complex trig functions)
//    private double cosh(double theta) {
//        return (Math.exp(theta)+Math.exp(-theta))/2;
//    }
//
//    // Real sinh function (used to compute complex trig functions)
//    private double sinh(double theta) {
//        return (Math.exp(theta)-Math.exp(-theta))/2;
//    }
//
//    /**
//        Sine of this Complex number (doesn't change this Complex number).
//        <br>sin(z) = (exp(i*z)-exp(-i*z))/(2*i).
//        @return sin(z) where z is this Complex number.
//    */
//    public Complex sin() {
//        return new Complex(cosh(b)*Math.sin(a),sinh(b)*Math.cos(a));
//    }
//
//    /**
//        Cosine of this Complex number (doesn't change this Complex number).
//        <br>cos(z) = (exp(i*z)+exp(-i*z))/ 2.
//        @return cos(z) where z is this Complex number.
//    */
//    public Complex cos() {
//        return new Complex(cosh(b)*Math.cos(a),-sinh(b)*Math.sin(a));
//    }
//
//    /**
//        Hyperbolic sine of this Complex number
//        (doesn't change this Complex number).
//        <br>sinh(z) = (exp(z)-exp(-z))/2.
//        @return sinh(z) where z is this Complex number.
//    */
//    public Complex sinh() {
//        return new Complex(sinh(a)*Math.cos(b),cosh(a)*Math.sin(b));
//    }
//
//    /**
//        Hyperbolic cosine of this Complex number
//        (doesn't change this Complex number).
//        <br>cosh(z) = (exp(z) + exp(-z)) / 2.
//        @return cosh(z) where z is this Complex number.
//    */
//    public Complex cosh() {
//        return new Complex(cosh(a)*Math.cos(b),sinh(a)*Math.sin(b));
//    }
//
//    /**
//        Tangent of this Complex number (doesn't change this Complex number).
//        <br>tan(z) = sin(z)/cos(z).
//        @return tan(z) where z is this Complex number.
//    */
//    public Complex tan() {
//        return (this.sin()).div(this.cos());
//    }
//
//    /**
//        Negative of this complex number (chs stands for change sign).
//        This produces a new Complex number and doesn't change
//        this Complex number.
//        <br>-(x+i*y) = -x-i*y.
//        @return -z where z is this Complex number.
//    */
//    public Complex chs() {
//        return new Complex(-a,-b);
//    }
    /**
    String representation of this Complex number.
    @return x+i*y, x-i*y, x, or i*y as appropriate.
     */
    @Override
    public String toString() {
        if (a != 0 && b > 0) {
            return a + " + " + b + "i";
        }
        if (a != 0 && b < 0) {
            return a + " - " + (-b) + "i";
        }
        if (b == 0) {
            return String.valueOf(a);
        }
        if (a == 0) {
            return b + "i";
        }
        // shouldn't get here (unless Inf or NaN)
        return a + " + i*" + b;

    }
}
