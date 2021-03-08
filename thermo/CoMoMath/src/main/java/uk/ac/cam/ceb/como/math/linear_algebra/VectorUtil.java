package uk.ac.cam.ceb.como.math.linear_algebra;

/**
 * 
 *
 * @author pb556
 */
public class VectorUtil {

    public static void debugPrint(double[] vector) {
        System.out.println(vector[0] + ", " + vector[1] + ", " + vector[2]);
    }

    /**
     * Calculate the magnitude of a vector represented by array of 3 double.
     * 
     * @param vector array of 3 double representing vector.
     * @return magnitude of a vector.
     */
    public static double getMagnitude(double[] vector) {
        return Math.sqrt(vector[0] * vector[0] + vector[1] * vector[1] + vector[2] * vector[2]);
    }

    public static void getCrossProduct(double[] vec_a, double[] vec_b, double[] vec_cross) {
        vec_cross[0] = vec_b[1] * vec_a[2] - vec_b[2] * vec_a[1];
        vec_cross[1] = vec_b[2] * vec_a[0] - vec_b[0] * vec_a[2];
        vec_cross[2] = vec_b[0] * vec_a[1] - vec_b[1] * vec_a[0];
    }

    public static double getDotProduct(double[] vec_a, double[] vec_b) {
        return vec_a[0] * vec_b[0] + vec_a[1] * vec_b[1] + vec_a[2] * vec_b[2];
    }

    public static double getAngle(double[] vec_a, double[] vec_b) {
        return Math.acos(getDotProduct(vec_a, vec_b) / getMagnitude(vec_a) / getMagnitude(vec_b));
    }

    public static void getSubtraction(double[] vec_a, double[] vec_b, double[] vec_result) {
        vec_result[0] = vec_a[0] - vec_b[0];
        vec_result[1] = vec_a[1] - vec_b[1];
        vec_result[2] = vec_a[2] - vec_b[2];
    }

    public static void getAddition(double[] vec_a, double[] vec_b, double[] vec_result) {
        vec_result[0] = vec_a[0] + vec_b[0];
        vec_result[1] = vec_a[1] + vec_b[1];
        vec_result[2] = vec_a[2] + vec_b[2];
    }

    public static void normalise(double[] vector) {
        double norm = getMagnitude(vector);
        vector[0] /= norm;
        vector[1] /= norm;
        vector[2] /= norm;
    }

    public static double[] createZeroVector() {
        double[] vec = new double[3];
        vec[0] = vec[1] = vec[2] = 0.0;
        return vec;
    }

    public static double[] createVector(double x, double y, double z) {
        double[] vec = new double[3];
        vec[0] = x;
        vec[1] = y;
        vec[2] = z;
        return vec;
    }
}
