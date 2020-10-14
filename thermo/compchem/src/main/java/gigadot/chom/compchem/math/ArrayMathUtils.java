package gigadot.chom.compchem.math;

import java.util.Arrays;

/**
 *
 * @author Weerapong
 */
public class ArrayMathUtils {

    public static void copy(double[] original, double[] destination) {
        if (original.length != destination.length) {
            throw new IllegalArgumentException("source and destination array are not of the same length.");
        }
        System.arraycopy(original, 0, destination, 0, original.length);
    }

    public static double[] copyAndScale(double[] original, double scale) {
        double[] copy = Arrays.copyOf(original, original.length);
        for (int i = 0; i < copy.length; i++) {
            copy[i] *= scale;
        }
        return copy;
    }

    public static void copyAndScale(double[] original, double[] destination, double scale) {
        if (original.length != destination.length) {
            throw new IllegalArgumentException("source and destination array are not of the same length.");
        }
        System.arraycopy(original, 0, destination, 0, original.length);
        for (int i = 0; i < destination.length; i++) {
            destination[i] *= scale;
        }
    }

    public static double[] copyOf(double[] original) {
        return Arrays.copyOf(original, original.length);
    }
}
