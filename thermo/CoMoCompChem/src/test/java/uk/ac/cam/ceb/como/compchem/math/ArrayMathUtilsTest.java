package uk.ac.cam.ceb.como.compchem.math;

import uk.ac.cam.ceb.como.compchem.math.ArrayMathUtils;
import java.util.Arrays;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author pb556
 */
public class ArrayMathUtilsTest {
    
    /**
     * Test of copy method, of class ArrayMathUtils.
     */
    @Test
    public void testArraycopy() {
        System.out.println("copy");
        double[] original = {1.0, 2.3, 1E-10, 3e2, 5.003d};
        double[] destination = new double[5];
        assertFalse(Arrays.equals(original, destination));
        ArrayMathUtils.copy(original, destination);
        assertTrue(Arrays.equals(original, destination));
    }

    /**
     * Test of copyAndScale method, of class ArrayMathUtils.
     */
    @Test
    public void testCopyAndScale_doubleArr_double() {
        System.out.println("copyAndScale");
        double[] original = {5.0, 2.0, 5.003d, 4d};
        double scale = 2.5;
        double[] expResult = {12.5, 5.0, 12.5075, 10.0};
        assertFalse(Arrays.equals(original, expResult));
        double[] result = ArrayMathUtils.copyAndScale(original, scale);
        assertTrue(Arrays.equals(expResult, result));
    }

    /**
     * Test of copyAndScale method, of class ArrayMathUtils.
     */
    @Test
    public void testCopyAndScale_3args() {
        System.out.println("copyAndScale");
        double[] original = {5.0, 2.0, 5.003d, 4d};
        double[] destination = new double[4];
        double[] expResult = {12.5, 5.0, 12.5075, 10.0};
        double scale = 2.5;
        assertFalse(Arrays.equals(original, destination));
        ArrayMathUtils.copyAndScale(original, destination, scale);
        assertTrue(Arrays.equals(expResult, destination));
    }

    @Test
    public void testCopyAndScaleOnTheSameArray() {
        System.out.println("copyAndScale on the same array");
        double[] original = {5.0, 2.0, 5.003d, 4d};
        //double[] destination = new double[4];
        double[] expResult = {12.5, 5.0, 12.5075, 10.0};
        double scale = 2.5;
        ArrayMathUtils.copyAndScale(original, original, scale);
        assertTrue(Arrays.equals(expResult, original));
    }

    /**
     * Test of copyOf method, of class ArrayMathUtils.
     */
    @Test
    public void testCopyOf() {
        System.out.println("copyOf");
        double[] original = {5.0, 2.0, 5.003d, 4d};
        double[] result = ArrayMathUtils.copyOf(original);
        assertTrue(Arrays.equals(original, result));
    }
}
