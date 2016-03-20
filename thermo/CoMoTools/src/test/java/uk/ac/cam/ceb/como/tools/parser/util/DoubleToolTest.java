package uk.ac.cam.ceb.como.tools.parser.util;

import uk.ac.cam.ceb.como.tools.parser.util.DoubleTool;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author pb556
 */
public class DoubleToolTest extends AbstractUnitTest {

    public DoubleToolTest() {
    }

    /**
     * Test of number method, of class DoubleTool.
     */
    @Test
    public void testNumberPattern() {
        printTestTitle("NUMBER_PATTERN/NUMBER_REGEX");
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("0.1234").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher(".1234").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("123.1234").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-1.234").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-.1234").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("+.1234").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("12.1234E5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("12.1234E-05").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("12.1234E+5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("+12.1234G5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-12.1234G+5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-12.1234G-05").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-12.1234G5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("12.1234G+5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("12.1234G-05").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("12.1234G5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-.1234e+5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher(".1234e-05").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("+.1234e5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("+12.1234e+5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("+12.1234e-05").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("+12.1234e5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-12.1234e+5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-12.1234e-05").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-12.1234e5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("12.1234e+5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("12.1234e-05").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("12.1234e5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-.1234E+5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher(".1234E-05").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("+.1234E5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("+12.1234E+5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("+12.1234E-05").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("+12.1234E5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-12.1234E+5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-12.1234E-05").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-12.1234E5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("+12.1234G-05").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("+12.1234G+5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("+.1234E5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher(".1234E-05").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-.1234E+5").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("1234.").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-1234.").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("0.").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("+12.").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-12.E0").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("+12.E-1").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("12.E+1").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-12.D0").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("+12.D-1").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("12.D+1").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("-12.G0").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("+12.G-1").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("12.G+1").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("12.g+1").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("12.g+1d").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("12.e+1D").matches());
        assertTrue(DoubleTool.NUMBER_PATTERN.matcher("12.g+1F").matches());
    }

    /**
     * Test of number method, of class DoubleTool.
     */
    @Test
    public void testParseDouble() {
        printTestTitle("parseDouble");
        assertEquals(0.1234, DoubleTool.parseDouble("0.1234"), 0);
        assertEquals(0.1234, DoubleTool.parseDouble(".1234"), 0);
        assertEquals(123.1234, DoubleTool.parseDouble("123.1234"), 0);
        assertEquals(-1.234, DoubleTool.parseDouble("-1.234"), 0);
        assertEquals(-0.1234, DoubleTool.parseDouble("-.1234"), 0);
        assertEquals(0.1234, DoubleTool.parseDouble("+.1234"), 0);
        assertEquals(12.1234E5, DoubleTool.parseDouble("12.1234E5"), 0);
        assertEquals(12.1234E-5, DoubleTool.parseDouble("12.1234E-05"), 0);
        assertEquals(12.1234E5, DoubleTool.parseDouble("12.1234E+5"), 0);
        assertEquals(12.1234E5, DoubleTool.parseDouble("+12.1234G5"), 0);
        assertEquals(-12.1234E5, DoubleTool.parseDouble("-12.1234G+5"), 0);
        assertEquals(-12.1234E-5, DoubleTool.parseDouble("-12.1234G-05"), 0);
        assertEquals(-12.1234E5, DoubleTool.parseDouble("-12.1234G5"), 0);
        assertEquals(12.1234E5, DoubleTool.parseDouble("12.1234G+5"), 0);
        assertEquals(12.1234E-5, DoubleTool.parseDouble("12.1234G-05"), 0);
        assertEquals(12.1234E5, DoubleTool.parseDouble("12.1234G5"), 0);
        assertEquals(-0.1234E5, DoubleTool.parseDouble("-.1234e+5"), 0);
        assertEquals(0.1234E-5, DoubleTool.parseDouble(".1234e-05"), 0);
        assertEquals(0.1234E5, DoubleTool.parseDouble("+.1234e5"), 0);
        assertEquals(12.1234E5, DoubleTool.parseDouble("+12.1234e+5"), 0);
        assertEquals(12.1234E-5, DoubleTool.parseDouble("+12.1234e-05"), 0);
        assertEquals(12.1234E5, DoubleTool.parseDouble("+12.1234e5"), 0);
        assertEquals(-12.1234E5, DoubleTool.parseDouble("-12.1234e+5"), 0);
        assertEquals(-12.1234E-5, DoubleTool.parseDouble("-12.1234e-05"), 0);
        assertEquals(-12.1234E5, DoubleTool.parseDouble("-12.1234e5"), 0);
        assertEquals(12.1234E5, DoubleTool.parseDouble("12.1234e+5"), 0);
        assertEquals(12.1234E-5, DoubleTool.parseDouble("12.1234e-05"), 0);
        assertEquals(12.1234E5, DoubleTool.parseDouble("12.1234e5"), 0);
        assertEquals(-0.1234E5, DoubleTool.parseDouble("-.1234E+5"), 0);
        assertEquals(0.1234E-5, DoubleTool.parseDouble(".1234E-05"), 0);
        assertEquals(0.1234E5, DoubleTool.parseDouble("+.1234E5"), 0);
        assertEquals(12.1234E5, DoubleTool.parseDouble("+12.1234E+5"), 0);
        assertEquals(12.1234E-5, DoubleTool.parseDouble("+12.1234E-05"), 0);
        assertEquals(12.1234E5, DoubleTool.parseDouble("+12.1234E5"), 0);
        assertEquals(-12.1234E5, DoubleTool.parseDouble("-12.1234E+5"), 0);
        assertEquals(-12.1234E-5, DoubleTool.parseDouble("-12.1234E-05"), 0);
        assertEquals(-12.1234E5, DoubleTool.parseDouble("-12.1234E5"), 0);
        assertEquals(12.1234E-5, DoubleTool.parseDouble("+12.1234G-05"), 0);
        assertEquals(12.1234E5, DoubleTool.parseDouble("+12.1234G+5"), 0);
        assertEquals(0.1234E5, DoubleTool.parseDouble("+.1234E5"), 0);
        assertEquals(0.1234E-5, DoubleTool.parseDouble(".1234E-05"), 0);
        assertEquals(-0.1234E5, DoubleTool.parseDouble("-.1234E+5"), 0);
        assertEquals(1234, DoubleTool.parseDouble("1234."), 0);
        assertEquals(-1234, DoubleTool.parseDouble("-1234."), 0);
        assertEquals(0, DoubleTool.parseDouble("0."), 0);
        assertEquals(12, DoubleTool.parseDouble("+12."), 0);
        assertEquals(-12, DoubleTool.parseDouble("-12.E0"), 0);
        assertEquals(1.2, DoubleTool.parseDouble("+12.E-1"), 0);
        assertEquals(120, DoubleTool.parseDouble("12.E+1"), 0);
        assertEquals(-12, DoubleTool.parseDouble("-12.D0"), 0);
        assertEquals(1.2, DoubleTool.parseDouble("+12.D-1"), 0);
        assertEquals(120, DoubleTool.parseDouble("12.D+1"), 0);
        assertEquals(-12, DoubleTool.parseDouble("-12.G0"), 0);
        assertEquals(1.2, DoubleTool.parseDouble("+12.G-1"), 0);
        assertEquals(120, DoubleTool.parseDouble("12.G+1"), 0);
        assertEquals(120, DoubleTool.parseDouble("12.g+1"), 0);

        assertEquals(12, DoubleTool.parseDouble("12.0d"), 0);
        assertEquals(12, DoubleTool.parseDouble("12.d"), 0);
        assertEquals(12, DoubleTool.parseDouble("12.f"), 0);
        assertEquals(12, DoubleTool.parseDouble("12.d0D"), 0);
        try {
            double d = DoubleTool.parseDouble(" ");
            assertEquals(Double.NaN, d, 0);
            assertTrue(false);
        } catch (NumberFormatException nfe) {
            assertTrue(true);
        }
    }

}