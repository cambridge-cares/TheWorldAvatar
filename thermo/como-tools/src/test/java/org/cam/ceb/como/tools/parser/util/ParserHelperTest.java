/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.parser.util;

import org.cam.ceb.como.tools.parser.util.ParserHelper;
import java.util.ArrayList;
import java.util.List;
import org.cam.ceb.como.tools.parser.util.ParserHelper;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ParserHelperTest {

//    @Test
//    public void parseLineTest() {
//        assert(ParserHelper.parseLine(Pattern.compile("\\d+"), "  Atom  AN      X      Y      Z        X      Y      Z        X      Y      Z") == null);
//        //System.out.println(ParserHelper.parseLine(Pattern.compile("\\d+"), "  Atom  1 AN      X      Y      Z        X      Y      Z        X      Y      Z"));
//        //assert("1".equals(ParserHelper.parseLine(Pattern.compile("(\\d+)"), "  Atom  1 AN      X      Y      Z        X      Y      Z        X      Y      Z").trim()));
//        //assert("100".equals(ParserHelper.parseLine(Pattern.compile("(\\d+)"), "  Atom  AN   100   X      Y      Z        X      Y      Z        X      Y      Z").trim()));
//    }

    @Test
    public void parseDoublesTest() {
        ArrayList<String> vals = new ArrayList<String>();
        vals.add("0.1");
        vals.add("0.091");
        vals.add("0.100");
        vals.add("1.1");
        vals.add("34.1");
        vals.add("+32.1");
        vals.add("-323.1");
        vals.add("0");
        vals.add("1.0");
        vals.add("5");
        vals.add("+9");
        vals.add("-10000");
        vals.add("1e2");
        vals.add("1e-1");
        ArrayList<Double> cmpVals = new ArrayList<Double>();
        cmpVals.add(0.1);
        cmpVals.add(0.091);
        cmpVals.add(0.1);
        cmpVals.add(1.1);
        cmpVals.add(34.1);
        cmpVals.add(32.1);
        cmpVals.add(-323.1);
        cmpVals.add(0.0);
        cmpVals.add(1.0);
        cmpVals.add(5.0);
        cmpVals.add(9.0);
        cmpVals.add(-10000.0);
        cmpVals.add(100.0);
        cmpVals.add(0.1);

        List<Double> pVals = ParserHelper.parseDoubles(vals);
        assert(pVals.size() == cmpVals.size());
        for (Double v : pVals) {
            assert(cmpVals.contains(v));
        }
    }

    @Test
    public void tokenizeTest() {
        List<String> token = ParserHelper.tokenize("empty");
        assert (!token.isEmpty());
        assert (token.size() == 1);
        assert (token.contains("empty"));

        token = ParserHelper.tokenize("not empty");
        assert (!token.isEmpty());
        assert (token.size() == 2);
        assert (token.contains("empty"));
        assert (token.contains("not"));

        token = ParserHelper.tokenize("123 258      987\t45436\n485\rasd1 fff/ffff\f566.");
        assert (!token.isEmpty());
        assert (token.size() == 8);
        assert (token.contains("123"));
        assert (token.contains("258"));
        assert (token.contains("987"));
        assert (token.contains("45436"));
        assert (token.contains("485"));
        assert (token.contains("asd1"));
        assert (token.contains("fff/ffff"));
        assert (token.contains("566."));
    }

    @Test
    public void extractNumbersTest() {
        List<String> lines = new ArrayList<String>();
        lines.add(" Frequencies --   1101.3327              1173.4500              1263.3184");
        lines.add(" Red. masses --      2.3486                 1.5301                 1.2567");
        lines.add(" Frc consts  --      1.6784                 1.2413                 1.1817");
        lines.add(" IR Inten    --     32.6278                 3.8434                69.5885");
        List<Integer> val = ParserHelper.extractNumbers(lines);
        int[] cmpVal = new int[]{1101, 3327, 1173, 4500, 1263, 3184,
            2, 3486, 1, 5301, 1, 2567,
            1, 6784, 1, 2413, 1, 1817,
            32, 6278, 3, 8434, 69, 5885};
        assert (!val.isEmpty());
        assert (val.size() == cmpVal.length);
        for (int i = 0; i < cmpVal.length; i++) {
            assert (val.contains(cmpVal[i]));
        }

        lines = new ArrayList<String>();
        lines.add("  Atom  AN      X      Y      Z        X      Y      Z        X      Y      Z");
        val = ParserHelper.extractNumbers(lines);
        assert (val.isEmpty());

        lines = new ArrayList<String>();
        lines.add("     1   6    -0.10   0.03  -0.02    -0.03  -0.04   0.11     0.05  -0.07  -0.01");
        val = ParserHelper.extractNumbers(lines);
        int[] cmpVal1 = new int[]{1, 6, 0, 10, 0, 3, 0, 2, 0, 3, 0, 4, 0, 11, 0, 5, 0, 7, 0, 1};
        assert (!val.isEmpty());
        assert (val.size() == cmpVal1.length);
        for (int i = 0; i < cmpVal1.length; i++) {
            assert (val.contains(cmpVal1[i]));
        }
    }

    @Test
    public void extractDoublesTest() {
        List<String> lines = new ArrayList<String>();
        lines.add(" Frequencies --   1101.3327              1173.4500              1263.3184");
        lines.add(" Red. masses --      2.3486                 1.5301                 1.2567");
        lines.add(" Frc consts  --      1.6784                 1.2413                 1.1817");
        lines.add(" IR Inten    --     32.6278                 3.8434                69.5885");
        List<Double> val = ParserHelper.extractDoubles(lines);
        double[] cmpVal = new double[]{1101.3327, 1173.4500, 1263.3184,
            2.3486, 1.5301, 1.2567,
            1.6784, 1.2413, 1.1817,
            32.6278, 3.8434, 69.5885};
        assert (!val.isEmpty());
        assert (val.size() == 12);
        for (int i = 0; i < cmpVal.length; i++) {
            assert (val.contains(cmpVal[i]));
        }

        lines = new ArrayList<String>();
        lines.add("  Atom  AN      X      Y      Z        X      Y      Z        X      Y      Z");
        val = ParserHelper.extractDoubles(lines);
        assert (val.isEmpty());

        lines = new ArrayList<String>();
        lines.add("     1   6    -0.10   0.03  -0.02    -0.03  -0.04   0.11     0.05  -0.07  -0.01");
        val = ParserHelper.extractDoubles(lines);
        double[] cmpVal1 = new double[]{-0.10,
            0.03, -0.02, -0.03, -0.04, 0.11, 0.05, -0.07, -0.01};
        assert (!val.isEmpty());
        assert (val.size() == 9);
        for (int i = 0; i < cmpVal1.length; i++) {
            assert (val.contains(cmpVal1[i]));
        }
    }

    @Test
    public void extractIntegersTest() {
        List<String> lines = new ArrayList<String>();

        lines.add("     1   6    -0.10   0.03  -0.02    -0.03  -0.04   0.11     0.05  -0.07  -0.01");
        List<Integer> val = ParserHelper.extractIntegers(lines);
        assert (!val.isEmpty());
        assert (val.size() == 2);
        assert (val.contains(1));
        assert (val.contains(6));

        lines = new ArrayList<String>();
        lines.add(" Frequencies --   1101.3327              1173.4500              1263.3184");
        lines.add(" Red. masses --      2.3486                 1.5301                 1.2567");
        lines.add(" Frc consts  --      1.6784                 1.2413                 1.1817");
        lines.add(" IR Inten    --     32.6278                 3.8434                69.5885");
        val = ParserHelper.extractIntegers(lines);
        assert (val.isEmpty());

        lines = new ArrayList<String>();
        lines.add("  Atom  AN      X      Y      Z        X      Y      Z        X      Y      Z");
        val = ParserHelper.extractIntegers(lines);
        assert (val.isEmpty());
    }
}
