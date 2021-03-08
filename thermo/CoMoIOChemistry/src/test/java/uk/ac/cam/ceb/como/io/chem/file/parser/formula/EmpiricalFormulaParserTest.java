package uk.ac.cam.ceb.como.io.chem.file.parser.formula;

import uk.ac.cam.ceb.como.io.chem.file.parser.formula.EmpiricalFormulaParser;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class EmpiricalFormulaParserTest {

    /**
     * Test of parse method, of class EmpiricalFormulaParser.
     */

	@Test
    public void testParse() {
        System.out.println("parse");
        
        EmpiricalFormulaParser empParser = new EmpiricalFormulaParser();

        String result_1 = empParser.parse("Cl(2)");
        System.out.println(result_1);
        
//        String result_2 = empParser.parse("CH3(CH3)");
//        System.out.println(result_2);

        //C29Cl20H15Ti40
//        result = empParser.parse("H:15,C:29,Ti:40,Cl:20");
//        result = empParser.parse("H:0,C:29,Ti:40,Cl:20");
//        result = empParser.parse("H:*,C:29,Ti:40,Cl:20");
    }
}