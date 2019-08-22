/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.parser;

import org.cam.ceb.como.tools.pattern.composite.Composite;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ParserTest {
    
    @Test
    public void parseTest() throws Exception{
        Parser parser = new ChemFormulaParser();
        parser.setLexer(new ChemFormulaLexer());
        parser.setObject("1. Si(OCH=CH2)(OCHCH3)(OH)2\n2. Si(OCH=CH2)(OCHCH3)(OH)2");
        parser.build();
        Composite composite = (Composite) parser.getProduct();
        System.out.println();
    }
}
