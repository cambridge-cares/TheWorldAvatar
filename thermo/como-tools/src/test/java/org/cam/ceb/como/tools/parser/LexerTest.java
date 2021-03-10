/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.parser;

import java.util.Arrays;
import java.util.List;
import org.cam.ceb.como.tools.pattern.composite.Composite;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class LexerTest {

//    @Test
//    public void extractTokensTest() {
//        ChemFormulaLexer lexer = new ChemFormulaLexer();
//
//        List<Token> tokens1 = lexer.extractTokens("Si(OCH=CH2)(OCHCH3)(OH)2");
//        List<String> indivChemFormula = Arrays.asList(new String[] {"Si", "OCH=CH2", "OCHCH3", "OH"});
//        List<Integer> indivNumBranches = Arrays.asList(new Integer[] {0, 0, 0, 2});
//        assert (tokens1.size() == 4);
//        for (int i = 0; i < tokens1.size(); i++) {
//            assert (indivChemFormula.contains(tokens1.get(i).getChemicalFormula()));
//            for (int j = 0; j < tokens1.size(); j++) {
//                if (indivChemFormula.get(i).equals(indivChemFormula.get(j))) {
//                    assert(tokens1.get(i).getNumberOfBranches() == indivNumBranches.get(j));
//                }
//            }
//        }
//    }
    @Test
    @Ignore
    public void buildTest() throws Exception {
        Lexer lexer = new ChemFormulaLexer();
        lexer.setObject("1. Si(OCH=CH2)(OCHCH3)(OH)2\n2. Si(OCH=CH2)(OCHCH3)(OH)2");
        lexer.build();
        Composite comp = (Composite) lexer.getProduct();
        assert (comp.getChildren().size() == 4);

        // String object will be a token object containing the description as well 
        // as the number of repititions
        List<String> indivChemFormula = Arrays.asList(new String[]{"Si", "OCH=CH2", "OCHCH3", "OH"});
        List<Integer> indivNumBranches = Arrays.asList(new Integer[]{0, 1, 1, 2});
        for (int i = 0; i < comp.getChildren().size(); i++) {
            Token token = (Token) comp.getChildren().get(i).get();
            assert (indivChemFormula.contains(token.getChemicalFormula()));
            for (int j = 0; j < indivChemFormula.size(); j++) {
                if (indivChemFormula.get(j).equals(token.getChemicalFormula())) {
                    assert (token.getNumberOfBranches() == indivNumBranches.get(j));
                }
            }
        }
    }

    @Test
    public void fullBuildTest() throws Exception {
        
//        ChemFileReader reader = new WMLReader();
//        CompoundConverter converter = new CompoundConverter();
//        Lexer lexer = new ChemFormulaLexer();
//        StructureCreator creator = new StructureCreator();
//        
//        lexer.setObject("1. Si(OCH=CH2)(OCHCH3)(OH)2");
//        lexer.build();
//        converter.setComponent(reader.read("TEOS-3d.wml"));
//        converter.convert();
//        creator.extractStructure((Composite) lexer.getProduct(), (Compound) converter.getConvertedComponent());
//        
//        //Lexer lexer = new ChemFormulaLexer();
////        lexer.setObject("Si(OCH=CH2)(OCHCH3)(OH)2\nSi(OCH=CH2)HCTi(OCHCH3)(OH)2");
////        lexer.build();
////        Composite comp = (Composite) lexer.getProduct();
////        assert (comp.getChildren().size() == 2);
//        
//        String content = "18. Si(OC2H4)(OC2H5)2(OCH3)          \n" +
//        "19. Si(OC2H4)(OC2H5)2(OH)            \n" +
//        "20. Si(OC2H4)(OC2H5)3                \n" +
//        "21. Si(O)(OCH=CH2)(OCH3)2            \n" +
//        "22. Si(OCH=CH2)(OCHCH3)(OCH3)2        \n" + 
//        "23. Si(OCH=CH2)(OCHCH3)(OH)(OCH3)    \n" +
//        "24. Si(OCH=CH2)(OCHCH3)(OH)2  \n";
//        
//        lexer.setObject(content);
//        lexer.build();
//        Composite comp = (Composite) lexer.getProduct();
//        assert (comp.getChildren().size() == 7);
    }
}
