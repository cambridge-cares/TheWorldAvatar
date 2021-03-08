/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.results;

import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ResultsValueDistributionListTest {
    
    @Test
    public void toStringTest() throws Exception {
        ResultsData data = MockResultsData.getC2H4ResultsData("resDataId", "Comment", 1);
        
        String d = RealMockReactionsC2H4.getSolution1().calculateHf() + ", ";
        d += RealMockReactionsC2H4.getSolution2().calculateHf() + ", ";
        d += RealMockReactionsC2H4.getSolution3().calculateHf() + ", ";
        d += RealMockReactionsC2H4.getSolution4().calculateHf() + ", ";
        d += RealMockReactionsC2H4.getSolution5().calculateHf();
        String dist = "\"{[" + d + "]}\"";
        //assert (data.getValueDistributions().toString().compareToIgnoreCase(dist) == 0);
    }
}
