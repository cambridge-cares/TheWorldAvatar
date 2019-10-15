/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.results;

import org.cam.ceb.como.enthalpy.calculator.MockSpecies;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ResultsDataTest {
    
    protected double tolerance = 0.01;
    
    @Test
    public void getterTest() throws Exception {
        ResultsData data = MockResultsData.getC2H4ResultsData("resDataId", "Comment", 1);
        
        assert (data.getId().compareToIgnoreCase("resDataId") == 0);
        assert (data.getComment().compareToIgnoreCase("Comment") == 0);
        assert (Math.abs(MockSpecies.getC2H4().getTotalEnergy() - data.getEnergy()) < tolerance);
        assert (Math.abs(MockSpecies.getC2H4().getHf() - data.getHf()) < tolerance);
        assert (data.getSpecies().equals(MockSpecies.getC2H4(), true));
        
        String d = RealMockReactionsC2H4.getSolution1().calculateHf() + ", ";
        d += RealMockReactionsC2H4.getSolution2().calculateHf() + ", ";
        d += RealMockReactionsC2H4.getSolution3().calculateHf() + ", ";
        d += RealMockReactionsC2H4.getSolution4().calculateHf() + ", ";
        d += RealMockReactionsC2H4.getSolution5().calculateHf();
        String dist = "\"{[" + d + "]}\"";
        //assert (data.getValueDistributions().toString().compareToIgnoreCase(dist) == 0);
    }
    
    @Test
    public void toStringTest() throws Exception {
        ResultsData data = MockResultsData.getC2H4ResultsData("resDataId", "Comment", 1);
        assert(data.toString().compareToIgnoreCase("\"" + 
                data.getId() + "\", \"" + 
                data.getSpecies().getRef() + "\", \"" + 
                data.getSpecies().getTotalEnergy() + "\", \"" + 
                data.getSpecies().getHf() + "\", \"" + 
                data.getValueDistributions().toString() + "\", \"" + 
                data.getComment() + "\"") == 0);
    }
    
    
}
