/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.results;

import java.util.ArrayList;

/**
 *
 * @author pb556
 */
public class ResultsValueDistributionList extends ArrayList<ResultsValueDistribution> {
    
    @Override
    public String toString() {
        String str = "{";
        for (ResultsValueDistribution d : this) {
            str += d.toString();
        }
        str += "}";
        return str;
    }
}
