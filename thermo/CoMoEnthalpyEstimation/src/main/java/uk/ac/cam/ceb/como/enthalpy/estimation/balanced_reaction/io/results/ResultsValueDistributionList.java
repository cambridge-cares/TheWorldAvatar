/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.results;

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
