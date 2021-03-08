/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.reaction.selector;

import java.util.logging.Level;
import org.apache.log4j.Logger;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.math.average.Mean;
import org.cam.ceb.como.math.variability.MadianAbsoluteDeviation;

/**
 *
 * @author pb556
 */
public class MADReactionSelector extends ReactionSelector {

    private Logger logger = Logger.getLogger(MADReactionSelector.class);

    protected double zScoreThreshold = 3.5;
    
    public MADReactionSelector() {}
    
    public MADReactionSelector(double zScoreThreshold) {
        this.zScoreThreshold = zScoreThreshold;
    }
    
    @Override
    public ReactionList select(ReactionList reactions) {
        ReactionList notAnOutliers = new ReactionList();
        if (reactions.size() >= 5) {
            Mean m = new Mean();
            double[] values = new double[reactions.size()];
            for (int i = 0; i < reactions.size(); i++) {
                values[i] = reactions.get(i).calculateHf();
            }
            try {
                MadianAbsoluteDeviation v = new MadianAbsoluteDeviation();
                double mean = m.calculate(values);
                double mad = v.calculate(values);
                for (Reaction r : reactions) {
                    if (getZScore(r, mean, mad) <= zScoreThreshold) {
                        notAnOutliers.add(r);
                    }
                }
            } catch (Exception ex) {
                java.util.logging.Logger.getLogger(MADReactionSelector.class.getName()).log(Level.SEVERE, null, ex);
            }

        } else {
            logger.warn("Sample size is too small to identify any outliers!");
        }
        return notAnOutliers;
    }
    
    public ReactionList selectOutliers(ReactionList reactions) {
        ReactionList actualOutliers = new ReactionList();
        if (reactions.size() >= 5) {
            Mean m = new Mean();
            double[] values = new double[reactions.size()];
            for (int i = 0; i < reactions.size(); i++) {
                values[i] = reactions.get(i).calculateHf();
            }
            try {
                MadianAbsoluteDeviation v = new MadianAbsoluteDeviation();
                double mean = m.calculate(values);
                double mad = v.calculate(values);
                for (Reaction r : reactions) {
                    if (getZScore(r, mean, mad) > zScoreThreshold) {
                        actualOutliers.add(r);
                    }
                }
            } catch (Exception ex) {
                java.util.logging.Logger.getLogger(MADReactionSelector.class.getName()).log(Level.SEVERE, null, ex);
            }

        } else {
            logger.warn("Sample size is too small to identify any outliers!");
        }
        return actualOutliers;
    }

    protected double getZScore(Reaction r, double mean, double mad) {
        return 0.6745 * (r.calculateHf() - mean) / mad;
    }
}
