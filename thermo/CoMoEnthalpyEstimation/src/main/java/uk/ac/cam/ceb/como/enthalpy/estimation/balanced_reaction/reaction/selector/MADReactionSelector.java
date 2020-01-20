/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector;

import java.util.logging.Level;
import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.math.average.Mean;
import uk.ac.cam.ceb.como.math.variability.MadianAbsoluteDeviation;

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
