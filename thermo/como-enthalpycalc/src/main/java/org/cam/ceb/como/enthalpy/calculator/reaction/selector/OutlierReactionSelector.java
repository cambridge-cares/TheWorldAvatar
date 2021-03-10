/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.reaction.selector;

import java.util.ArrayList;
import java.util.HashMap;

import org.apache.log4j.Logger;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;

/**
 *
 * @author pb556
 * 
 */
public class OutlierReactionSelector extends ReactionSelector {

    private Logger logger = Logger.getLogger(OutlierReactionSelector.class);

    @Override
    public ReactionList select(ReactionList reactions) {
        ReactionList actualOutliers = new ReactionList();
        if (reactions.size() >= 5) {
            // removal of clear outliers from the reaction list
            HashMap<Reaction, ArrayList> outliers = new HashMap<Reaction, ArrayList>();
            HashMap<Reaction, Integer> ctr = new HashMap<Reaction, Integer>();
            for (int i = 0; i < reactions.size(); i++) {
                ctr.put(reactions.get(i), 0);
                outliers.put(reactions.get(i), identifyOutliers(reactions.get(i), reactions, 50));
            }

            // identify correct classification
            for (Reaction centre : outliers.keySet()) {
                ArrayList<Reaction> o = outliers.get(centre);
                for (int i = 0; i < o.size(); i++) {
                    ctr.put(o.get(i), ctr.get(o.get(i)) + 1);
                }
            }


            for (Reaction r : ctr.keySet()) {
                if (ctr.get(r) >= reactions.size() * 0.5) {
                    actualOutliers.add(r);
                }
            }
        } else {
            logger.warn("Sample size is too small to identify any outliers!");
        }

        ReactionList valid = new ReactionList();
        for (int i = 0; i < reactions.size(); i++) {
            boolean identified = false;
            for (int j = 0; j < actualOutliers.size(); j++) {
                if (reactions.get(i).equals(actualOutliers.get(j))) {
                    identified = true;
                    break;
                }
            }
            if (!identified) {
                valid.add(reactions.get(i));
            }
        }

        return valid;
    }

    public ArrayList<Reaction> identifyOutliers(Reaction centre, ReactionList reactions, double tol) {
        ArrayList<Reaction> outliers = new ArrayList<Reaction>();
        for (int i = 0; i < reactions.size(); i++) {
            double refHf = centre.calculateHf();
            if (!reactions.get(i).equals(centre)) {
                double hf = reactions.get(i).calculateHf();
                if (hf < refHf - tol || hf > refHf + tol) {
                    outliers.add(reactions.get(i));
                }
            }
        }
        return outliers;
    }
}
