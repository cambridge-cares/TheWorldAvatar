/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector;

import java.util.Map;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;

/**
 *
 * @author pb556
 */
public class RemoveFlaggedReactions extends ReactionSelector {

    protected Map<Species, String> flags = null;
    protected String flag = null;

    public RemoveFlaggedReactions(Map<Species, String> flags, String invalidFlag) {
        super();
        this.flags = flags;
        flag = invalidFlag;
    }

    public RemoveFlaggedReactions() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
    public void setProperties(Map<Species, String> flags, String invalidFlag) {
        this.flags = flags;
        flag = invalidFlag;
    }

    @Override
    public ReactionList select(ReactionList reactions) {
        if (flags != null && flag != null) {
            ReactionList newReactionList = new ReactionList();
            for (Reaction r : reactions) {
                boolean valid = true;
                for (Species reactant : r.getReactants().keySet()) {
                    for (Species s : flags.keySet()) {
                        if (s.equals(reactant, true) && flags.get(s).equalsIgnoreCase(flag)) {
                            valid = false;
                            break;
                        }
                    }
                    if (valid) {
                        for (Species s : flags.keySet()) {
                            if (s.equals(reactant, true) && flags.get(s).equalsIgnoreCase(flag)) {
                                valid = false;
                                break;
                            }
                        }
                    }
                    if (valid) {
                        newReactionList.add(r);
                    }
                }
            }
            if (newReactionList.size() != reactions.size()) {
                System.out.println("marked reactions removed");
            }
            return newReactionList;
        }
        return reactions;
    }
}
