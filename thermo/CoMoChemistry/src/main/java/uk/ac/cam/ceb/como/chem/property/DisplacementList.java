/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.chem.property;

import java.util.ArrayList;
import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public class DisplacementList extends ArrayList<Displacement> {
    
    private Logger logger = Logger.getLogger(this.getClass());
    
    @Override
    public Object clone() {
        ArrayList<Displacement> disp = new ArrayList<Displacement>();
        for (Displacement d : this) {
            try {
                disp.add((Displacement) d.clone());
            } catch (CloneNotSupportedException ex) {
                logger.error("DisplacementList object could not be cloned!");
            }
        }
        return disp;
    }
}
