/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.periodictable;

import static org.cam.ceb.como.tools.periodictable.Orbital.d;
import static org.cam.ceb.como.tools.periodictable.Orbital.f;
import static org.cam.ceb.como.tools.periodictable.Orbital.p;
import static org.cam.ceb.como.tools.periodictable.Orbital.s;
import com.sun.media.sound.InvalidDataException;
import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public class OrbitalConfiguration {

    protected Orbital orbital = Orbital.undefined;
    protected int order = 0;
    protected int numElectrons = 0;
    public static Logger logger = Logger.getLogger(OrbitalConfiguration.class);

    public OrbitalConfiguration(int order, Orbital orbital, int numElectrons) {
        if (!verifyOrderAndOrbital(order, orbital) || ! verifyOrbitalAndNumberOfElectrons(orbital, numElectrons)) {
             logger.error("Invalid data were identified.", new InvalidDataException("Invalid data were identified."));
             return;
        }
        this.order = order;
        this.orbital = orbital;
        this.numElectrons = numElectrons;
    }
    
    public OrbitalConfiguration(int order, String strOrbital, int numElectrons) {
        Orbital convOrbital = getOrbital(strOrbital);
        if (!verifyOrderAndOrbital(order, convOrbital) || ! verifyOrbitalAndNumberOfElectrons(convOrbital, numElectrons)) {
             logger.error("Invalid data were identified.", new InvalidDataException("Invalid data were identified."));
             return;
        }
        this.order = order;
        this.orbital = convOrbital;
        this.numElectrons = numElectrons;
    }
    
    public int getOrder() {
        return order;
    }
    
    public int getNumberOfElectrons() {
        return numElectrons;
    }
    
    public Orbital getOrbital() {
        return orbital;
    }
    
    public final Orbital getOrbital(String strOrbital) {
        if (strOrbital.trim().toLowerCase().equals(Orbital.s.toString())) {
            return Orbital.s;
        } else if (strOrbital.trim().toLowerCase().equals(Orbital.p.toString())) {
            return Orbital.p;
        } else if (strOrbital.trim().toLowerCase().equals(Orbital.d.toString())) {
            return Orbital.d;
        } else if (strOrbital.trim().toLowerCase().equals(Orbital.f.toString())) {
            return Orbital.f;
        }
        return Orbital.undefined;
    }
    
    @Override
    public String toString() {
        return order + orbital.toString() + "^(" + numElectrons + ")";
    }

    private boolean verifyOrderAndOrbital(int order, Orbital orbital) {
        switch (orbital) {
            case s:
                return (order > 0 && order <= 7);
            case p:
                return (order > 1 && order <= 7);
            case d:
                return (order > 2 && order <= 6);
            case f:
                return (order > 3 && order <= 5);
        }
        return false;
    }
    
    private boolean verifyOrbitalAndNumberOfElectrons(Orbital orbital, int numElectrons) {
        switch (orbital) {
            case s:
                return (numElectrons >= 0 && numElectrons <= 2);
            case p:
                return (numElectrons >= 0 && numElectrons <= 6);
            case d:
                return (numElectrons >= 0 && numElectrons <= 10);
            case f:
                return (numElectrons >= 0 && numElectrons <= 14);
        }
        return false;
    }
}
