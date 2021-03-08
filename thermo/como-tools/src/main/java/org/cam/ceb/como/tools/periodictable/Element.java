/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.periodictable;

import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public class Element {

    protected ElectronConfiguration eConfig;
    protected String name;
    protected String symbol;
    protected int atomicNumber;
    protected ElementCategory eCat;
    protected int massNumber;
    protected int group;
    protected int period;
    protected Block block;
    protected PhysicalProperties physProperties;
    protected AtomicProperties atomicProperties;
    private Logger logger = Logger.getLogger(Element.class);

    public Element() {
    }

    public Element(String symbol, String name) {
        this.symbol = symbol.trim();
        this.name = name.trim();
    }

    public Element(String symbol, String name, int atomicNumber, int massNumber) {
        this.symbol = symbol.trim();
        this.name = name.trim();
        this.atomicNumber = atomicNumber;
        this.massNumber = massNumber;
    }

    public Element(String symbol, String name, int atomicNumber, int massNumber, int period, int group) {
        this.symbol = symbol.trim();
        this.name = name.trim();
        this.atomicNumber = atomicNumber;
        this.massNumber = massNumber;
        this.group = group;
        this.period = period;
        // determine the block automatically
        block = getBlock(period, group);
    }

    public void setElectronConfiguration(ElectronConfiguration config) {
        eConfig = config;
    }

    public void setName(String name) {
        this.name = name.trim();
    }

    public void setSymbol(String symbol) {
        this.symbol = symbol.trim();
    }

    public void setAtomicCharacteristics(int atomicNumber, int massNumber) {
        if (atomicNumber < 0) {
            logger.error("The atomic number cannot be negative.");
        }
        if (massNumber < 0) {
            logger.error("The mass number cannot be negative.");
        }
        this.atomicNumber = atomicNumber;
        this.massNumber = massNumber;
    }

    public void setElementCategory(ElementCategory cat) {
        eCat = cat;
    }

    public void setBlock(Block block) {
        this.block = block;
    }

    public void setPosition(int period, int group) {
        if (period < 1 || period > 7) {
            logger.error("The defined period (" + period + ") is invalid.");
        }
        if (group < 0 || group > 18) {
            logger.error("The defined group (" + group + ") is invalid.");
        }
        this.group = group;
        this.period = period;
    }

    public void setPhysicalProperties(PhysicalProperties prop) {
        physProperties = prop;
    }

    public void setAtomicProperties(AtomicProperties prop) {
        atomicProperties = prop;
    }

    public String getName() {
        return name;
    }

    public String getSymbol() {
        return symbol;
    }

    public int getAtomicNumber() {
        return atomicNumber;
    }

    public int getMassNumber() {
        return massNumber;
    }

    public int getNumberOfNeutrons() {
        return massNumber - atomicNumber;
    }

    public int getGroup() {
        return group;
    }

    public int getPeriod() {
        return period;
    }

    public Block getBlock() {
        return block;
    }

    public ElectronConfiguration getElectronConfiguration() {
        return eConfig;
    }

    public ElementCategory getElementCategory() {
        return eCat;
    }

    public PhysicalProperties getPhysicalProperties() {
        return physProperties;
    }

    public final Block getBlock(int period, int group) {
        if (group == 1 || group == 2 || (group == 18 && period == 1)) {
            return Block.sBlock;
        } else if (group >= 3 && group <= 12) {
            return Block.dBlock;
        } else if (period > 1 && (group >= 13 && group <= 18)) {
            return Block.pBlock;
        } else if (group == 0) {
            return Block.fBlock;
        }
        return Block.undefined;
    }

    public int getNumberOfPairedElectrons() {
        int num = 0;
        for (OrbitalConfiguration orbital : eConfig.getOrbitals()) {
            //use the order which defines the row in which the element is placed!
            int maxElectrons = (orbital.getOrder() - 1) * orbital.getOrbital().getMaxNumberOfElectrons() + orbital.getNumberOfElectrons();
            int numElectrons = (orbital.getOrder() - 1) * orbital.getNumberOfElectrons() + orbital.getNumberOfElectrons();
//            if (orbital.getNumberOfElectrons() < orbital.getOrbital().getMaxNumberOfElectrons() / 2) {
//                num += 0;
//            }
            int max = orbital.getOrbital().getMaxNumberOfElectrons();
            int n = orbital.getNumberOfElectrons();
            int a = 2 * ((max / 2) - (max - n));
            if ((max / 2) - (max - n) > 0) {
                num += 2 * ((max / 2) - (max - n));
            }
            
            //num += 2 * (orbital.getOrbital().getMaxNumberOfElectrons() / 2 - (orbital.getOrbital().getMaxNumberOfElectrons() - orbital.getNumberOfElectrons()));
        }
        return num;
        
        
        
        
//        int num = 0;
//        for (OrbitalConfiguration orbital : eConfig.getOrbitals()) {
//            //use the order which defines the row in which the element is placed!
//            int numElectrons = (orbital.getOrder() - 1) * orbital.getOrbital().getMaxNumberOfElectrons() + orbital.getNumberOfElectrons();
//            if (orbital.getNumberOfElectrons() < orbital.getOrbital().getMaxNumberOfElectrons() / 2) {
//                num += 0;
//            }
//            
//            
//            
//            
//            int max = orbital.getOrbital().getMaxNumberOfElectrons();
//            int n = orbital.getNumberOfElectrons();
//            num += 2 * ((max / 2.0) - (max - n));
//            //num += 2 * (orbital.getOrbital().getMaxNumberOfElectrons() / 2 - (orbital.getOrbital().getMaxNumberOfElectrons() - orbital.getNumberOfElectrons()));
//        }
//        return num;
    }

    public int getNumberOfUnpairedElectrons() {
        // block;
        OrbitalConfiguration orbital = eConfig.getLastOrbital();
        if (orbital.getNumberOfElectrons() < orbital.getOrbital().getMaxNumberOfElectrons() / 2) {
            return orbital.getNumberOfElectrons();
        }
        int max = orbital.getOrbital().getMaxNumberOfElectrons();
        int num = orbital.getNumberOfElectrons();
        return orbital.getOrbital().getMaxNumberOfElectrons() - orbital.getNumberOfElectrons();
    }
    
    public int getNumberOfUnpairedElectrons(boolean singleAtom) {
        if (singleAtom) {
            return getNumberOfUnpairedElectrons();
        }
        // block;
        OrbitalConfiguration orbital = getElectronConfiguration().getLastOrbital();
        
                int max = orbital.getOrbital().getMaxNumberOfElectrons();
        int num = orbital.getNumberOfElectrons();
        
        if (orbital.getNumberOfElectrons() < orbital.getOrbital().getMaxNumberOfElectrons() / 2) {
            
            return (orbital.getOrbital().getMaxNumberOfElectrons() / 2 - orbital.getNumberOfElectrons()) * 2 + orbital.getNumberOfElectrons();
        }
        return orbital.getOrbital().getMaxNumberOfElectrons() - orbital.getNumberOfElectrons();
    }
}
