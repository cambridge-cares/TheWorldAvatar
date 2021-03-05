/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.chem.property;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.chem.structure.Bond;

/**
 *
 * @author pb556
 */
public class Vibrations extends ArrayList<Vibration> {
    // list of vibrations
    
    private Logger logger = Logger.getLogger(this.getClass());
    
    @Override
    public boolean add(Vibration e) {
//        if (e.getFrequency() <= 0.0) {
//            logger.warn("A frequency <= 0.0 cannot be defined!");
//            return false;
//        } else {
//            for (Vibration v : this) {
//                if (e.isSimilar(v)) {
//                    logger.warn("Vibrational mode already defined!");
//                    return false;
//                }
//            }
            return super.add(e); //To change body of generated methods, choose Tools | Templates.
//        }
    }

    @Override
    public void add(int index, Vibration element) {
//        for (Vibration v : this) {
//            if (element.isSimilar(v)) {
//                logger.warn("Vibrational mode already defined!");
//            }
//        }
        super.add(index, element); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public boolean addAll(Collection<? extends Vibration> c) {
//        for (Vibration v : this) {
//            for (Vibration e : c) {
//                if (e.isSimilar(v)) {
//                    logger.warn("At least one of the vibrational modes is already defined!");
//                    return false;
//                }
//            }
//        }
        return super.addAll(c); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public boolean addAll(int index, Collection<? extends Vibration> c) {
//        for (Vibration v : this) {
//            for (Vibration e : c) {
//                if (e.isSimilar(v)) {
//                    logger.warn("At least one of the vibrational modes is already defined!");
//                    return false;
//                }
//            }
//        }
        return super.addAll(index, c); //To change body of generated methods, choose Tools | Templates.
    }
    
    public List<Bond> clonedRotorBonds() {
        List<Bond> torsBonds = new ArrayList<Bond>();
        for (Vibration v : this) {
            if (v instanceof Rotation) {
                torsBonds.add(((Rotation) v).getTorsionalBond());
            }
        }
        return torsBonds;
    }

    public int getIRotorBondCount() {
        List<Bond> torsBonds = new ArrayList<Bond>();
        for (Vibration v : this) {
            if (v instanceof Rotation) {
                torsBonds.add(((Rotation) v).getTorsionalBond());
            }
        }
        return torsBonds.size();
    }

    public boolean removeAllIRotors() {
        List<Vibration> rotors = new ArrayList<Vibration>();
        for (Vibration v : this) {
            if (v instanceof Rotation) {
                rotors.add(v);
            }
        }
        return this.removeAll(rotors);
    }

    public boolean removeIRotor(Bond bond_axis) {
        List<Vibration> rotors = new ArrayList<Vibration>();
        for (Vibration v : this) {
            if (v instanceof Rotation) {
                if (bond_axis.equals(((Rotation) v).getTorsionalBond())) {
                    rotors.add(v);
                }
            }
        }
        return this.removeAll(rotors);
    }

    public boolean hasIRotorBond(Bond bond_axis) {
        List<Vibration> rotors = new ArrayList<Vibration>();
        for (Vibration v : this) {
            if (v instanceof Rotation) {
                if (bond_axis.equals(((Rotation) v).getTorsionalBond())) {
                    return true;
                }
            }
        }
        return false;
    }

    // for specific mode
    public boolean addIRotorBond(Vibration vib, Bond bond_axis) {
        if (this.contains(vib)) {
            // if it is already a rotation add it to it
            if (vib instanceof Rotation) {
                // CLONE and ADD clone
                Vibration clone = clone(vib);
                ((Rotation) clone).getTorsionalBond().equals(bond_axis);
                add(clone);
            } else {
                // CONVERT to Rotation object
                Rotation rot = convert(vib);
                rot.setTorsionalBond(bond_axis);
                // REMOVE vib
                if (remove(vib)) {
                    // ADD Rotation object
                    add(rot);
                    return true;
                }
                return false;
            }
        } else {
            // ADD Rotation object
            if (vib instanceof Rotation) {
                ((Rotation) vib).getTorsionalBond().equals(bond_axis);
                add(vib);
            } else {
                Rotation rot = convert(vib);
                rot.setTorsionalBond(bond_axis);
                add(rot);
            }
        }
        return false;
    }
    
    protected Vibration clone(Vibration vib) {
        return null;
    }
    
    protected Rotation convert(Vibration vib) {
        return null;
    }

    public List<Bond> clonedRotorBonds(Vibration vib) {
        List<Bond> torsBonds = new ArrayList<Bond>();
        for (Vibration v : this) {
            if (vib.isSimilar(v) && v instanceof Rotation) {
                torsBonds.add(((Rotation) v).getTorsionalBond());
            }
        }
        return torsBonds;
    }

    public int getIRotorBondCount(Vibration vib) {
        List<Bond> torsBonds = new ArrayList<Bond>();
        for (Vibration v : this) {
            if (vib.isSimilar(v) && v instanceof Rotation) {
                torsBonds.add(((Rotation) v).getTorsionalBond());
            }
        }
        return torsBonds.size();
    }

    public boolean removeAllIRotors(Vibration vib) {
        List<Vibration> vibs = new ArrayList<Vibration>();
        for (Vibration v : this) {
            if (vib.isSimilar(v) && v instanceof Rotation) {
                vibs.add(v);
            }
        }
        return vibs.removeAll(vibs);
    }

    public boolean removeIRotor(Vibration vib, Bond bond_axis) {
        for (Vibration v : this) {
            if (vib.isSimilar(v) && v instanceof Rotation && bond_axis.equals(((Rotation) v).getTorsionalBond())) {
                return remove(v);
            }
        }
        return false;
    }

    public boolean hasIRotorBond(Vibration vib, Bond bond_axis) {
        for (Vibration v : this) {
            if (vib.isSimilar(v) && v instanceof Rotation && bond_axis.equals(((Rotation) v).getTorsionalBond())) {
                return true;
            }
        }
        return false;
    }

    public enum Property {

        FREQUENCY, REDUCED_MASS, FORCE_CONSTANT, IR_INTEN, MODE, REDUCED_MOMENTS, DISPLACEMENTS
    }

    public void add(Property p, int index, double... value) {
        if (value.length < 1) {
            return;
        }
        if (index >= this.size()) {
            Vibration normalMode = new Vibration();
            switch (p) {
                case FORCE_CONSTANT:
                    normalMode.frcConst = value[0];
                    break;
                case FREQUENCY:
                    normalMode.freq = value[0];
                    break;
                case IR_INTEN:
                    normalMode.irInten = value[0];
                    break;
                case MODE:
                    normalMode.mode = (int) value[0];
                    break;
                case REDUCED_MASS:
                    normalMode.redMass = value[0];
                    break;
                case DISPLACEMENTS:
                    if (value.length == 3) {
//                        VibrationalNormalMode.XYZ xyz = normalMode.new XYZ();
//                        xyz.setX(value[0]);
//                        xyz.setY(value[1]);
//                        xyz.setZ(value[2]);
//                        normalMode.pos.add(xyz);
                        System.out.println("Parsing error during the extraction of the displacement values.");
                    } else {
                        // logger has to be added
                        System.out.println("Parsing error during the extraction of the displacement values.");
                    }
            }
            this.add(normalMode);
        } else {
            switch (p) {
                case FORCE_CONSTANT:
                    this.get(index).frcConst = value[0];
                    break;
                case FREQUENCY:
                    this.get(index).freq = value[0];
                    break;
                case IR_INTEN:
                    this.get(index).irInten = value[0];
                    break;
                case MODE:
                    this.get(index).mode = (int) value[0];
                    break;
                case REDUCED_MASS:
                    this.get(index).redMass = value[0];
                    break;
                case DISPLACEMENTS:
                    if (value.length == 3) {
                        //VibrationalNormalMode.XYZ xyz = normalMode.new XYZ();
                        //xyz.x = value[0];
                        //xyz.y = value[1];
                        //xyz.z = value[2];
                        //this.normalModes.get(index).pos.get(index)
                    } else {
                        // logger has to be added
                        System.out.println("Parsing error during the extraction of the displacement values.");
                    }
            }
        }
    }

    public void addAll(Property p, Collection<Double> values) {
        //if (this.normalModes.size() <= values.size()) {
        int i = this.size();
        for (Double val : values) {
            this.add(p, i, val);
            i++;
        }
//        } else {
//            // do not do anything
//        }
    }

    public void addAll(Property p, int index, Collection<Double> values) {
        int i = index;
        for (Double val : values) {
            this.add(p, i, val);
            i++;
        }
    }

    public List<Double> get(Property p) {
        List<Double> values = new ArrayList<Double>();
        for (int i = 0; i < this.size(); i++) {
            values.add(this.get(p, i));
        }
        return values;
    }

    public double get(Property p, int index) {
        switch (p) {
            case FORCE_CONSTANT:
                return this.get(index).frcConst;
            case FREQUENCY:
                return this.get(index).freq;
            case IR_INTEN:
                return this.get(index).irInten;
            case MODE:
                return this.get(index).mode;
            case REDUCED_MASS:
                return this.get(index).redMass;
        }
        return 0;
    }
    
    @Override
    public Object clone() {
        Vibrations vibs = new Vibrations();
        for (Vibration v : this) {
            try {
                vibs.add((Rotation) v.clone());
            } catch (CloneNotSupportedException ex) {
                logger.error("DisplacementList object could not be cloned!");
            }
        }
        return vibs;
    }
}