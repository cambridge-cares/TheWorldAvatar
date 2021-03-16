/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.xmlcml.cml.base.CMLElement;
import org.xmlcml.cml.element.CMLArray;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLModule;
import org.xmlcml.cml.element.CMLMolecule;
import org.xmlcml.cml.element.CMLProperty;
import org.xmlcml.cml.element.CMLPropertyList;
import org.xmlcml.cml.element.CMLTable;
import uk.ac.cam.ceb.como.chem.property.Displacement;
import uk.ac.cam.ceb.como.chem.property.DisplacementList;
import uk.ac.cam.ceb.como.chem.property.Vibration;
import uk.ac.cam.ceb.como.chem.property.Vibrations;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.compchem.CompChemWrapper;
import uk.ac.cam.ceb.como.math.constant.PhysicalConstants;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation.RotationalMode;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation.RotationalModes;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation.Top;

/**
 *
 * @author pb556
 */

public class IRCompChemWrapper extends CompChemWrapper {
	
    //same job as HinderedRotorCMLParser but no parsing is required

    private final String AXIS_X = "x3";
    private final String AXIS_Y = "y3";
    private final String AXIS_Z = "z3";
    
    public IRCompChemWrapper(CompChem compchem) {
        super(compchem);
    }

    public IRCompChemWrapper(CompChem compchem, CMLModule job) {
        super(compchem, job);
    }

    protected String getPropertyValue(CMLPropertyList properties, String name) throws Exception {
        List<String> val = getPropertyValues(properties, name);
        if (val == null || val.size() != 1) {
            throw new Exception("Invalid number of property values identified.");
        }
        return val.get(0).trim();
    }

    protected Vibration getMode(int mode) throws Exception {
        Vibrations vibModes = getVibrationalNormalModes();
        for (int i = 0; i < vibModes.size(); i++) {
            if (mode == vibModes.get(i).getMode()) {
                return vibModes.get(i);
            }
        }
        throw new Exception("Vibrational normal mode could not be identified.");
    }

    protected CMLBond getTorsionalBond(CMLPropertyList properties) throws Exception {
        String[] items = properties.getDictRef().split("_");
        if (items.length != 3) {
            throw new Exception("Torsional bond " + properties.getDictRef() + " cannot be identified.");
        }
        String a1 = items[1];
        String a2 = items[2];
        // get bond from the molecule
        for (CMLBond b : super.getFinalMolecule().getBonds()) {
            if ((b.getAtom(0).getId().equals(a1) && b.getAtom(1).getId().equals(a2))
                    || (b.getAtom(1).getId().equals(a1) && b.getAtom(0).getId().equals(a2))) {
                if (b.getAtom(0) == null || b.getAtom(1) == null) {
                    throw new Exception("Bond " + properties.getDictRef() + " cannot be identified.");
                }
                return b;
            }
        }
        throw new Exception("Torsional bond " + properties.getDictRef() + " cannot be identified.");
    }

    protected List<CMLAtom> getTopComposition(CMLPropertyList properties) throws Exception {
        List<CMLProperty> props = properties.getPropertyDescendantsByName("cc:top_composition");
        if (!props.isEmpty()) {
            CMLArray array = (CMLArray) props.get(props.size() - 1).getArrayElements().get(0);
            ArrayList<CMLAtom> atoms = new ArrayList<CMLAtom>();
            for (int i = 0; i < array.getArraySize(); i++) {
                String id = array.getElementAt(i).getValue();
                boolean added = false;
                for (CMLAtom a : super.getFinalMolecule().getAtoms()) {
                    if (a.getId().equals(id)) {
                        atoms.add(a);
                        added = true;
                        break;
                    }
                }
                if (!added) {
                    throw new Exception("Atom " + id + " cannot be identified.");
                }
            }
            return atoms;
        }
        throw new Exception("Top composition cannot be identified.");
    }

    public RotationalMode getRotationalMode(CMLPropertyList properties, boolean inclTorsBondAtom) throws Exception {
        RotationalMode mode = getRotationalMode(properties);
        if (!inclTorsBondAtom) {
            return mode;
        }
        // add single atom
        CMLMolecule ref = mode.getTop().getReferenceGeometry();
        CMLBond bond = mode.getTop().getBond();
        if (ref != null && bond != null) {
            for (CMLBond b : ref.getBonds()) {
                if (b.equals(bond)) {
                    continue;
                }
                CMLAtom bondAtom = null;
                CMLAtom oppBondAtom = null;
                if (bond.getAtom(0).equals(b.getAtom(0)) || bond.getAtom(1).equals(b.getAtom(0))) {
                    bondAtom = b.getAtom(1);
                    oppBondAtom = b.getAtom(0);
                } else if (bond.getAtom(0).equals(b.getAtom(1)) || bond.getAtom(1).equals(b.getAtom(1))) {
                    bondAtom = b.getAtom(0);
                    oppBondAtom = b.getAtom(1);
                }
                if (bondAtom != null) {
                    boolean identified = false;
                    for (CMLAtom a : mode.getTop().getAtoms()) {
                        if (a.equals(bondAtom)) {
                            identified = true;
                            break;
                        }
                    }
                    if (identified) {
                        mode.getTop().getAtoms().add(oppBondAtom);
                        break;
                    }
                }
            }
        }
        return mode;
    }

    public RotationalMode getRotationalMode(CMLPropertyList properties) throws Exception {
        //TorsionalBond bond = new TorsionalBond();
        RotationalMode mode = new RotationalMode();
        mode.setFrequency(Double.parseDouble(getPropertyValue(properties, "cc:internal_rotational_frequency")));
        mode.setReducedMoment(Double.parseDouble(getPropertyValue(properties, "cc:reduced_moment")));
        mode.setSymmetryNumber(Integer.parseInt(getPropertyValue(properties, "cc:symmetry")));
        mode.setMultiplicity(Integer.parseInt(getPropertyValue(properties, "cc:multiplicity")));
        mode.setPeriodicity(Integer.parseInt(getPropertyValue(properties, "cc:periodicity")));
        mode.setVibrationalNormalMode(getMode(Integer.parseInt(getPropertyValue(properties, "cc:vibrational_mode"))));
        List<CMLAtom> atoms = getTopComposition(properties);
        CMLBond bond = getTorsionalBond(properties);
        mode.setTop(new Top(super.getFinalMolecule(), atoms, bond));
        return mode;
    }
    
    public double getTotalEnergy() throws Exception {
        return getTotalEnergy(1.0);
    }
    
    public double getTotalEnergy(double scalingFactor) throws Exception {
        return getSCFEnergy() + getZPE(scalingFactor);
    }
    
    public double getG09ZeroPointEnergy() throws Exception {
        List<CMLProperty> properties = getProperties("cc:zpe");
        // get last property (newest!)
        if (properties.isEmpty()) {
            return 1.0;
        }
        CMLProperty propSCF = properties.get(0);
        if (propSCF.getScalarElements().size() == 0) {
            return 1.0;
        }
        return Double.parseDouble(propSCF.getScalarElements().get(0).getValue());
    }
    
    public double getG09ThermalEnergy() throws Exception {
        List<CMLProperty> properties = getProperties("cc:thermalenergy");
        // get last property (newest!)
        if (properties.isEmpty()) {
            return 1.0;
        }
        CMLProperty propSCF = properties.get(0);
        if (propSCF.getScalarElements().size() == 0) {
            return 1.0;
        }
        return Double.parseDouble(propSCF.getScalarElements().get(0).getValue());
    }
    
    public double getSCFEnergy() throws Exception {
        List<CMLProperty> properties = getProperties("cc:scfenergy");
        // get last property (newest!)
        if (properties.isEmpty()) {
            return 1.0;
        }
        CMLProperty propSCF = properties.get(0);
        if (propSCF.getScalarElements().size() == 0) {
            return 1.0;
        }
        return Double.parseDouble(propSCF.getScalarElements().get(0).getValue());
    }
    
    public double getZPE() throws Exception {
        return getZPE(1.0);
    }
    
    public double getZPE(double scalingFactor) throws Exception {
        Vibrations modes = getVibrationalNormalModes();
        double zpe = 0.0;
        for (int i = 0; i < modes.size(); i++) {
            double theta = (modes.get(i).getFrequency() * scalingFactor)
                    / (PhysicalConstants.k_B / (PhysicalConstants.h * 100 * PhysicalConstants.c));
            zpe += theta / 2;
        }
        return zpe * PhysicalConstants.R / (PhysicalConstants.NA * 4.3597439422e-18);
    }

    // SPLITTING INTO SMALLER PIECES IS REQUIRED!!!
    public RotationalModes getRotationalModes(boolean inclTorsBondAtom) throws Exception {
        List<CMLProperty> properties = getProperties("cc:hindered_rotations");
        // get last property (newest!)
        if (properties.isEmpty()) {
            return new RotationalModes();
        }
        CMLProperty propHR = properties.get(0);
        RotationalModes modes = new RotationalModes();
        String freqUnit = "";
        String redMomentUnit = "";
        for (CMLElement element : propHR.getChildCMLElements()) {
            RotationalMode mode = getRotationalMode((CMLPropertyList) element, inclTorsBondAtom);
            if (mode == null) {
                throw new Exception("Rotational mode could not be identified.");
            } else {
                List<String> attrValuesFreq = getPropertyValueAttributes((CMLPropertyList) element, "cc:internal_rotational_frequency", "units");
                List<String> attrValuesRedMoment = getPropertyValueAttributes((CMLPropertyList) element, "cc:reduced_moment", "units");
                if (attrValuesFreq != null) {
                    String bUnit = verifyRotationalModeUnit(attrValuesFreq);
                    if (!modes.isEmpty()) {
                        if (freqUnit.compareToIgnoreCase(bUnit) != 0) {
                            throw new Exception("An inconsistency with the units has been identified.");
                        }
                    } else {
                        freqUnit = bUnit;
                    }
                }
                if (attrValuesRedMoment != null) {
                    String bUnit = verifyRotationalModeUnit(attrValuesRedMoment);
                    if (!modes.isEmpty()) {
                        if (redMomentUnit.compareToIgnoreCase(bUnit) != 0) {
                            throw new Exception("An inconsistency with the units has been identified.");
                        }
                    } else {
                        redMomentUnit = bUnit;
                    }
                }
                modes.add(mode);
            }
        }
        modes.setFrequencyUnit(freqUnit);
        modes.setReducedMomentUnit(redMomentUnit);
        return modes;
    }

    public RotationalModes getRotationalModes() throws Exception {
        return getRotationalModes(false);
    }

    private String verifyRotationalModeUnit(List<String> attrValues) throws Exception {
        String bUnit = attrValues.get(0);
        for (String unit : attrValues) {
            if (unit.trim().compareToIgnoreCase(bUnit) != 0) {
                throw new Exception("An inconsistency with the units has been identified.");
            }
        }
        return bUnit;
    }

    public Vibrations getVibrationalNormalModesWithoutDisplacements() throws Exception {
        Vibrations modes = new Vibrations();

        List<CMLProperty> properties = getProperties("cc:vibrations");
        // get last property (newest!)
        CMLProperty propVib = properties.get(properties.size() - 1);
        HashMap<String, List<String>> vibArrays = getCMLArrayValues((CMLTable) propVib.getChildCMLElements().get(0));
        //HashMap<String, String> vibUnits = getCMLAttributeValues((CMLTable) propVib.getChildCMLElements().get(0), "units");
        //modes.setUnits(vibUnits.get("cc:frequencies"), vibUnits.get("cc:reduced_masses"), vibUnits.get("cc:ir_internal"), vibUnits.get("cc:force_consts"), "nonSi:Angstroms");

        if (vibArrays.isEmpty()) {
            return modes;
        }

        int length = -1;
        for (String dictRef : vibArrays.keySet()) {
            if (length == -1) {
                length = vibArrays.get(dictRef).size();
                continue;
            }
            if (vibArrays.get(dictRef).size() != length) {
                throw new Exception("Invalid syntax found in cc:vibrations.");
            }
        }

        for (int i = 0; i < length; i++) {
            modes.add(new Vibration());
        }

        for (String dictRef : vibArrays.keySet()) {
            if (dictRef.equals("cc:labels")) {
                List<String> val = vibArrays.get(dictRef);
                for (int i = 0; i < val.size(); i++) {
                    modes.get(i).setMode(Integer.parseInt(val.get(i).substring(1)));
                }
            } else if (dictRef.equals("cc:frequencies")) {
                List<String> val = vibArrays.get(dictRef);
                for (int i = 0; i < val.size(); i++) {
                    modes.get(i).setFrequency(Double.parseDouble(val.get(i)));
                }
            } else if (dictRef.equals("cc:ir_internal")) {
                List<String> val = vibArrays.get(dictRef);
                for (int i = 0; i < val.size(); i++) {
                    modes.get(i).setIRInten(Double.parseDouble(val.get(i)));
                }
            } else if (dictRef.equals("cc:reduced_masses")) {
                List<String> val = vibArrays.get(dictRef);
                for (int i = 0; i < val.size(); i++) {
                    modes.get(i).setReducedMass(Double.parseDouble(val.get(i)));
                }
            } else if (dictRef.equals("cc:force_consts")) {
                List<String> val = vibArrays.get(dictRef);
                for (int i = 0; i < val.size(); i++) {
                    modes.get(i).setForceConstant(Double.parseDouble(val.get(i)));
                }
            }
        }
        return modes;
    }

    public Vibrations getVibrationalNormalModes() throws Exception {
        Vibrations modes = getVibrationalNormalModesWithoutDisplacements();

        try {
            List<CMLProperty> properties = getProperties("cc:vibrational_displacements");
            CMLProperty propVib = properties.get(properties.size() - 1);
            HashMap<String, List<String>> vibDispArrays = getCMLArrayValues((CMLTable) propVib.getChildCMLElements().get(0));

            if (vibDispArrays.isEmpty()) {
                return modes;
            }

            int length = -1;
            for (String dictRef : vibDispArrays.keySet()) {
                if (length == -1) {
                    length = vibDispArrays.get(dictRef).size();
                    continue;
                }
                if (vibDispArrays.get(dictRef).size() != length) {
                    throw new Exception("Invalid syntax found in cc:vibrational_displacements.");
                }
            }

            if ((vibDispArrays.size() - 1) % 3 != 0 || ((int) ((vibDispArrays.size() - 1) / 3)) != modes.size()) {
                throw new Exception("Invalid syntax found in cc:vibrational_displacements.");
            }
            for (String dictRef : vibDispArrays.keySet()) {
                // get the displacements if they are available!!!
                if (dictRef.contains("cc:atoms")) {
                    List<String> val = vibDispArrays.get(dictRef);
                    for (int j = 0; j < modes.size(); j++) {
                        for (int i = 0; i < val.size(); i++) {
                            if (i == 0) {
                                modes.get(j).setDisplacements(new DisplacementList());
                            }
                            modes.get(j).getDisplacements().add(i, new Displacement());
                            modes.get(j).getDisplacements().get(i).setAtomReference(val.get(i));
                        }
                    }
                } else if (dictRef.startsWith("cc:displacement_f")) {
                    try {
                        int num = Integer.parseInt(dictRef.replace("cc:displacement_f", "").split("_")[0]) - 1;
                        String axis = dictRef.replace("cc:displacement_f", "").split("_")[1];
                        List<String> val = vibDispArrays.get(dictRef);
                        if (modes.get(num).getDisplacements() == null) {
                            modes.get(num).setDisplacements(new DisplacementList());
                        }
                        for (int i = 0; i < val.size(); i++) {
                            if (modes.get(num).getDisplacements().size() <= i) {
                                modes.get(num).getDisplacements().add(i, new Displacement());
                            }
                            if (modes.get(num).getDisplacements().get(i) == null) {
                                modes.get(num).getDisplacements().add(i, new Displacement());
                            }
                            if (axis.compareToIgnoreCase(AXIS_X) == 0) {
                                modes.get(num).getDisplacements().get(i).setdxInA(Double.parseDouble(val.get(i)));
                            } else if (axis.compareToIgnoreCase(AXIS_Y) == 0) {
                                modes.get(num).getDisplacements().get(i).setdyInA(Double.parseDouble(val.get(i)));
                            } else if (axis.compareToIgnoreCase(AXIS_Z) == 0) {
                                modes.get(num).getDisplacements().get(i).setdzInA(Double.parseDouble(val.get(i)));
                            }
                        }
                    } catch (NumberFormatException nfe) {
                    }
                } else {
                    throw new Exception("Invalid property found in cc:vibrational_displacements.");
                }
            }
        } catch (Exception e) {
        }
        return modes;
    }

    protected List<CMLProperty> getProperties(String name) throws Exception {
        CMLPropertyList list = this.getPropertyList();
        if (list != null) {
            return list.getPropertyDescendantsByName(name);
        } else {
            throw new Exception("No properties found.");
        }
    }

    protected List<String> getPropertyValues(CMLPropertyList prop, String name) {
        try {
            List<CMLProperty> props = prop.getPropertyDescendantsByName(name);
            ArrayList<String> values = new ArrayList<String>();
            if (!props.isEmpty()) {
                for (int i = 0; i < props.size(); i++) {
                    values.add(props.get(i).getValue());
                }
                return values;
            }
        } catch (NullPointerException npe) {
        }
        return null;
    }

//    protected List<Map<String, String>> getPropertyAttributes(CMLPropertyList prop) {
//        List<Map<String, String>> attrVals = new ArrayList<Map<String, String>>();
//        List<CMLProperty> props = prop.getPropertyDescendants();
//        if (!props.isEmpty()) {
//            for (int i = 0; i < props.size(); i++) {
//                attrVals.add(getPropertyAttributes(props.get(i)));
//            }
//        }
//        return attrVals;
//    }
//    protected List<Map<String, String>> getPropertyValueAttributes(CMLPropertyList prop) {
//        xxx
//        List<Map<String, String>> attrVals = new ArrayList<Map<String, String>>();
//        List<CMLProperty> props = prop.getPropertyDescendants();
//        if (!props.isEmpty()) {
//            for (int i = 0; i < props.size(); i++) {
//                attrVals.add(getPropertyAttributes(props.get(i)));
//            }
//        }
//        return attrVals;
//    }
//
//    protected Map<String, String> getPropertyAttributes(CMLPropertyList prop, String name) {
//        HashMap<String, String> attrVals = new HashMap<String, String>();
//        try {
//            List<CMLProperty> props = prop.getPropertyDescendantsByName(name);
//            if (!props.isEmpty()) {
//                for (int i = 0; i < props.size(); i++) {
//                    for (int j = 0; j < props.get(i).getAttributeCount(); j++) {
//                        try {
//                            attrVals.put(props.get(i).getAttribute(j).getQualifiedName(), props.get(i).getAttribute(j).getValue());
//                        } catch (NullPointerException npe) {
//                        }
//                    }
//                }
//            }
//        } catch (NullPointerException npe) {
//        }
//        return attrVals;
//    }
//    
//    protected Map<String, String> getPropertyValueAttributes(CMLPropertyList prop, String name) {
//        xxx
//        HashMap<String, String> attrVals = new HashMap<String, String>();
//        try {
//            List<CMLProperty> props = prop.getPropertyDescendantsByName(name);
//            if (!props.isEmpty()) {
//                for (int i = 0; i < props.size(); i++) {
//                    for (int j = 0; j < props.get(i).getAttributeCount(); j++) {
//                        try {
//                            attrVals.put(props.get(i).getAttribute(j).getQualifiedName(), props.get(i).getAttribute(j).getValue());
//                        } catch (NullPointerException npe) {
//                        }
//                    }
//                }
//            }
//        } catch (NullPointerException npe) {
//        }
//        return attrVals;
//    }
//
//    protected Map<String, String> getPropertyAttributes(CMLProperty prop) {
//        HashMap<String, String> attrVals = new HashMap<String, String>();
//        for (int j = 0; j < prop.getAttributeCount(); j++) {
//            try {
//                attrVals.put(prop.getAttribute(j).getQualifiedName(), prop.getAttribute(j).getValue());
//            } catch (NullPointerException npe) {
//            }
//        }
//        return attrVals;
//    }
    protected List<Map<String, String>> getPropertyValueAttributes(CMLPropertyList prop, String name) {
        return getPropertyValueAttributes(prop).get(name);
    }

    protected List<String> getPropertyValueAttributes(CMLPropertyList prop, String name, String attribute) {
        List<String> attrValues = new ArrayList<String>();
        List<Map<String, String>> values = getPropertyValueAttributes(prop).get(name);
        for (Map<String, String> m : values) {
            if (m.containsKey(attribute)) {
                attrValues.add(m.get(attribute));
            }
        }
        return attrValues;
    }

    protected Map<String, List<Map<String, String>>> getPropertyValueAttributes(CMLPropertyList prop) {
        Map<String, List<Map<String, String>>> attrList = new HashMap<String, List<Map<String, String>>>();
        for (CMLProperty p : prop.getPropertyDescendants()) {
            attrList.put(p.getDictRef(), getPropertyValueAttributes(p));
        }
        return attrList;
    }

    protected List<Map<String, String>> getPropertyValueAttributes(CMLProperty prop) {
        List<Map<String, String>> attrList = new ArrayList<Map<String, String>>();
        List<CMLElement> elements = prop.getDescendants();
        for (CMLElement e : elements) {
            HashMap<String, String> attr = new HashMap<String, String>();
            for (int j = 0; j < e.getAttributeCount(); j++) {
                // ignore dictref
                if (e.getAttribute(j).getQualifiedName().equalsIgnoreCase("dictRef")) {
                    continue;
                }
                attr.put(e.getAttribute(j).getQualifiedName(), e.getAttribute(j).getValue());
            }
            if (!attr.isEmpty()) {
                attrList.add(attr);
            }
        }
        return attrList;
    }

    protected HashMap<String, List<String>> getCMLArrayValues(CMLTable table) {
        HashMap<String, List<String>> val = new HashMap<String, List<String>>();
        for (CMLElement element : table.getDescendants()) {
            if (element.getQualifiedName().equals("array")) {
                CMLArray array = (CMLArray) element;
                val.put(array.getDictRef(), getCMLArrayValues(element));
            }
        }
        return val;
    }

    protected List<String> getCMLArrayValues(CMLElement element) {
        if (element.getQualifiedName().equals("array")) {
            CMLArray array = (CMLArray) element;
            List<String> v = new ArrayList<String>();
            for (int i = 0; i < array.getArraySize(); i++) {
                v.add(array.getElementAt(i).getValue());
            }
            return v;
        }
        return null;
    }

    protected HashMap<String, String> getCMLAttributeValues(CMLTable table, String attribute) {
        HashMap<String, String> val = new HashMap<String, String>();
        for (CMLElement element : table.getDescendants()) {
            try {
                if (element.getAttribute(attribute).getQualifiedName().trim().equalsIgnoreCase(attribute.trim())) {
                    val.put(element.getAttribute("dictRef").getValue(), element.getAttribute(attribute).getValue());
                }
            } catch (NullPointerException npe) {
            }
        }
        return val;
    }
}
