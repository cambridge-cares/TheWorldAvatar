/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.periodictable;

/**
 *
 * @author pb556
 */
public class PeriodicTableElements {

    public static Element getHydrogen() {

        Element e = new Element("H", "Hydrogen");
        e.setAtomicCharacteristics(1, 1);
        e.setPosition(1, 1);
        e.setBlock(Block.sBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(1)"));
        e.setElementCategory(ElementCategory.DiatomicNonMetals);
        return e;
    }

    public static Element getHelium() {

        Element e = new Element("He", "Helium");        
        e.setAtomicCharacteristics(2, 4);        
        e.setPosition(1, 18);
        e.setBlock(Block.sBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2)"));
        e.setElementCategory(ElementCategory.NobleGases);
        return e;
    }

    public static Element getLithium() {

        Element e = new Element("Li", "Lithium");        
        e.setAtomicCharacteristics(3, 7);        
        e.setPosition(2, 1);
        e.setBlock(Block.sBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(1)"));
        e.setElementCategory(ElementCategory.AlkaliMetals);
        return e;
    }

    public static Element getBeryllium() {

        Element e = new Element("Be", "Beryllium");        
        e.setAtomicCharacteristics(4, 9);        
        e.setPosition(2, 2);
        e.setBlock(Block.sBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2)"));
        e.setElementCategory(ElementCategory.AlkalineEarthMetals);
        return e;
    }

    public static Element getBoron() {

        Element e = new Element("B", "Boron");        
        e.setAtomicCharacteristics(5, 11);        
        e.setPosition(2, 13);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 2p^(1)"));
        e.setElementCategory(ElementCategory.Metalloids);
        return e;
    }

    public static Element getCarbon() {

        Element e = new Element("C", "Carbon");        
        e.setAtomicCharacteristics(6, 12);        
        e.setPosition(2, 14);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 2p^(2)"));
        e.setElementCategory(ElementCategory.PolyatomicNonMetals);
        return e;
    }

    public static Element getNitrogen() {

        Element e = new Element("N", "Nitrogen");        
        e.setAtomicCharacteristics(7, 14);        
        e.setPosition(2, 15);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 2p^(3)"));
        e.setElementCategory(ElementCategory.DiatomicNonMetals);
        return e;
    }

    public static Element getOxygen() {

        Element e = new Element("O", "Oxygen");        
        e.setAtomicCharacteristics(8, 16);        
        e.setPosition(2, 16);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 2p^(4)"));
        e.setElementCategory(ElementCategory.DiatomicNonMetals);
        return e;
    }

    public static Element getFluorine() {

        Element e = new Element("F", "Fluorine");        
        e.setAtomicCharacteristics(9, 19);        
        e.setPosition(2, 17);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 2p^(5)"));
        e.setElementCategory(ElementCategory.DiatomicNonMetals);
        return e;
    }

    public static Element getNeon() {

        Element e = new Element("Ne", "Neon");        
        e.setAtomicCharacteristics(10, 20);        
        e.setPosition(2, 18);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 2p^(6)"));
        e.setElementCategory(ElementCategory.NobleGases);
        return e;
    }

    public static Element getSodium() {

        Element e = new Element("Na", "Sodium");        
        e.setAtomicCharacteristics(11, 23);        
        e.setPosition(3, 1);
        e.setBlock(Block.sBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 3s^(1)"));
        e.setElementCategory(ElementCategory.AlkaliMetals);
        return e;
    }

    public static Element getMagnesium() {

        Element e = new Element("Mg", "Magnesium");       
        e.setAtomicCharacteristics(12, 24);        
        e.setPosition(3, 2);
        e.setBlock(Block.sBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 3s^(2)"));
        e.setElementCategory(ElementCategory.AlkalineEarthMetals);
        return e;
    }

    public static Element getAluminium() {

        Element e = new Element("Al", "Aluminium");        
        e.setAtomicCharacteristics(13, 27);   
        e.setPosition(3, 13);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 3s^(2) 2p^(6) 3p^(1)"));
        e.setElementCategory(ElementCategory.PoorMetals);
        return e;
    }

    public static Element getSilicon() {

        Element e = new Element("Si", "Silicon");      
        e.setAtomicCharacteristics(14, 28);   
        e.setPosition(3, 14);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 3s^(2) 2p^(6) 3p^(2)"));
        e.setElementCategory(ElementCategory.Metalloids);
        return e;
    }

    public static Element getPhosphorus() {

        Element e = new Element("P", "Phosphorus");      
        e.setAtomicCharacteristics(15, 31);   
        e.setPosition(3, 15);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 3s^(2) 2p^(6) 3p^(3)"));
        e.setElementCategory(ElementCategory.PolyatomicNonMetals);
        return e;
    }

    public static Element getSulphur() {

        Element e = new Element("S", "Sulphur");       
        e.setAtomicCharacteristics(16, 32);     
        e.setPosition(3, 16);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 3s^(2) 2p^(6) 3p^(4)"));
        e.setElementCategory(ElementCategory.PolyatomicNonMetals);
        return e;
    }

    public static Element getChlorine() {

        Element e = new Element("Cl", "Chlorine");    
        e.setAtomicCharacteristics(17, 35); 
        e.setPosition(3, 17);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 3s^(2) 2p^(6) 3p^(5)"));
        e.setElementCategory(ElementCategory.DiatomicNonMetals);
        return e;
    }

    public static Element getArgon() {

        Element e = new Element("Ar", "Argon");     
        e.setAtomicCharacteristics(18, 40);  
        e.setPosition(3, 18);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 3s^(2) 2p^(6) 3p^(6)"));
        e.setElementCategory(ElementCategory.NobleGases);
        return e;
    }

    public static Element getPotassium() {

        Element e = new Element("K", "Potassium");      
        e.setAtomicCharacteristics(19, 39);     
        e.setPosition(4, 1);
        e.setBlock(Block.sBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 3s^(2) 4s^(1) 2p^(6) 3p^(6)"));
        e.setElementCategory(ElementCategory.AlkaliMetals);
        return e;
    }

    public static Element getCalcium() {

        Element e = new Element("Ca", "Calcium");     
        e.setAtomicCharacteristics(20, 40);     
        e.setPosition(4, 2);
        e.setBlock(Block.sBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 3s^(2) 4s^(2) 2p^(6) 3p^(6)"));
        e.setElementCategory(ElementCategory.AlkalineEarthMetals);
        return e;
    }

    public static Element getScandium() {

        Element e = new Element("Sc", "Scandium");    
        e.setAtomicCharacteristics(21, 45);   
        e.setPosition(4, 3);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("3d^(1) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 2p^(6) 3p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getTitanium() {

        Element e = new Element("Ti", "Titanium");       
        e.setAtomicCharacteristics(22, 48);   
        e.setPosition(4, 4);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("3d^(2) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 2p^(6) 3p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getVanadium() {

        Element e = new Element("V", "Vanadium");  
        e.setAtomicCharacteristics(23, 51);     
        e.setPosition(4, 5);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("3d^(3) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 2p^(6) 3p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getChromium() {

        Element e = new Element("Cr", "Chromium");    
        e.setAtomicCharacteristics(24, 52);  
        e.setPosition(4, 6);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("3d^(4) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 2p^(6) 3p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getManganese() {

        Element e = new Element("Mn", "Manganese");    
        e.setAtomicCharacteristics(25, 55);  
        e.setPosition(4, 7);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("3d^(5) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 2p^(6) 3p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getIron() {

        Element e = new Element("Fe", "Iron");      
        e.setAtomicCharacteristics(26, 56);    
        e.setPosition(4, 8);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("3d^(6) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 2p^(6) 3p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getCobalt() {

        Element e = new Element("Co", "Cobalt");        
        e.setAtomicCharacteristics(27, 59);        
        e.setPosition(4, 9);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("3d^(7) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 2p^(6) 3p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getNickel() {

        Element e = new Element("Ni", "Nickel");        
        e.setAtomicCharacteristics(28, 59);        
        e.setPosition(4, 10);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("3d^(8) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 2p^(6) 3p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getCopper() {

        Element e = new Element("Cu", "Copper");       
        e.setAtomicCharacteristics(29, 64);    
        e.setPosition(4, 11);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("3d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(1) 2p^(6) 3p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getZinc() {

        Element e = new Element("Zn", "Zinc");      
        e.setAtomicCharacteristics(30, 65);  
        e.setPosition(4, 12);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("3d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 2p^(6) 3p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getGallium() {

        Element e = new Element("Ga", "Gallium");     
        e.setAtomicCharacteristics(31, 70);     
        e.setPosition(4, 13);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("3d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 2p^(6) 3p^(6) 4p^(1)"));
        e.setElementCategory(ElementCategory.PoorMetals);
        return e;
    }

    public static Element getGermanium() {

        Element e = new Element("Ge", "Germanium");     
        e.setAtomicCharacteristics(32, 73);      
        e.setPosition(4, 14);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("3d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 2p^(6) 3p^(6) 4p^(2)"));
        e.setElementCategory(ElementCategory.Metalloids);
        return e;
    }

    public static Element getArsenic() {

        Element e = new Element("As", "Arsenic");     
        e.setAtomicCharacteristics(33, 75); 
        e.setPosition(4, 15);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("3d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 2p^(6) 3p^(6) 4p^(3)"));
        e.setElementCategory(ElementCategory.Metalloids);
        return e;
    }

    public static Element getSelenium() {

        Element e = new Element("Se", "Selenium");        
        e.setAtomicCharacteristics(34, 79);    
        e.setPosition(4, 16);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("3d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 2p^(6) 3p^(6) 4p^(4)"));
        e.setElementCategory(ElementCategory.PolyatomicNonMetals);
        return e;
    }

    public static Element getBromine() {

        Element e = new Element("Br", "Bromine");      
        e.setAtomicCharacteristics(35, 80);      
        e.setPosition(4, 17);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("3d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 2p^(6) 3p^(6) 4p^(5)"));
        e.setElementCategory(ElementCategory.DiatomicNonMetals);
        return e;
    }

    public static Element getKrypton() {

        Element e = new Element("Kr", "Krypton");      
        e.setAtomicCharacteristics(36, 84);     
        e.setPosition(4, 18);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("3d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 2p^(6) 3p^(6) 4p^(6)"));
        e.setElementCategory(ElementCategory.NobleGases);
        return e;
    }

    public static Element getRubidium() {

        Element e = new Element("Rb", "Rubidium");     
        e.setAtomicCharacteristics(37, 85);    
        e.setPosition(5, 1);
        e.setBlock(Block.sBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(1) 2p^(6) 3p^(6) 4p^(6)"));
        e.setElementCategory(ElementCategory.AlkaliMetals);
        return e;
    }

    public static Element getStrontium() {

        Element e = new Element("Sr", "Strontium");       
        e.setAtomicCharacteristics(38, 88);    
        e.setPosition(5, 2);
        e.setBlock(Block.sBlock);
        e.setElectronConfiguration(new ElectronConfiguration("1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 2p^(6) 3p^(6) 4p^(6)"));
        e.setElementCategory(ElementCategory.AlkalineEarthMetals);
        return e;
    }

    public static Element getYttrium() {

        Element e = new Element("Y", "Yttrium");     
        e.setAtomicCharacteristics(39, 89);  
        e.setPosition(5, 3);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(1) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 2p^(6) 3p^(6) 4p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getZirconium() {

        Element e = new Element("Zr", "Zicronium");        
        e.setAtomicCharacteristics(40, 91);    
        e.setPosition(5, 4);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(2) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 2p^(6) 3p^(6) 4p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getNiobium() {

        Element e = new Element("Nb", "Niobium");   
        e.setAtomicCharacteristics(41, 93);   
        e.setPosition(5, 5);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(4) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(1) 2p^(6) 3p^(6) 4p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getMolybdenum() {

        Element e = new Element("Mo", "Molybdenum");      
        e.setAtomicCharacteristics(42, 96);    
        e.setPosition(5, 6);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(5) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(1) 2p^(6) 3p^(6) 4p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getTechnetium() {

        Element e = new Element("Tc", "Technetium");     
        e.setAtomicCharacteristics(43, 98);   
        e.setPosition(5, 7);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(5) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 2p^(6) 3p^(6) 4p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getRuthenium() {

        Element e = new Element("Ru", "Ruthenium");      
        e.setAtomicCharacteristics(44, 101);   
        e.setPosition(5, 8);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(7) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(1) 2p^(6) 3p^(6) 4p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getRhodium() {

        Element e = new Element("Rh", "Rhodium");       
        e.setAtomicCharacteristics(45, 103);  
        e.setPosition(5, 9);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(8) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(1) 2p^(6) 3p^(6) 4p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getPalladium() {

        Element e = new Element("Pd", "Palladium");      
        e.setAtomicCharacteristics(46, 106);     
        e.setPosition(5, 10);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 2p^(6) 3p^(6) 4p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getSilver() {

        Element e = new Element("Ag", "Silver");     
        e.setAtomicCharacteristics(47, 108);   
        e.setPosition(5, 11);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(1) 2p^(6) 3p^(6) 4p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getCadmium() {

        Element e = new Element("Cd", "Cadmium");       
        e.setAtomicCharacteristics(48, 112);   
        e.setPosition(5, 12);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 2p^(6) 3p^(6) 4p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getIndium() {

        Element e = new Element("In", "Indium");     
        e.setAtomicCharacteristics(49, 115);     
        e.setPosition(5, 13);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(1)"));
        e.setElementCategory(ElementCategory.PoorMetals);
        return e;
    }

    public static Element getTin() {

        Element e = new Element("Sn", "Tin");   
        e.setAtomicCharacteristics(50, 119);    
        e.setPosition(5, 14);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(2)"));
        e.setElementCategory(ElementCategory.PoorMetals);
        return e;
    }

    public static Element getAntimony() {

        Element e = new Element("Sb", "Antimony");     
        e.setAtomicCharacteristics(51, 121);  
        e.setPosition(5, 15);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(3)"));
        e.setElementCategory(ElementCategory.Metalloids);
        return e;
    }

    public static Element getTellurium() {

        Element e = new Element("Te", "Tellurium");      
        e.setAtomicCharacteristics(52, 127);   
        e.setPosition(5, 16);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(4)"));
        e.setElementCategory(ElementCategory.Metalloids);
        return e;
    }

    public static Element getIodine() {

        Element e = new Element("I", "Iodine");   
        e.setAtomicCharacteristics(53, 126);  
        e.setPosition(5, 17);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(5)"));
        e.setElementCategory(ElementCategory.DiatomicNonMetals);
        return e;
    }

    public static Element getXenon() {

        Element e = new Element("Xe", "Xenon");       
        e.setAtomicCharacteristics(54, 130);    
        e.setPosition(5, 18);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.NobleGases);
        return e;
    }

    public static Element getCaesium() {

        Element e = new Element("Cs", "Caesium");      
        e.setAtomicCharacteristics(55, 133);       
        e.setPosition(6, 1);
        e.setBlock(Block.sBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(1) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.AlkaliMetals);
        return e;
    }

    public static Element getBarium() {

        Element e = new Element("Ba", "Barium");     
        e.setAtomicCharacteristics(56, 137);     
        e.setPosition(6, 2);
        e.setBlock(Block.sBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.AlkalineEarthMetals);
        return e;
    }

    public static Element getLanthanum() {

        Element e = new Element("La", "Lanthanum");       
        e.setAtomicCharacteristics(57, 139);  
        e.setPosition(6, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4d^(10) 5d^(1) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.Lanthanoids);
        return e;
    }

    public static Element getCerium() {

        Element e = new Element("Ce", "Cerium");    
        e.setAtomicCharacteristics(58, 140);    
        e.setPosition(6, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(1) 4d^(10) 5d^(1) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.Lanthanoids);
        return e;
    }

    public static Element getPraseodymium() {

        Element e = new Element("Pr", "Praseodymium");    
        e.setAtomicCharacteristics(59, 141);    
        e.setPosition(6, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(3) 4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.Lanthanoids);
        return e;
    }

    public static Element getNeodymium() {

        Element e = new Element("Nd", "Neodymium");     
        e.setAtomicCharacteristics(60, 144);  
        e.setPosition(6, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(4) 4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.Lanthanoids);
        return e;
    }

    public static Element getPromethium() {

        Element e = new Element("Pm", "Promethium");      
        e.setAtomicCharacteristics(61, 145);    
        e.setPosition(6, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(5) 4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.Lanthanoids);
        return e;
    }

    public static Element getSamarium() {

        Element e = new Element("Sm", "Samarium");      
        e.setAtomicCharacteristics(62, 150);     
        e.setPosition(6, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(6) 4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.Lanthanoids);
        return e;
    }

    public static Element getEuropium() {

        Element e = new Element("Eu", "Europium");       
        e.setAtomicCharacteristics(63, 152);      
        e.setPosition(6, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(7) 4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.Lanthanoids);
        return e;
    }

    public static Element getGadolinium() {

        Element e = new Element("Gd", "Gadolinium");      
        e.setAtomicCharacteristics(64, 157);     
        e.setPosition(6, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(7) 4d^(10) 5d^(1) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.Lanthanoids);
        return e;
    }

    public static Element getTerbium() {

        Element e = new Element("Tb", "Terbium");       
        e.setAtomicCharacteristics(65, 159);      
        e.setPosition(6, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(9) 4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.Lanthanoids);
        return e;
    }

    public static Element getDysprosium() {

        Element e = new Element("Dy", "Dysprosium");    
        e.setAtomicCharacteristics(66, 163);   
        e.setPosition(6, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(10) 4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.Lanthanoids);
        return e;
    }

    public static Element getHolmium() {

        Element e = new Element("Ho", "Holmium");      
        e.setAtomicCharacteristics(67, 165);      
        e.setPosition(6, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(11) 4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.Lanthanoids);
        return e;
    }

    public static Element getErbium() {

        Element e = new Element("Er", "Erbium");   
        e.setAtomicCharacteristics(68, 167);    
        e.setPosition(6, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(12) 4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.Lanthanoids);
        return e;
    }

    public static Element getThulium() {

        Element e = new Element("Tm", "Thulium");        
        e.setAtomicCharacteristics(69, 169);        
        e.setPosition(6, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(13) 4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.Lanthanoids);
        return e;
    }

    public static Element getYtterbium() {

        Element e = new Element("Yb", "Ytterbium");        
        e.setAtomicCharacteristics(70, 173);        
        e.setPosition(6, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.Lanthanoids);
        return e;
    }

    public static Element getLutetium() {

        Element e = new Element("Lu", "Lutetium");      
        e.setAtomicCharacteristics(71, 175);  
        e.setPosition(6, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(1) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.Lanthanoids);
        return e;
    }

    public static Element getHafnium() {

        Element e = new Element("Hf", "Hafnium");     
        e.setAtomicCharacteristics(72, 178);   
        e.setPosition(6, 4);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(2) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getTantalum() {

        Element e = new Element("Ta", "Tantalum");     
        e.setAtomicCharacteristics(73, 181);  
        e.setPosition(6, 5);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(3) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getTungsten() {

        Element e = new Element("W", "Tungsten");      
        e.setAtomicCharacteristics(74, 184);    
        e.setPosition(6, 6);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(4) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getRhenium() {

        Element e = new Element("Re", "Rhenium");      
        e.setAtomicCharacteristics(75, 186);    
        e.setPosition(6, 7);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(5) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getOsmium() {

        Element e = new Element("Os", "Osmium");       
        e.setAtomicCharacteristics(76, 190);   
        e.setPosition(6, 8);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(6) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getIridium() {

        Element e = new Element("Ir", "Iridium");       
        e.setAtomicCharacteristics(77, 192);       
        e.setPosition(6, 9);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(7) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getPlatinum() {

        Element e = new Element("Pt", "Platinum");     
        e.setAtomicCharacteristics(78, 195);      
        e.setPosition(6, 10);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(9) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(1) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getGold() {

        Element e = new Element("Au", "Gold");       
        e.setAtomicCharacteristics(79, 197);   
        e.setPosition(6, 11);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(1) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getMercury() {

        Element e = new Element("Hg", "Mercury");       
        e.setAtomicCharacteristics(80, 201);   
        e.setPosition(6, 12);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getThallium() {

        Element e = new Element("Tl", "Thallium");      
        e.setAtomicCharacteristics(81, 204);  
        e.setPosition(6, 13);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6) 6p^(1)"));
        e.setElementCategory(ElementCategory.PoorMetals);
        return e;
    }

    public static Element getLead() {

        Element e = new Element("Pb", "Lead");      
        e.setAtomicCharacteristics(82, 207);    
        e.setPosition(6, 14);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6) 6p^(2)"));
        e.setElementCategory(ElementCategory.PoorMetals);
        return e;
    }

    public static Element getBismuth() {

        Element e = new Element("Bi", "Bismuth");     
        e.setAtomicCharacteristics(83, 209);    
        e.setPosition(6, 15);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6) 6p^(3)"));
        e.setElementCategory(ElementCategory.PoorMetals);
        return e;
    }

    public static Element getPolonium() {

        Element e = new Element("Po", "Polonium");    
        e.setAtomicCharacteristics(84, 209);      
        e.setPosition(6, 16);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6) 6p^(4)"));
        e.setElementCategory(ElementCategory.PoorMetals);
        return e;
    }

    public static Element getAstatine() {

        Element e = new Element("At", "Astatine");    
        e.setAtomicCharacteristics(85, 210);   
        e.setPosition(6, 17);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6) 6p^(5)"));
        e.setElementCategory(ElementCategory.Metalloids);
        return e;
    }

    public static Element getRadon() {

        Element e = new Element("Rn", "Radon");     
        e.setAtomicCharacteristics(86, 222);    
        e.setPosition(6, 18);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6) 6p^(6)"));
        e.setElementCategory(ElementCategory.NobleGases);
        return e;
    }

    public static Element getFrancium() {

        Element e = new Element("Fr", "Francium");     
        e.setAtomicCharacteristics(87, 223);
        e.setPosition(7, 1);
        e.setBlock(Block.sBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(1)"));
        e.setElementCategory(ElementCategory.AlkaliMetals);
        return e;
    }

    public static Element getRadium() {

        Element e = new Element("Ra", "Radium");   
        e.setAtomicCharacteristics(88, 226);    
        e.setPosition(7, 2);
        e.setBlock(Block.sBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.AlkalineEarthMetals);
        return e;
    }

    public static Element getActinium() {

        Element e = new Element("Ac", "Actinium");     
        e.setAtomicCharacteristics(89, 227);     
        e.setPosition(7, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(10) 6d^(1) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.Actinoids);
        return e;
    }

    public static Element getThorium() {

        Element e = new Element("Th", "Thorium");   
        e.setAtomicCharacteristics(90, 232);     
        e.setPosition(7, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 4d^(10) 5d^(10) 6d^(2) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.Actinoids);
        return e;
    }

    public static Element getProtactinium() {

        Element e = new Element("Pa", "Protactinium");   
        e.setAtomicCharacteristics(91, 231);    
        e.setPosition(7, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(2) 4d^(10) 5d^(10) 6d^(1) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.Actinoids);
        return e;
    }

    public static Element getUranium() {

        Element e = new Element("U", "Uranium");     
        e.setAtomicCharacteristics(92, 238);     
        e.setPosition(7, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(3) 4d^(10) 5d^(10) 6d^(1) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.Actinoids);
        return e;
    }

    public static Element getNeptunium() {

        Element e = new Element("Np", "Neptunium");     
        e.setAtomicCharacteristics(93, 227);   
        e.setPosition(7, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(4) 4d^(10) 5d^(10) 6d^(1) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.Actinoids);
        return e;
    }

    public static Element getPlutonium() {

        Element e = new Element("Pu", "Plutonium");      
        e.setAtomicCharacteristics(94, 244);     
        e.setPosition(7, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(6) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.Actinoids);
        return e;
    }

    public static Element getAmericium() {

        Element e = new Element("Am", "Americium");     
        e.setAtomicCharacteristics(95, 243);   
        e.setPosition(7, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(7) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.Actinoids);
        return e;
    }

    public static Element getCurium() {

        Element e = new Element("Cm", "Curium");        
        e.setAtomicCharacteristics(96, 247); 
        e.setPosition(7, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(7) 4d^(10) 5d^(10) 6d^(1) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.Actinoids);
        return e;
    }

    public static Element getBerkelium() {

        Element e = new Element("Bk", "Berkelium");       
        e.setAtomicCharacteristics(97, 247);     
        e.setPosition(7, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(9) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.Actinoids);
        return e;
    }

    public static Element getCalifornium() {

        Element e = new Element("Cf", "Californium");    
        e.setAtomicCharacteristics(98, 251);   
        e.setPosition(7, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(10) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.Actinoids);
        return e;
    }

    public static Element getEinsteinium() {

        Element e = new Element("Es", "Einsteinium");     
        e.setAtomicCharacteristics(99, 252);   
        e.setPosition(7, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(11) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.Actinoids);
        return e;
    }

    public static Element getFermium() {

        Element e = new Element("Fm", "Fermium");    
        e.setAtomicCharacteristics(100, 257);     
        e.setPosition(7, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(12) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.Actinoids);
        return e;
    }

    public static Element getMendelevium() {

        Element e = new Element("Md", "Mendelevium");     
        e.setAtomicCharacteristics(101, 258);     
        e.setPosition(7, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(13) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.Actinoids);
        return e;
    }

    public static Element getNobelium() {

        Element e = new Element("No", "Nobelium");     
        e.setAtomicCharacteristics(102, 259);     
        e.setPosition(7, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(14) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.Actinoids);
        return e;
    }

    public static Element getLawrencium() {

        Element e = new Element("Lr", "Lawrencium");       
        e.setAtomicCharacteristics(103, 262);  
        e.setPosition(7, 0);
        e.setBlock(Block.fBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(14) 4d^(10) 5d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6) 6p^(6) 7p^(1)"));
        e.setElementCategory(ElementCategory.Actinoids);
        return e;
    }

    public static Element getRutherfordium() {

        Element e = new Element("Rf", "Rutherfordium");      
        e.setAtomicCharacteristics(104, 261);    
        e.setPosition(7, 4);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(14) 4d^(10) 5d^(10) 6d^(2) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getDubnium() {

        Element e = new Element("Db", "Dubnium");       
        e.setAtomicCharacteristics(105, 262);     
        e.setPosition(7, 5);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(14) 4d^(10) 5d^(10) 6d^(3) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getSeaborgium() {

        Element e = new Element("Sg", "Seaborgium");     
        e.setAtomicCharacteristics(106, 263);      
        e.setPosition(7, 6);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(14) 4d^(10) 5d^(10) 6d^(4) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getBohrium() {

        Element e = new Element("Bh", "Bohrium");   
        e.setAtomicCharacteristics(107, 262);    
        e.setPosition(7, 7);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(14) 4d^(10) 5d^(10) 6d^(5) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getHassium() {

        Element e = new Element("Hs", "Hassium");      
        e.setAtomicCharacteristics(108, 265);    
        e.setPosition(7, 8);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(14) 4d^(10) 5d^(10) 6d^(6) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }

    public static Element getMeitnerium() {

        Element e = new Element("Mt", "Meitnerium");      
        e.setAtomicCharacteristics(109, 266);     
        e.setPosition(7, 9);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(14) 4d^(10) 5d^(10) 6d^(7) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.Unknown);
        return e;
    }

    public static Element getDarmstadtium() {

        Element e = new Element("Ds", "Darmstadtium");        
        e.setAtomicCharacteristics(110, 281);        
        e.setPosition(7, 10);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(14) 4d^(10) 5d^(10) 6d^(8) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.Unknown);
        return e;
    }

    public static Element getRoentgenium() {

        Element e = new Element("Rg", "Roentgenium");       
        e.setAtomicCharacteristics(111, 281);       
        e.setPosition(7, 11);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(14) 4d^(10) 5d^(10) 6d^(9) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.Unknown);
        return e;
    }

    public static Element getCopernicium() {

        Element e = new Element("Cn", "Copernicium");       
        e.setAtomicCharacteristics(112, 285);        
        e.setPosition(7, 12);
        e.setBlock(Block.dBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(14) 4d^(10) 5d^(10) 6d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2)"));
        e.setElementCategory(ElementCategory.TransitionMetals);
        return e;
    }
    
    public static Element getUnuntrium() {

        Element e = new Element("Uut", "Ununtrium");       
        e.setAtomicCharacteristics(113, 286);        
        e.setPosition(7, 13);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(14) 4d^(10) 5d^(10) 6d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6) 6p^(6) 7p^(1)"));
        e.setElementCategory(ElementCategory.Unknown);
        return e;
    }
    
    public static Element getFlerovium() {

        Element e = new Element("Fl", "Flerovium");       
        e.setAtomicCharacteristics(114, 289);        
        e.setPosition(7, 14);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(14) 4d^(10) 5d^(10) 6d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6) 6p^(6) 7p^(2)"));
        e.setElementCategory(ElementCategory.Unknown);
        return e;
    }
    
    public static Element getUnunpentium() {

        Element e = new Element("Uup", "Ununpentium");       
        e.setAtomicCharacteristics(115, 289);        
        e.setPosition(7, 15);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(14) 4d^(10) 5d^(10) 6d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6) 6p^(6) 7p^(3)"));
        e.setElementCategory(ElementCategory.Unknown);
        return e;
    }
    
    public static Element getLivermorium() {

        Element e = new Element("Lv", "Livermorium");       
        e.setAtomicCharacteristics(116, 293);        
        e.setPosition(7, 16);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(14) 4d^(10) 5d^(10) 6d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6) 6p^(6) 7p^(4)"));
        e.setElementCategory(ElementCategory.Unknown);
        return e;
    }
    
    public static Element getUnunseptium() {

        Element e = new Element("Uus", "Ununseptium");       
        e.setAtomicCharacteristics(117, 294);        
        e.setPosition(7, 17);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(14) 4d^(10) 5d^(10) 6d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6) 6p^(6) 7p^(5)"));
        e.setElementCategory(ElementCategory.Unknown);
        return e;
    }
    
    public static Element getUnunoctium() {

        Element e = new Element("Uuo", "Ununoctium");       
        e.setAtomicCharacteristics(118, 294);        
        e.setPosition(7, 18);
        e.setBlock(Block.pBlock);
        e.setElectronConfiguration(new ElectronConfiguration("4f^(14) 5f^(14) 4d^(10) 5d^(10) 6d^(10) 1s^(2) 2s^(2) 3s^(2) 4s^(2) 5s^(2) 6s^(2) 7s^(2) 2p^(6) 3p^(6) 4p^(6) 5p^(6) 6p^(6) 7p^(6)"));
        e.setElementCategory(ElementCategory.Unknown);
        return e;
    }
}
