package uk.ac.cam.ceb.como.paper.enthalpy.io;

import java.io.File;
import java.util.Collection;
import java.util.Map;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.HfSpeciesConverter;

public class LoadSpecies {

	/**
	 * 
	 * @author mk510 (caresssd@hermes.cam.ac.uk)
	 * @author am2145( am2145@cam.ac.uk )
	 * 
	 * @param ref Set of target species given as csv file. The file contains species name, electronic enegry at zero Kelvin, species standard enthalpy of formation, atom list, connectivity - bond type multiset. (given by Dr Angiras Menon, email: am2145@cam.ac.uk )
	 * @param spinMultiplicity the spin multiplicity value.
	 * @param srcCompoundsRef the path to Gaussian files (g09) as a reference set of species.
	 * @param mapElPairing contains species name and number of electrons.
	 * @param invalids initial collection of invalid species.
	 * @throws Exception the exception.
	 * 
	 */
	public void loadSpeciesProperties(int crt, Collection<Species> ref, Map<Species, Integer> spinMultiplicity,String srcCompoundsRef, Map<String, Integer[]> mapElPairing, Collection<Species> invalids ) throws Exception {
	
		int ctr = 1; // remove from here
		
	     /**
	      * Iterates over target species collection
	      */
		for (Species s : ref) {
			
        System.out.println("REF: Processing " + ctr + " / " + ref.size());
        
        ctr++;
        
        File f = new File(srcCompoundsRef + s.getRef().replace(".g09", "") + ".g09");
            
        if (f.exists()) {
                
            	System.out.print(f.getName() + ": ");
                
                Integer[] e = HfSpeciesConverter.getNumberOfElectrons(HfSpeciesConverter.parse(f));
                
                if (e != null) {
                    
                	spinMultiplicity.put(s, e[1] + 1);
                
                    mapElPairing.put(s.getRef(), e);
                    
                } else {
                    
                	System.out.println("REF: e- pairing could not be determined for " + s.getRef());
                    
                    invalids.add(s);
                }
                
            } else {
            	
                System.out.println("REF: No file found for " + s.getRef());
                
                invalids.add(s);
            }
        }
		
		ref.removeAll(invalids);
		
	}
	
	
}