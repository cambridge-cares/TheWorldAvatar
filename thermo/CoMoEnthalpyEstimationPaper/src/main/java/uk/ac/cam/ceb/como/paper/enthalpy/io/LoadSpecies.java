package uk.ac.cam.ceb.como.paper.enthalpy.io;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.HfSpeciesConverter;

public class LoadSpecies {

	private Collection<Species> refSpecies;

	/**
	 * 
	 * @author mk510 (caresssd@hermes.cam.ac.uk)
	 * @author am2145( am2145@cam.ac.uk )
	 * 
	 * @param ref Set of target species given as csv file. The file contains species name, electronic enegry at zero Kelvin, species standard enthalpy of formation, atom list, connectivity - bond type multiset. (given by Dr Angiras Menon, email: am2145@cam.ac.uk )
	 * @param spinMultiplicity the spin multiplicity value.
	 * @param srcCompoundsRef the path to Gaussian files (g09) as a reference set of species.
	 * @param mapElPairing contains species name and number of electrons.
	 * @throws Exception the exception.
	 * 
	 */
	
	public List<Species> loadSpeciesProperties(Collection<Species> ref, Map<Species, Integer> spinMultiplicity, String srcCompoundsRef, Map<String, Integer[]> mapElPairing ) throws Exception {	
		 
		 Collection<Species> invalids = new HashSet<>();
		 
		int ctr = 1;
		
	     /**
	      * Iterates over target species collection.
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
		
		List<Species> refSpecies = new ArrayList<Species>(ref);
		
		return refSpecies;
		
	}
	/**
	 * @author mk510 (caresssd@hermes.cam.ac.uk)
	 * @author am2145 ( am2145@cam.ac.uk )
	 * 
	 * @param srcCompoundsRef The path to Gaussian files
	 * @param srcRefPool The path to csv file that contains information about reference species total energy at zero Kelvin, enthalpy of formation, species names, etc.
	 * @return the collection of Species objects.
	 * @throws Exception
	 */
	public Collection<Species> loadReferenceSpeciesFiles(String srcRefPool) throws Exception{
		
		SpeciesPoolParser refParser = new SpeciesPoolParser(new File(srcRefPool));
        
        refParser.parse();
        
        Collection<Species> ref = refParser.getRefSpecies();
        
		return ref;
		 
		
	}
	
	/**
	 * @param srcRefPoolv The path to csv file that contains information about reference species total energy at zero Kelvin, enthalpy of formation, species names, etc.
	 * @param validSpecies  The set of valid species generated in pre-processing step.
	 * @return The collection of species that are selected as valid species in pre-processing step.
	 * @throws Exception The exception.
	 */
	public void loadReferenceSpeciesForInitialAnalysis(String srcRefPool, Set<Species> validSpecies) throws Exception{
		
		SpeciesPoolParser refParser = new SpeciesPoolParser(new File(srcRefPool));

		refParser.parse();
        
        refSpecies = new ArrayList<>(refParser.getRefSpecies());

        List<Species> intialAnalysisRefSpecies = new ArrayList<Species>();
        
        System.out.println("Started reference species: ");
        
        for(Species r: refSpecies) {
        	
        	System.out.println("species ref name: " +r.getRef() + " species ref  enthalpy: "  + r.getHf());
        	
        	for(Species vs: validSpecies) {
        	
        		if(vs.getRef().equals(r.getRef())) {
        			
        		intialAnalysisRefSpecies.add(r);
        		
        	}
        	}
        }
        
        System.out.println("Initial Analysis Ref Species: ");
        
        for(Species iars: intialAnalysisRefSpecies) {
        	
        	System.out.println(iars.getRef());
        	
        }
        
//		return intialAnalysisRefSpecies;
        
	}
	
	public Collection<Species> getRefSpecies() {
		return refSpecies;
	}
	
	public void setRefSpecies(Collection<Species> refSpecies) {
		this.refSpecies = refSpecies;
	}
}