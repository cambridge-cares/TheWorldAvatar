package uk.ac.cam.ceb.como.paper.enthalpy.io;

import java.io.BufferedWriter;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolParser;
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
	 * @throws Exception the exception.
	 * 
	 */
	
	public List<Species> loadSpeciesProperties(Collection<Species> ref, Map<Species, Integer> spinMultiplicity, String srcCompoundsRef, Map<String, Integer[]> mapElPairing, BufferedWriter printedResultsFile ) throws Exception {
		
		Collection<Species> invalids = new HashSet<>();
		
		int ctr = 1;
		
	     /**
	      * 
	      * Iterates over target species collection.
	      * 
	      */
		for (Species s : ref) {
			
        System.out.println("REF: Processing " + ctr + " / " + ref.size());
        
        printedResultsFile.write("REF: Processing " + ctr + " / " + ref.size());
        
        printedResultsFile.write("\n");
        
        ctr++;
        
        File f = new File(srcCompoundsRef + s.getRef().replace(".g09", "") + ".g09");
            
        if (f.exists()) {
                
            	System.out.print(f.getName() + ": ");
            	
            	printedResultsFile.write(f.getName() + ": ");
            	printedResultsFile.write("\n");
                
                Integer[] e = HfSpeciesConverter.getNumberOfElectrons(HfSpeciesConverter.parse(f));
                
                if (e != null) {
                    
                	spinMultiplicity.put(s, e[1] + 1);
                
                    mapElPairing.put(s.getRef(), e);
                    
                } else {
                    
                	System.out.println("REF: e- pairing could not be determined for " + s.getRef());
                    
                	printedResultsFile.write("REF: e- pairing could not be determined for " + s.getRef());
                	
                	printedResultsFile.write("\n");
                	
                    invalids.add(s);
                }
                
            } else {
            	
                System.out.println("REF: No file found for " + s.getRef());
                
                printedResultsFile.write("REF: No file found for " + s.getRef());
                
                printedResultsFile.write("\n");
                
                invalids.add(s);
            }
        }
		
//		System.out.println("spinMultiplicity map in LoadSpecies calss: method public List<Species> loadSpeciesProperties:");
		
		for(Map.Entry<Species, Integer> sspin: spinMultiplicity.entrySet()) {
		
//		System.out.println("species name: " + sspin.getKey().getRef() + " species Hf: "+ sspin.getKey().getHf() + " spinMultiplicity value: " + sspin.getValue());
		
		}
		
		ref.removeAll(invalids);
		
		List<Species> refSpecies = new ArrayList<Species>(ref);
		
//		System.out.println("refSpecies at the end of method loadSpeciesProperties");
		
		for(Species rsp: refSpecies) {
			
//		System.out.println("species name: " + rsp.getRef() + " Hf: " + rsp.getHf());
		
		}
		
		return refSpecies;
		
	}
	
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk)
	 * @author am2145 ( am2145@cam.ac.uk )
	 * 
	 * @param srcCompoundsRef_ti The path to Gaussian files
	 * @param srcRefPool The path to csv file that contains information about reference species total energy at zero Kelvin, enthalpy of formation, species names, etc.
	 * @return the collection of Species objects.
	 * @throws Exception
	 * 
	 */
	
	public Collection<Species> loadReferenceSpeciesFiles(String srcRefPool) throws Exception{
		
		SpeciesPoolParser refParser = new SpeciesPoolParser(new File(srcRefPool));
        
        refParser.parse();
        
        Collection<Species> ref = refParser.getRefSpecies();
        
//        System.out.println("ref.isEmpty() (LoadSpecies class)" +ref.isEmpty());
        
		return ref;
	}
	
	/**
	 * 
	 * @param srcRefPoolv The path to csv file that contains information about reference species total energy at zero Kelvin, enthalpy of formation, species names, etc.
	 * @param validSpecies  The set of valid species generated in pre-processing step.
	 * @return The collection of species that are selected as valid species in pre-processing step.
	 * @throws Exception The exception.
	 * 
	 */
	public List<Species> loadReferenceSpeciesForInitialAnalysis(String srcRefPool, LinkedHashSet<Species> validSpecies) throws Exception{
		
		SpeciesPoolParser refParser = new SpeciesPoolParser(new File(srcRefPool));

		refParser.parse();
        
		Collection<Species> refSpecies = new ArrayList<>(refParser.getRefSpecies());

        List<Species> intialAnalysisRefSpecies = new ArrayList<Species>();
        
//        System.out.println("Started reference species used in pre-processing step: ");
        
        for(Species r: refSpecies) {
        	
//        	System.out.println("species ref name: " +r.getRef() + " species ref  enthalpy: "  + r.getHf());
        	
        	for(Species vs: validSpecies) {
        	
        		if(vs.getRef().equals(r.getRef())) {
        			
        		intialAnalysisRefSpecies.add(r);        		
        	}
        	
        	}
        }
        
//        System.out.println("Initial Analysis Ref Species: ");
        
        for(Species iars: intialAnalysisRefSpecies) {
        	
//      System.out.println("Species name: " + iars.getRef() + " Enthalpy of formation: " + iars.getHf());
        	
        }
        
		return intialAnalysisRefSpecies;
        
	}
	
	/**
	 * @param refSpecies The collection of reference set of species that is accepted set of species.
	 * @param soiSpecies The collection of target species that is one species from rejected list of spcies having maximum error bar. 
	 * @param all The collection of reference and target set of species
	 * @param spinMultiplicity The spin multiplicity 
	 * @param srcCompoundsRef The source folder that contains Gaussian files for selected set of reference species taken from valid list of species.
	 * @param mapElPairing 
	 * @throws Exception
	 */
	public void loadSpeciesPropertiesInitialAnalysis(List<Species> refSpecies, List<Species> soiSpecies, Set<Species> all, Map<Species, Integer> spinMultiplicity, String srcCompoundsRef, Map<String, Integer[]> mapElPairing, Collection<Species> invalids,BufferedWriter printedResultsFile ) throws Exception {
        
        int ctr = 1;
        
        all.addAll(soiSpecies);
        all.addAll(refSpecies);

        for (Species s : refSpecies) {
        	
            System.out.println("REF: Processing " + ctr + " / " + refSpecies.size());
            
            printedResultsFile.write("REF: Processing " + ctr + " / " + refSpecies.size());
            
            printedResultsFile.write("\n");
            
            ctr++;
            
            File f = new File(srcCompoundsRef + s.getRef().replace(".g09", "") + ".g09");
            
            if (f.exists()) {
            
            	System.out.print(f.getName() + ": ");
            	
            	printedResultsFile.write(f.getName() + ": ");
                
                printedResultsFile.write("\n");
                
                
            	try {
            		
                    Integer[] e = HfSpeciesConverter.getNumberOfElectrons(HfSpeciesConverter.parse(f));
                    
                    if (e != null) {
                    	
                        spinMultiplicity.put(s, e[1] + 1);
                        mapElPairing.put(s.getRef(), e);
                        
                    } else {
                        
                    	System.out.println("REF: e- pairing could not be determined for " + s.getRef());
                    	
                    	printedResultsFile.write("REF: e- pairing could not be determined for " + s.getRef());
                    	
                    	printedResultsFile.write("\n");
                    	 
                        invalids.add(s);
                    }
                    
                } catch (NullPointerException npe) {
                	
                    if (s.getRef().compareTo("Ti5O6Cl8") == 0) {
                    	
                        spinMultiplicity.put(s, 1);
                        
                    } else {
                    	
                        System.out.println(s.getRef());
                        
                        printedResultsFile.write(s.getRef());
                        
                        printedResultsFile.write("\n");
                    }
                }
            	
            } else {
            	
            System.out.println("REF: No file found for " + s.getRef());
                
            printedResultsFile.write("REF: No file found for " + s.getRef());
            
            printedResultsFile.write("\n");
            
            invalids.add(s);
            
            }
        }
        
        refSpecies.removeAll(invalids);
        all.removeAll(invalids);
        soiSpecies.removeAll(invalids);
        
	}
	
	
}