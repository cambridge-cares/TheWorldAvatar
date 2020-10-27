package com.cmclinnovations.jps.model.species;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.stereotype.Controller;

import com.cmclinnovations.jps.agent.configuration.EBRAgentProperty;
import com.cmclinnovations.jps.agent.configuration.SpringConfiguration;
import com.cmclinnovations.jps.agent.ebr.EBRAgent;
import com.cmclinnovations.jps.agent.ebr.Property;
import com.cmclinnovations.jps.kg.OntoSpeciesKG;

import uk.ac.cam.cares.jps.base.slurm.job.configuration.SlurmJobProperty;
@Controller
public class SpeciesBean extends EBRAgent{	

	private String name = "";
	private String formula = "";
	private String CASRegNr = "";

	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk) NISTSpeciesId is extended with
	 *         identifier, bond, geometry, scf energy, zero point energy as species
	 *         attributes (properties)
	 * 
	 */

	private String identifier = "";
	private String bond = "";
	private String geometry = "";

	private String scfEnergy = "";
	private String zeroPointEnergy = "";

	private String standardEnthalpyOfFormation = "";

	public SpeciesBean(String CASRegNr) {
		this.CASRegNr = CASRegNr;
	}

	public SpeciesBean(String CASRegNr, String bond, String geometry, String standardEnthalpyOfFormation) {
		this.CASRegNr = CASRegNr;
		this.bond=bond;
		this.geometry=geometry;
		this.standardEnthalpyOfFormation=standardEnthalpyOfFormation;
	}
	
	
	public SpeciesBean(String name, String formula, String CASRegNr) {
		this.name = name;
		this.formula = formula;
		this.CASRegNr = CASRegNr;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	/**
	 * @param identifier the identifier
	 * @param CASRegNr the cas registry id
	 * @param bond the bond
	 * @param geometry  the geometry
	 * @param standardEnthalpyOfFormation  the standard enthalpy of formation
	 * @param scfEnergy the scf energy
	 * @param zeroPointEnergy the zero point energy
	 */
	public SpeciesBean(String identifier, String CASRegNr, String bond, String geometry,
			String standardEnthalpyOfFormation, String scfEnergy, String zeroPointEnergy) {
		this.identifier = identifier;
		this.CASRegNr = CASRegNr;
		this.bond = bond;
		this.geometry = geometry;
		this.scfEnergy = scfEnergy;
		this.zeroPointEnergy = zeroPointEnergy;
		this.standardEnthalpyOfFormation = standardEnthalpyOfFormation;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 	
	/**
	 * @param identifier the identifier
	 * @param bond the bond
	 * @param geometry the geometry
	 * @param standardEnthalpyOfFormation the standard enthalpy of formation
	 * @param scfEnergy the scf energy
	 * @param zeroPointEnergy the zero point energy
	 */
	public SpeciesBean(String identifier, String bond, String geometry, String standardEnthalpyOfFormation,
			String scfEnergy, String zeroPointEnergy) {

		this.identifier = identifier;
		this.bond = bond;
		this.geometry = geometry;
		this.scfEnergy = scfEnergy;
		this.zeroPointEnergy = zeroPointEnergy;
		this.standardEnthalpyOfFormation = standardEnthalpyOfFormation;
	}

	public String getName() {
		return name;
	}

	public String getFormula() {
		return formula;
	}

	public String getCASRegNr() {
		return CASRegNr;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @return the identifier
	 */

	public String getIdentifier() {
		return identifier;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @return the bond
	 */
	public String getBond() {
		return bond;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @return the geometry
	 */
	public String getGeometry() {
		return geometry;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @return the scf energy
	 */
	public String getScfEnergy() {
		return scfEnergy;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @param scfEnergy
	 */
	public void setScfEnergy(String scfEnergy) {
		this.scfEnergy = scfEnergy;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @return zero point energy
	 */
	public String getZeroPointEnergy() {
		return zeroPointEnergy;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @param zeroPointEnergy
	 */
	public void setZeroPointEnergy(String zeroPointEnergy) {
		this.zeroPointEnergy = zeroPointEnergy;
	}

	public String getStandardEnthalpyOfFormation() {
		
		if(standardEnthalpyOfFormation.contains("±")) {
			
			String[] tokens = standardEnthalpyOfFormation.split("±");
			
			if(tokens.length > 1) {
				
				standardEnthalpyOfFormation = tokens[0].trim();
			}
		}
		
		return standardEnthalpyOfFormation;
	}

	public void setStandardEnthalpyOfFormation(String standardEnthalpyOfFormation) {
		this.standardEnthalpyOfFormation = standardEnthalpyOfFormation;
	}
	

	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @author msff2 (msff2@cam.ac.uk)
	 * 
	 * @param species the List<Map> of species IRIs. Map contains json key as the
	 *                map key value and speices IRI as a map value.
	 * @param nistSpeciesIdList The list of SpeciesBean objects 
	 * @param oskg the onto species knowledge graph object
	 * @return the list of SpeciesBean objects.
	 * @throws Exception
	 */
	public static LinkedList<SpeciesBean> getSpeciesIRIList(List<Map<String, Object>> species,
		LinkedList<SpeciesBean> nistSpeciesIdList, OntoSpeciesKG oskg, boolean isTargetSpecies) throws Exception {
		for (Map<String, Object> speciesMap : species) {
			LinkedList<String> speciesIRIList = new LinkedList<String>();
			for (Map.Entry<String, Object> entry : speciesMap.entrySet()) {
				speciesIRIList.add(entry.getValue().toString());
			}
			String queryString = oskg.getSpeciesQueryFromJsonInput(speciesIRIList.getFirst(), speciesIRIList.getLast());
			System.out.println("");
			System.out.println("queryString: " + queryString);
			// initialising classes to read properties from the ebr-agent.properites file
	        if (applicationContextSlurmJobAPI == null) {
	        	applicationContextSlurmJobAPI = new AnnotationConfigApplicationContext(SpringConfiguration.class);
			}
	        if (applicationContextEBRAgent == null) {
	        	applicationContextEBRAgent = new AnnotationConfigApplicationContext(com.cmclinnovations.jps.agent.configuration.SpringConfiguration.class);
			}
			if (slurmJobProperty == null) {
				slurmJobProperty = applicationContextSlurmJobAPI.getBean(SlurmJobProperty.class);
			}
			if (ebrAgentProperty == null) {
				ebrAgentProperty = applicationContextEBRAgent.getBean(EBRAgentProperty.class);
			}
			nistSpeciesIdList.addAll(oskg.querySpecies(ebrAgentProperty.getEbrAgentEndPoints(), queryString, isTargetSpecies));
		}
		return nistSpeciesIdList;
	}
	
	

}
