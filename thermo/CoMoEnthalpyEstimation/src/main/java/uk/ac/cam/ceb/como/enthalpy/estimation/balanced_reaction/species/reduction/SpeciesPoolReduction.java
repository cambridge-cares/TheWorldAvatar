/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.reduction;

import java.util.Collection;

import com.cmclinnovations.data.collections.ObjectPool;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.ObjectPoolCalculator;

/**
 *
 * @author pb556
 * @param <T>
 */
public class SpeciesPoolReduction {
    
	/*
	 * Constructors of this class are added by using ObjectPoolCalculator's calculator as an argument, 
	 * because it is used in class SystematicPoolReductionAlgorithm that extends SpeciesPoolReduction class.
	 */
	/**
	 * @author nk510
	 */
	protected ObjectPoolCalculator calculator =null;
	
	/**
	 * @author nk510
	 */
	/*
	 * Variable pool added because it is used in subclass SystematicPoolReductionAlgorithm . Check whether this would work during running project.
	 */
	protected ObjectPool<Species> pool;
	
	public SpeciesPoolReduction() {
		
	}
	
	public SpeciesPoolReduction(ObjectPoolCalculator calculator) {
		super();
		this.calculator = calculator;
	}

	/**
	 * @author nk510
	 */
	/*
	 * Methods reduce()  and getReducedObjectPool() are added because they are implemented in subclass SystematicPoolReductionAlgorithm that extends SpeciesPoolReduction class. 
	 */
	
	public void reduce() throws Exception {
		// TODO Auto-generated method stub
		return;
	}

	public ObjectPool<Species> getReducedObjectPool() {
		// TODO Auto-generated method stub
		return null;
	}
}
