/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;

/**
 * A representation for an isodesmic reaction. The <code>IsodesmicSolver</code>
 * class solves the problem and returns the result as an object of
 * <code>ISDReaction</code>. <code>ISDReaction</code> consists of reactants and
 * products which can be used for calculating the enthalpy of formation. The
 * calculation can be done using <code>calculateHf()</code> method.
 *
 * @author pb556
 */
public class Reaction {

	private Map<Species, Double> reactantToStoichiometry = new HashMap<Species, Double>();
	private Map<Species, Double> productToStoichiometry = new HashMap<Species, Double>();
	private final Species species;

	/**
	 * Create an isodesmic reaction using the specified species as a target (whose
	 * enthalpy of formation is unknown).
	 *
	 * @param species
	 */
	public Reaction(Species species) {
		this.species = species;
	}

	public Species getSpecies() {
		return species;
	}

	private void addPutToMap(Map<Species, Double> map, Species species, double count) {
		if (count > 0) {
			if (map.containsKey(species)) {
				Double oldCount = map.get(species);
				map.put(species, count + oldCount);
			} else {
				map.put(species, count);
			}
		}
	}

	/**
	 * Get a map of reactants with their stoichiometry numbers.
	 *
	 * @return a map of reactants (keys) with their stoichiometry numbers (values)
	 */
	public Map<Species, Double> getReactants() {
		return reactantToStoichiometry;
	}

	/**
	 * Add a reactant with its stoichiometry numbers. The reactant is added if and
	 * only if the count is a positive integer.
	 *
	 * @param species a reactant
	 * @param count   a stoichiometry number
	 */
	public void addReactant(Species species, double count) {
		addPutToMap(reactantToStoichiometry, species, count);
	}

	/**
	 * Get a map of products with their stoichiometry numbers.
	 *
	 * @return a map of products (keys) with their stoichiometry numbers (values)
	 */
	public Map<Species, Double> getProducts() {
		return productToStoichiometry;
	}

	/**
	 * Add a product with its stoichiometry numbers. The product is added if and
	 * only if the count is a positive integer.
	 *
	 * @param species a product
	 * @param count a stoichiometry number
	 */
	public void addProduct(Species species, double count) {

		addPutToMap(productToStoichiometry, species, count);
	}

	/**
	 * Calculate the enthalpy of formation of a target species.
	 *
	 * @return the calculated enthalpy of formation of a target species.
	 */
	public double calculateHf() {

		Map<Species, Double> lhsNoTargetMap = new HashMap<Species, Double>();

		Map<Species, Double> rhsMap = new HashMap<Species, Double>();
		
//		System.out.println("reactantToStoichiometry map: " );
		
		for (Map.Entry<Species, Double> entry : reactantToStoichiometry.entrySet()) {

			Species species = entry.getKey();

			Double value = entry.getValue();

//			System.out.println("[reactantToStoichiometry.getRef(): " + species.getRef() + "], [reactantToStoichiometry.getHf(): " + species.getHf() + "] , [" + "value: " + value + "], [reactantToStoichiometry.getTotalEnergy(): " + species.getTotalEnergy() + "] ");

		}
		
//		System.out.println("productToStoichiometry map: ");
		
		for (Map.Entry<Species, Double> entry : productToStoichiometry.entrySet()) {

			Species species = entry.getKey();

			Double value = entry.getValue();

//			System.out.println("[productToStoichiometry.getRef(): " + species.getRef() + "], [productToStoichiometry.getHf(): " + species.getHf() + "] , [" + "value: " + value + "], [productToStoichiometry.getTotalEnergy(): " + species.getTotalEnergy() + "] ");

		}
		
//		System.out.println("calculateHf(): reactantToStoichiometry.containsKey(species): " + reactantToStoichiometry.containsKey(species));
		
//		System.out.println("calculateHf():  productToStoichiometry.containsKey(species): " + productToStoichiometry.containsKey(species));
		
		if (reactantToStoichiometry.containsKey(species)) {
			
			lhsNoTargetMap = new HashMap<Species, Double>(reactantToStoichiometry);
			
			rhsMap = new HashMap<Species, Double>(productToStoichiometry);			
			
		} else if (productToStoichiometry.containsKey(species)) {

			lhsNoTargetMap = new HashMap<Species, Double>(productToStoichiometry);			
			
			rhsMap = new HashMap<Species, Double>(reactantToStoichiometry);

		} else {

			throw new RuntimeException("Unable to evaluate the enthalpy of formation because the species is not part of the isodesmic reaction.");
		}

//		System.out.println("printing rhsMap: ");

		for (Map.Entry<Species, Double> entry : rhsMap.entrySet()) {

			Species species = entry.getKey();

			Double value = entry.getValue();

//			System.out.println("[species.getRef(): " + species.getRef() + "], [species.getHf(): " + species.getHf() + "] , [" + "value: " + value + "], [species.getTotalEnergy(): " + species.getTotalEnergy() + "] ");

		}

		double v = lhsNoTargetMap.get(species);

//		System.out.println( "in calculateHf() - 2. species.getRef(): " + species.getRef() + " species.getHf(): " + species.getHf());

		lhsNoTargetMap.remove(species);

//		System.out.println("Printing lhsNoTargetMap, after removing species: ");

		for (Map.Entry<Species, Double> entry : lhsNoTargetMap.entrySet()) {

			Species species = entry.getKey();

			Double value = entry.getValue();

//			System.out.println("[species.getRef(): " + species.getRef() + "], [species.getHf(): " + species.getHf() + "], [" + "value: " + value + "] , [ species.getTotalEnergy(): " + species.getTotalEnergy() + "]");

		}

		// sum rhs for zpe, hf
		// sum lhs[!target] for zpe, hf with no target
		// zpe(sum lhs[!target] - sum rhs) + zpe(target) = hf(sum lhs[!target] - sum
		// rhs) + hf(target)
		// hf(target) = zpe(sum lhs[!target] - sum rhs) + zpe(target) - hf(sum
		// lhs[!target] - sum rhs)

		// faulty implementation!!!
		double lhsSumZPENoTarget = sumTerms(lhsNoTargetMap, new TotalEnergySelector());

		double rhsSumZPE = sumTerms(rhsMap, new TotalEnergySelector());

		double enthalpyReaction = rhsSumZPE - (lhsSumZPENoTarget + species.getTotalEnergy() * v);

//		System.out.println("[v: " + v + "], [lhsSumZPENoTarget: " + lhsSumZPENoTarget + "], [species.getTotalEnergy(): " + species.getTotalEnergy() + "], [rhsSumZPE: " + rhsSumZPE + "], [enthalpyReaction = rhsSumZPE - (lhsSumZPENoTarget + species.getTotalEnergy() * v): " + enthalpyReaction + " ] ");

		double rhsSumHf = sumTerms(rhsMap, new EnthalpyEnergySelector());

		double lhsSumHfNoTarget = sumTerms(lhsNoTargetMap, new EnthalpyEnergySelector());

//		System.out.println("[rhsSumHf: " + rhsSumHf + "], [enthalpyReaction: " + enthalpyReaction + "], [lhsSumHfNoTarget: " + lhsSumHfNoTarget + "], [ calculateHf: = (rhsSumHf - enthalpyReaction - lhsSumHfNoTarget) / v: " + (rhsSumHf - enthalpyReaction - lhsSumHfNoTarget) / v + " ] ");

		return (rhsSumHf - enthalpyReaction - lhsSumHfNoTarget) / v;

//        double lhsSumHfNoTarget = sumTerms(lhsNoTargetMap, new EnthalpyEnergySelector());
//        double rhsSumHf = sumTerms(rhsMap, new EnthalpyEnergySelector());
//        
//        // stoichiometry is missing
//        double hf = lhsSumZPENoTarget - rhsSumZPE + species.getTotalEnergy() - lhsSumHfNoTarget + rhsSumHf;
//        return hf;

//        double lhsSumZPENoTarget = sumTerms(lhsNoTargetMap, new ZPEnergySelector());
//        double rhsSumZPE = sumTerms(rhsMap, new ZPEnergySelector());
//        double lhsSumHfNoTarget = sumTerms(lhsNoTargetMap, new EnthalpyEnergySelector());
//        double rhsSumHf = sumTerms(rhsMap, new EnthalpyEnergySelector());
//        double hf = lhsSumZPENoTarget - rhsSumZPE + species.getTotalEnergy() - lhsSumHfNoTarget + rhsSumHf;
//        return hf;
		
	}

	private double sumTerms(Map<Species, Double> terms, EnergySelector selector) {

		double sum = 0d; // 0d

//		System.out.println("sumTerms(Map<Species, Double> terms, EnergySelector selector) method (Reaction class) : ");

		int iteration = 1;

		for (Map.Entry<Species, Double> entry : terms.entrySet()) {

			Species sp = entry.getKey();

			Double count = entry.getValue();

			sum += (double) count * selector.getEnergy(sp); // count * sp.getHf()

//			System.out.println("[it. num:  " + iteration++ + " ], [sp.getRef(): " + sp.getRef() + "], [sp.getHf():  "+ sp.getHf() + "], [sp.getTotalEnergy():  " + sp.getTotalEnergy() + "], [count: " + count  + "], [selector.getEnergy(sp): " + selector.getEnergy(sp) + "], [ sum : =sum + (double) count * selector.getEnergy(sp):  " + sum + "]");

		}
		return sum;
	}

	private String concaternateTerms(Map<Species, Double> terms, String beginning, String delimiter, String ending) {
		StringBuilder objSb = new StringBuilder(beginning);

		for (Iterator<Map.Entry<Species, Double>> it = terms.entrySet().iterator(); it.hasNext();) {
			Map.Entry<Species, Double> entry = it.next();
			Species sp = entry.getKey();
			Double count = entry.getValue();
			if (count != 1) {
				objSb.append(count).append(" ");
			}
			objSb.append(sp);
			if (it.hasNext()) {
				objSb.append(delimiter);
			} else {
				objSb.append(ending);
			}
		}
		return objSb.toString();
	}

//    @Override
//    public String toString() {
//        return concaternateTerms(reactantToStoichiometry, "", " + ", "") + " = " + concaternateTerms(productToStoichiometry, "", " + ", "");
//    }

	private interface EnergySelector {

		double getEnergy(Species species);
	}

	private class TotalEnergySelector implements EnergySelector {

		@Override
		public double getEnergy(Species species) {

			return species.getTotalEnergy();
		}
	}

	private class EnthalpyEnergySelector implements EnergySelector {

		@Override
		public double getEnergy(Species species) {

//			System.out.println("{INSIDE getEnergy(Species species) of EnthalpyEnergySelector:  " + " species.getRef(): " + species.getRef() + " species.getHf(): " + species.getHf() + " species.getTotalEnergy(): " + species.getTotalEnergy() + " }");

			return species.getHf();
		}
	}

	public boolean isEmpty() {
		return productToStoichiometry.isEmpty() || reactantToStoichiometry.isEmpty();
	}

	public boolean equals(Reaction r) {
		if (!equals(productToStoichiometry, r.getProducts()) || !equals(reactantToStoichiometry, r.getReactants())) {
			return false;
		}
		return true;
	}

	private boolean equals(Map<Species, Double> s1, Map<Species, Double> s2) {
		for (Species s : s1.keySet()) {
			boolean identified = false;
			for (Species sR : s2.keySet()) {
				if (s.equals(sR, false) && Math.abs(s1.get(s) - s2.get(sR)) < 0.0001) {
					identified = true;
					break;
				}
			}
			if (!identified) {
				return false;
			}
		}
		return true;
	}

	@Override
	public String toString() {
		if (getReactants().size() >= 1 && getProducts().size() >= 1) {
			return getSpecies().getRef() + ": " + toString(getReactants()) + " <-> " + toString(getProducts());
		}
		return "";
	}

	protected String toString(Map<Species, Double> species) {
		
		String rStr = "";
		
		boolean first = true;
		
		for (Species s : species.keySet()) {
			if (!first) {
				rStr += " + ";
			} else {
				first = false;
			}
			rStr += species.get(s) + "x\"" + s.getRef() + "\"";
		}
		
		return rStr;
	}

	public boolean contains(Species s) {
		for (Species r : reactantToStoichiometry.keySet()) {
			if (s.equals(r, true)) {
				return true;
			}
		}
		for (Species r : productToStoichiometry.keySet()) {
			if (s.equals(r, true)) {
				return true;
			}
		}
		return false;
	}
}