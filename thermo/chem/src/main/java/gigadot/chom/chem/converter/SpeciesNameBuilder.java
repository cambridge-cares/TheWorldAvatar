package gigadot.chom.chem.converter;

import gigadot.chom.chem.structure.Compound;

/**
 *
 * @author wp214
 */
public interface SpeciesNameBuilder {
    /**
     * Build a species name or identifier of a given JChemDocument object.
     * @param jDoc JChemDocument object.
     * @return A String of species name or identifier.
     */
    String build(Compound jDoc);
}
