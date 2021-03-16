package uk.ac.cam.ceb.como.chem.converter;

import uk.ac.cam.ceb.como.chem.structure.Compound;

/**
 *
 * @author pb556
 */

public interface SpeciesNameBuilder {
    /**
     * Build a species name or identifier of a given JChemDocument object.
     * @param jDoc JChemDocument object.
     * @return A String of species name or identifier.
     */
    String build(Compound jDoc);
}