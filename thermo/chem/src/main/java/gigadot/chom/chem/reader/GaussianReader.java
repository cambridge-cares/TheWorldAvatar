package gigadot.chom.chem.reader;

import gigadot.chom.chem.structure.Compound;
import gigadot.chom.compchem.CompChem;
import gigadot.chom.compchem.parser.GaussianParser;
import java.io.IOException;

/**
 * GaussianReader was there in the older release but it deprecated so this is a quick wrapper
 * @author wp214
 */
public class GaussianReader extends ChemFileReader{

    @Override
    public void read(String file, Compound compound) throws IOException {
        GaussianParser parser = new GaussianParser();
        CompChem compchem = parser.parse(file);
        CompChemReader reader = new CompChemReader();
        reader.read(compchem, compound);
    }

}
