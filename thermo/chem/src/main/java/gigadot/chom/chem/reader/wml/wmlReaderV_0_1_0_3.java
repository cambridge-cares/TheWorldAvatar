package gigadot.chom.chem.reader.wml;

import gigadot.chom.chem.structure.BondType;

/**
 * 
 *
 * @author Weerapong Phadungsukanan
 */
public class wmlReaderV_0_1_0_3 extends wmlReaderV_0_1_0_2 {

    @Override
    protected BondType readBondType(String type) {
        return BondType.parseWMLBondType(type);
    }


}
