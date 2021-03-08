package gigadot.chom.chem.structure;

import org.apache.log4j.Logger;
import org.xmlcml.cml.element.CMLBond;

public enum BondType {

    UNKNOWN, SINGLE, DOUBLE, TRIPLE, AROMATIC;
    private static Logger logger = Logger.getLogger(BondType.class);

    public static BondType parseCMLBondType(double bond_type) {
        if (bond_type == 1.0) {
            return BondType.SINGLE;
        } else if (bond_type == 2.0) {
            return BondType.DOUBLE;
        } else if (bond_type == 3.0) {
            return BondType.TRIPLE;
        } else if (bond_type == 1.5) {
            return BondType.AROMATIC;
        } else {
            String msg = "Unexpected CML bond order : " + bond_type;
            logger.error(msg);
            throw new UnsupportedOperationException(msg);
        }
    }

    public static BondType parseCMLBondType(String bond_type) {
        if (bond_type.matches("[Ss1]")) {
            return BondType.SINGLE;
        } else if (bond_type.matches("[Dd2]")) {
            return BondType.DOUBLE;
        } else if (bond_type.matches("[Tt3]")) {
            return BondType.TRIPLE;
        } else if (bond_type.matches("[Aa(1.5)]")) {
            return BondType.AROMATIC;
        } else {
            String msg = "Unexpected CML bond order : " + bond_type;
            logger.error(msg);
            throw new UnsupportedOperationException(msg);
        }
    }

    public static BondType parseWMLBondType(String type) {
        if (type.equalsIgnoreCase("single")) {
            return BondType.SINGLE;
        } else if (type.equalsIgnoreCase("double")) {
            return BondType.DOUBLE;
        } else if (type.equalsIgnoreCase("triple")) {
            return BondType.TRIPLE;
        } else if (type.equalsIgnoreCase("aromatic")) {
            return BondType.AROMATIC;
        } else {
            String msg = "WML does not support bond type value of " + type + ".";
            logger.error(msg);
            throw new UnsupportedOperationException(msg);
        }
    }

    public static BondType parseWMLBondType(int type) {
        switch (type) {
            case 1:
                return BondType.SINGLE;
            case 2:
                return BondType.DOUBLE;
            case 3:
                return BondType.TRIPLE;
            case 15:
                return BondType.AROMATIC;
            default:
                String msg = "WML does not support bond type value of " + type + ".";
                logger.error(msg);
                throw new UnsupportedOperationException(msg);
        }
    }

    public static BondType parseOBBondType(int type) {
        switch (type) {
            case 1:
                return BondType.SINGLE;
            case 2:
                return BondType.DOUBLE;
            case 3:
                return BondType.TRIPLE;
            case 5 : return BondType.AROMATIC;
            default:
                String msg = "Unexpected OpenBabel bond order : " + type;
                logger.error(msg);
                throw new UnsupportedOperationException(msg);
        }
    }

    public static int convertToOBBondType(BondType bt) {
        switch (bt) {
            case SINGLE :
                return 1;
            case DOUBLE :
                return 2;
            case TRIPLE :
                return 3;
            case AROMATIC :
                return 5;
            default:
                String msg = "Does not support conversion of jchem BondType." + bt.name() + " to OpenBabel bond type";
                logger.error(msg);
                throw new UnsupportedOperationException(msg);
        }
    }

    public static String convertToCMLBondType(BondType bt) {
        switch (bt) {
            case SINGLE:
                return CMLBond.SINGLE_S;
            case DOUBLE:
                return CMLBond.DOUBLE_D;
            case TRIPLE:
                return CMLBond.TRIPLE_T;
            case AROMATIC:
                return CMLBond.AROMATIC;
            default:
                String msg = "Does not support conversion of jchem BondType." + bt.name() + " to CML bond type";
                logger.error(msg);
                throw new UnsupportedOperationException(msg);
        }
    }

}
