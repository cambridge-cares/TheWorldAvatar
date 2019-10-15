package gigadot.chom.compchem.parser.gaussian.util;

import gigadot.chom.compchem.info.ComputedInfoImpl;
import gigadot.chom.compchem.info.MolecularInfoImpl;
import gigatools.lite.math.DoubleTool;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.io.IOUtils;
import org.apache.log4j.Logger;

/**
 *
 * @author Weerapong Phadungsukanan
 */
public class GaussianHelper {

    protected static Logger logger = Logger.getLogger(GaussianHelper.class);

    /**
     * Extracted the job sections from Gaussian log file.
     * @param file
     * @return
     * @throws IOException 
     */
    public static List<JobSection> extractJobSections(File file) throws IOException {
        BufferedReader r = null;
        List<JobSection> l_jobs = new ArrayList<JobSection>();
        try {
            r = new BufferedReader(new FileReader(file));
            List<String> section_lines = new ArrayList<String>();
            int order = 0;
            String l = null;
            do {
                l = r.readLine();
                boolean ntg = false; // normal terminated
                if (l != null) {
                    section_lines.add(l);
                    ntg = l.contains("Normal termination of Gaussian");
                }
                if (ntg || l == null) { // end of section
                    JobSection js = new JobSection(section_lines, order++, ntg);
                    section_lines.clear();
                    js.trim();
                    if (!js.isEmpty()) {
                        l_jobs.add(js);
                    }
                }
            } while (l != null);
        } finally {
            IOUtils.closeQuietly(r);
        }
        return l_jobs;
    }

    /**
     * Parse the Gaussian Archive by converting the archive section to one long
     * String. It assumes that there is a GINC keywords in the beginning of line of
     * the Gaussian Archive. This is actually unsafe. GINC (Gaussian Inc.) keyword 
     * can be changed (what I was told).
     * 
     * @param js
     * @return null if no archive is found otherwise archive is returned.
     */
    public static String extractArchive(JobSection js) {
        // Concatenate Archive to String
        int fia = -1; // first index of archive
        for (int i = 0; i < js.size(); i++) {
            if (js.get(i).matches("^ \\d\\\\\\d\\\\GINC.*")) {
                fia = i;
                break;
            }
        }

        if (fia >= 0) {
            String archive = "";
            for (int i = fia; i < js.size(); i++) {
                archive += js.get(i).substring(1);
                if (archive.endsWith("\\@")) {
                    return archive;
                }
            }
            logger.warn("End of Job Section before the normal termination"
                    + " of Gaussian Archive. This should not happen unless"
                    + " there is a bug in the codes.");
            return archive;
        } else {
            return null; // no archive found
        }

    }

    public static Archive parseArchive(String archive) {
        //1\1\GINC-ESCHER\FOpt\UB3LYP\6-311+G(d,p)\C1O2\CPC36\08-Feb-2010\0\\#p ub3lyp/6-311+G(d,p) opt=(MaxCyc=400, Tight) freq #GFInput Population=None SCF=(Tight, MaxCyc=400) #Integral(Grid=UltraFine) Guess=Mix NoSymmetry\\NewComb ub3lyp/6-311+G(d,p)\\0,1\O,-0.1386776658,1.1007995912,0.0029802492\C,-1.2884000036,0.9408999649,-0.0001333362\O,-2.4381223306,0.781000444,-0.003246913\\Version=AM64L-G03RevE.01\HF=-188.646915\S2=0.\S2-1=0.\S2A=0.\RMSD=4.954e-09\RMSF=3.005e-07\Thermal=0.\Dipole=0.0000002,0.,0.\PG=C*V [C*(O1C1O1)]\\@
        //1\1\GINC-ESCHER\Freq\UB3LYP\6-311+G(d,p)\C1O2\CPC36\08-Feb-2010\0\\#P Geom=AllCheck Guess=Read SCRF=Check GenChk UB3LYP/6-311+G(d,p) Freq\\NewComb ub3lyp/6-311+G(d,p)\\0,1\O,-0.1386776658,1.1007995912,0.0029802492\C,-1.2884000036,0.9408999649,-0.0001333362\O,-2.4381223306,0.781000444,-0.003246913\\Version=AM64L-G03RevE.01\HF=-188.646915\S2=0.\S2-1=0.\S2A=0.\RMSD=3.123e-09\RMSF=2.768e-07\ZeroPoint=0.0116875\Thermal=0.0143084\Dipole=-0.0000007,-0.0000001,0.\DipoleDeriv=-1.243034,-0.1350503,-0.0026298,-0.1350479,-0.2907741,-0.0003657,-0.0026296,-0.0003657,-0.271999,2.4860675,0.2701006,0.0052596,0.2700956,0.5815481,0.0007315,0.0052593,0.0007314,0.5439979,-1.2430335,-0.1350503,-0.0026298,-0.1350477,-0.290774,-0.0003657,-0.0026296,-0.0003657,-0.2719989\Polar=25.6683835,2.320784,9.304007,0.0451905,0.0062849,8.9812886\PG=C*V [C*(O1C1O1)]\NImag=0\\1.03489274,0.13879656,0.05619523,0.00270317,0.00037580,0.03689803,-0.95025832,-0.12189521,-0.00237408,1.90051678,-0.12189521,-0.09073576,-0.00033003,0.24379038,0.18147139,-0.00237407,-0.00033003,-0.07378806,0.00474814,0.00066007,0.14757601,-0.08463442,-0.01690135,-0.00032910,-0.95025846,-0.12189518,-0.00237407,1.03489288,-0.01690135,0.03454053,-0.00004577,-0.12189517,-0.09073563,-0.00033003,0.13879652,0.05619510,-0.00032910,-0.00004577,0.03689003,-0.00237407,-0.00033003,-0.07378795,0.00270317,0.00037580,0.03689792\\0.00000051,0.00000018,0.,0.00000010,0.,0.,-0.00000060,-0.00000017,0.\\\@
        Matcher matcher = Archive.ARCHIVE_PATTERN.matcher(archive);
        if (matcher.find()) {
            Archive arc = new Archive();
            arc.setJobType(matcher.group(1));
            arc.setMethod(matcher.group(2));
            arc.setBasis(matcher.group(3));
            arc.setEmpiricalFormula(matcher.group(4));
            arc.setRoute(matcher.group(5));
            arc.setTitle(matcher.group(6));
            return arc;
        } else {
            logger.warn("Archive is not found for a given String : " + archive);
            return null;
        }
    }
    private static final Pattern ChargeAndSpinMultiplicityPattern = Pattern.compile(" Charge =\\s*(\\d+)\\s*Multiplicity =\\s*(\\d+)\\s*");

    public static boolean parseInlineChargeAndSpinMultiplicity(String line, MolecularInfoImpl molecularInfo) {
        // Charge =  0 Multiplicity = 1
        Matcher m = ChargeAndSpinMultiplicityPattern.matcher(line);
        if (m.matches()) {
            molecularInfo.setCharge(Integer.parseInt(m.group(1)));
            molecularInfo.setMultiplicity(Integer.parseInt(m.group(2)));
            return true;
        } else {
            return false;
        }
    }
    //  SCF Done:  E(UB97-1+HF-B97-1) =  -485.113819340     A.U. after   13 cycles
    private static final Pattern SCFDonePattern = Pattern.compile(" SCF Done:.*?=\\s*(" + DoubleTool.NUMBER_REGEX + ")\\s+A\\.U\\.\\s+.*");

    /**
     * Repeatedly read SCF energy and replace the SCF energy value in jDoc.
     * Not efficient but insure to get the last one. However, if there is an
     * unexpected thing in the file, there might be a problem.
     * @param line
     * @param computedInfo
     * @return  
     */
    public static boolean parseInlineSCFDone(String line, ComputedInfoImpl computedInfo) {
        Matcher matcher = SCFDonePattern.matcher(line);
        if (matcher.matches()) {
            computedInfo.setFinalSCFEnergyInHartree(DoubleTool.parseDouble(matcher.group(1)));
            return true;
        } else {
            return false;
        }
    }
    //  Rotational symmetry number  1.
    private static final Pattern RotationalSymmetryPattern = Pattern.compile(" Rotational symmetry number\\s+(\\d+)\\.");

    public static boolean parseInlineRotationalSymmetry(String line, MolecularInfoImpl molecularInfo) {
        Matcher matcher = RotationalSymmetryPattern.matcher(line);
        if (matcher.matches()) {
            molecularInfo.setRotationalSymmetryNumber(Integer.parseInt(matcher.group(1)));
            return true;
        } else {
            return false;
        }
    }

    public static List<String> tokenize(String str) {
        List<String> str_list = new ArrayList<String>();
        StringTokenizer tempStringTokenizer = new StringTokenizer(str);
        while (tempStringTokenizer.hasMoreTokens()) {
            str_list.add(((String) tempStringTokenizer.nextElement()).trim());
        }
        return str_list;
    }

    public static List<Double> parseDoubles(List<String> dbs) {
        List<Double> list_dbs = new ArrayList<Double>();
        for (String db : dbs) {
            list_dbs.add(DoubleTool.parseDouble(db));
        }
        return list_dbs;
    }
//==JobType
//Scan
//FOpt
//POpt
//Freq
//SP
//==Basis
//ZDO
//6-31+G(d)
//6-31G(d,p)
//6-311++G(df,pd)
//STO-3G
//Aug-CC-pVDZ
//6-31G(d)
//3-21G
//6-311+G(2d,p)
//==Method
//RB3PW91
//RB3LYP
//RBLYP
//UMP2-FC
//RHF
//UHF
//UPBEPBE
//UB3LYP
//RPM3
}
