/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.converter.nasa7.format.file;

import freemarker.template.TemplateException;
import uk.ac.cam.ceb.como.chem.info.alias.ThermoAnalyzable;
import uk.ac.cam.ceb.como.chem.structure.util.StructureTools;
import uk.ac.cam.ceb.como.thermo.converter.nasa7.fitting.LargrangianNASA7PolyFitting;
import com.cmclinnovations.io.writer.freemarker.FreeMarkerWriter;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.time.DateFormatUtils;
import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.chem.structure.Atom;
import uk.ac.cam.ceb.como.thermo.calculator.nasa7.NASA7ThermoCalculator;
import uk.ac.cam.ceb.como.thermo.calculator.StatMechThermoCalculator;
import uk.ac.cam.ceb.como.chem.structure.util.CompoundConverter;

/**
 *
 * @author pb556
 */
public class NASA7Format {

    private Logger logger = Logger.getLogger(getClass());
    private FreeMarkerWriter NASAMarkerWriter = new FreeMarkerWriter();

    /**
     * NASA Polynomial in CHEMKIN format using default fitting NASA7Fitting of 5000 points for
     * low temperature range and 4000 points for high temperature range. Use
     * {@link #formatCHEMKIN(String, JChemDocument, NASAPolynomial) }
     * if you want the change the number of fitting points.
     *
     * @param id id which is used as species name.
     * @param thermoCal An instance of {@link ThermoCalculator}.
     * @return String NASA Polynomial in CHEMKIN format.
     * @see #formatCHEMKIN(String, JChemDocument, NASAPolynomial)
     */
    public String formatCHEMKIN(String id, StatMechThermoCalculator thermoCal) throws Exception {
        LargrangianNASA7PolyFitting nasafit = new LargrangianNASA7PolyFitting();
        nasafit.setFittingPoints(5000, 4000);
        nasafit.setThermoCalculator(thermoCal);
        return formatCHEMKIN(id, thermoCal.getThermoAnalyzable(), nasafit.getNASA7ThermoCalculator());
    }
    
    public String formatCHEMKIN(String id, StatMechThermoCalculator thermoCal, int numLowFittingPoints, int numHighFittingPoints) throws Exception {
        LargrangianNASA7PolyFitting nasafit = new LargrangianNASA7PolyFitting();
        nasafit.setFittingPoints(numLowFittingPoints, numHighFittingPoints);
        nasafit.setThermoCalculator(thermoCal);
        return formatCHEMKIN(id, thermoCal.getThermoAnalyzable(), nasafit.getNASA7ThermoCalculator());
    }

    /**
     * Get a String of NASA Polynomial in CHEMKIN format. Alternatively, you
     * can use the default formatter {@link #formatCHEMKIN(String, ThermoCalculator) }.
     *
     * @param id id which is used as species name.
     * @param thermoAnalyzable JChemDocument object for getting additional information about composition of molecule.
     * @param nasa NASAPolynomial object to be formatted.
     * @return String NASA Polynomial in CHEMKIN format.
     * @see #formatCHEMKIN(String, ThermoCalculator)
     */
    public String formatCHEMKIN(String id, ThermoAnalyzable thermoAnalyzable, NASA7ThermoCalculator nasa) {
        //CH4               110203H   4C   1    0    0G   300.000  4000.000 1000.00    0 1
        // 0.47238333E+00 0.12680758E-01-0.55093741E-05 0.11295575E-08-0.89103779E-13    2
        //-0.96424500E+04 0.16199090E+02 0.38717898E+01-0.42480466E-02 0.24540181E-04    3
        //-0.21780766E-07 0.63010622E-11-0.10144425E+05 0.66008135E+00                   4
        String chem = "";
        String nasaString = null;
        Map NASAMarkerMap = makeCommonNASAMarkerMap(nasa);

        if (id.length() > 18) {
            logger.warn("Species name is too long for CHEMKIN thermo format.");
            NASAMarkerMap.put("Warn", "Warning! : Species name, " + id + ", is too long for CHEMKIN thermo format.");
            chem = id.substring(0, 18);
        } else {
            chem = StringUtils.rightPad(id, 18);
        }
        NASAMarkerMap.put("SpeciesName", chem);
        List<Atom> vec_elem = CompoundConverter.getImplodedAtomListByElement(thermoAnalyzable);
        if (vec_elem.size() > 5) {
            throw new UnsupportedOperationException("CHEMKIN does not support species with more than 5 elements.");
        } else {
            for (int i = 0; i < 5; i++) {
                String symbol = "  ";
                String count = "   ";
                if (i < vec_elem.size()) {
                    symbol = StringUtils.rightPad(vec_elem.get(i).getElement().getSymbol(), 2);
                    count = StringUtils.leftPad(vec_elem.get(i).getCount() + "", 3);
                }
                NASAMarkerMap.put("ElemSymbol" + (i + 1), symbol);
                NASAMarkerMap.put("ElemCount" + (i + 1), count);
            }
        }

        try {
            NASAMarkerWriter.setTemplate(getClass(), "nasa7.ftl");
            nasaString = NASAMarkerWriter.process(NASAMarkerMap);
        } catch (TemplateException ex) {
            System.out.println(ex);
        } catch (IOException ex) {
            System.out.println(ex);
        }

        return nasaString;
    }

    /**
     * NASA Polynomial in Cantera format using default fitting NASA7Fitting of 5000 points for
     * low temperature range and 4000 points for high temperature range.Use
     * {@link #formatCantera(String, JChemDocument, NASAPolynomial)} if you want the change the number of fitting points.
     *
     * @param id id which is used as species name.
     * @param thermoCal An instance of {@link ThermoCalculator}.
     * @return String NASA Polynomial in Cantera format.
     * @see #formatCHEMKIN(String, JChemDocument, NASAPolynomial)
     */
    public String formatCantera(String id, StatMechThermoCalculator thermoCal) throws Exception {
        LargrangianNASA7PolyFitting nasafit = new LargrangianNASA7PolyFitting();
        nasafit.setFittingPoints(5000, 4000);
        nasafit.setThermoCalculator(thermoCal);
        return formatCantera(id, thermoCal.getThermoAnalyzable(), nasafit.getNASA7ThermoCalculator());
    }
    
    public String formatCantera(String id, StatMechThermoCalculator thermoCal, int numLowFittingPoints, int numHighFittingPoints) throws Exception {
        LargrangianNASA7PolyFitting nasafit = new LargrangianNASA7PolyFitting();
        nasafit.setFittingPoints(numLowFittingPoints, numHighFittingPoints);
        nasafit.setThermoCalculator(thermoCal);
        return formatCantera(id, thermoCal.getThermoAnalyzable(), nasafit.getNASA7ThermoCalculator());
    }

    /**
     * Get a String of NASA Polynomial in Cantera format. Alternatively, you
     * can use the default formatter {@link #formatCantera(String, ThermoCalculator) }
     *
     * @param id id which is used as species name.
     * @param thermoAnalyzable JChemDocument object for getting additional information about composition of molecule.
     * @param nasa NASAPolynomial object to be formatted.
     * @return String NASA Polynomial in Cantera format.
     * @see #formatCantera(String, ThermoCalculator)
     */
    public String formatCantera(String id, ThermoAnalyzable thermoAnalyzable, NASA7ThermoCalculator nasa) {
        String canteraString = null;
        Map CanteraMarkerMap = makeCommonNASAMarkerMap(nasa);
        CanteraMarkerMap.put("SpeciesName", id);

        String cantera = "";
        List<Atom> atoms = StructureTools.getImplodedAtomListByElement(thermoAnalyzable);
        for (Atom atom : atoms) {
            cantera += atom.getElement().getSymbol() + ":" + atom.getCount() + " ";
        }
        CanteraMarkerMap.put("ElementalComposition", cantera);

        try {
            NASAMarkerWriter.setTemplate(getClass(), "cantera.ftl");
            canteraString = NASAMarkerWriter.process(CanteraMarkerMap);
        } catch (TemplateException ex) {
            System.out.println(ex);
        } catch (IOException ex) {
            System.out.println(ex);
        }
        return canteraString;
    }

    /**
     * Create a marker map with common properties for both chemkin and cantera template.
     *
     * @param thermoAnalyzable
     * @param nasa
     * @return
     */
    private Map makeCommonNASAMarkerMap(NASA7ThermoCalculator nasa) {
        Map m = new HashMap();

        m.put("TLow", formatTemperature(nasa.getNASA7().getTemperatureLowerBound()));
        m.put("TMid", formatTemperature(nasa.getNASA7().getTemperatureMidPoint()));
        m.put("THigh", formatTemperature(nasa.getNASA7().getTemperatureUpperBound()));
        m.put("TLowF", StringUtils.leftPad(formatTemperature(nasa.getNASA7().getTemperatureLowerBound()), 10));
        m.put("THighF", StringUtils.leftPad(formatTemperature(nasa.getNASA7().getTemperatureUpperBound()), 10));
        m.put("TMidF", StringUtils.leftPad(formatTemperature(nasa.getNASA7().getTemperatureMidPoint()), 8));

        String date = DateFormatUtils.format(Calendar.getInstance(), "ddMMyy");
        String dateTime = DateFormatUtils.format(Calendar.getInstance(), "dd MMM yyyy 'at' HH:mm:ss (z)");
        m.put("Phase", "G");
        m.put("Date", date);
        m.put("Note", dateTime);

        Map poly7 = new HashMap(14);
        for (int i = 0; i < 7; i++) {
            poly7.put("LoTemp" + (i + 1), formatNASACoeff(nasa.getNASA7().getALow(i)));
            poly7.put("HiTemp" + (i + 1), formatNASACoeff(nasa.getNASA7().getAHigh(i)));
        }
        m.put("Poly", poly7);
        return m;
    }
    /**
     * Decimal format for temperature string output.
     */
    private static final DecimalFormat dfTemp = new DecimalFormat("####0.00");
    /**
     * Decimal format for NASA coefficient.
     */
    private static final DecimalFormat dfCoef = new DecimalFormat(" 0.00000000E00;-0.00000000E00");
    /**
     * Pattern matching for positive exponent.
     */
    private static final Pattern p = Pattern.compile("E\\d");

    /**
     * Format <code>double</code> value for NASA coefficient output.
     * @param val NASA coefficient.
     * @return <code>String</code> of NASA coefficient.
     */
    public static String formatNASACoeff(double val) {
        String num_str = dfCoef.format(val);
        return ((p.matcher(num_str).find()) ? num_str.replace("E", "E+") : num_str);
    }

    /**
     * Format <code>double</code> value of temperature to proper string.
     * @param T Temperature.
     * @return <code>String</code> of temperature.
     */
    public static String formatTemperature(double T) {
        return dfTemp.format(T);
    }
}
