package uk.ac.cam.ceb.como.io.chem.file.parser.g09.util;

import uk.ac.cam.ceb.como.compchem.info.BasicInfoImpl;
import java.lang.reflect.InvocationTargetException;
import java.util.regex.Pattern;
import org.apache.commons.beanutils.BeanUtils;

/**
 *
 * @author pb556
 */
public class Archive extends BasicInfoImpl {
    //1\1\GINC-ELIZABETH\POpt\RB3PW91\6-311++G(df,pd)\C2H3Br1\RKANTERS\20-Aug-2004\1\\# OPT=Z-MATRIX B3PW91 6-311++G(DF,PD) FREQ TEMPERATURE=14\\Acetylene HBr B3PW91/6-311++G(df,pd) forcing Cs\\0,1\X\X,1,rx\C,2,rc,1,a90\C,2,rc,1,a90,3,t180,0\H,2,rh1,1,a1,3,t0,0\H,2,rh2,1,a2,3,t180,0\H,2,xh,3,a3,1,t180,0\Br,2,xbr,3,a4,1,t180,0\\rc=0.60002733\rh1=1.66512683\rh2=1.66512683\a1=89.62647358\a2=89.62647358\xh=2.47279549\xbr=3.90373211\a3=90.\a4=90.\rx=1.\a90=90.\t180=180.\t0=0.\\Version=Sun64-SVR4-Unix-G03RevB.05\State=1-A1\HF=-2652.0612678\RMSD=3.607e-09\RMSF=6.705e-06\Dipole=0.,0.,-0.6890684\PG=C02V [C2(H1Br1),SGV(C2H2)]\\@
    //public static final String ARCHIVE_REGEX = "\\d\\\\\\d\\\\GINC.*?\\\\([\\w]+?)\\\\(.+?)\\\\(.+?)\\\\(.+?)\\\\.+?\\\\.+?\\\\\\d+?\\\\\\\\\\s*(#.+?)\\\\\\\\\\s*(#.+?)\\\\\\\\.*";

    public static final String ARCHIVE_REGEX = "\\d\\\\\\d\\\\GINC.*?\\\\([\\w]+?)\\\\(.+?)\\\\(.+?)\\\\(.+?)\\\\.+?\\\\.+?\\\\\\d+?\\\\\\\\\\s*(#.+?)\\\\\\\\\\s*(.+?)\\\\\\\\";
    public static final Pattern ARCHIVE_PATTERN = Pattern.compile(ARCHIVE_REGEX);
    private String route;
    private String title;
    
    public Archive() {
        
    }

    public Archive(BasicInfoImpl basicInfo) {
        try {
            BeanUtils.copyProperties(this, basicInfo);
        } catch (IllegalAccessException ex) {
        } catch (InvocationTargetException ex) {
        }
    }

    /**
     * Get the value of title
     *
     * @return the value of title
     */
    public String getTitle() {
        return title;
    }

    /**
     * Set the value of title
     *
     * @param title new value of title
     */
    public void setTitle(String title) {
        this.title = title;
    }


    /**
     * Get the value of route
     *
     * @return the value of route
     */
    public String getRoute() {
        return route;
    }

    /**
     * Set the value of route
     *
     * @param route new value of route
     */
    public void setRoute(String route) {
        this.route = route;
    }
}
