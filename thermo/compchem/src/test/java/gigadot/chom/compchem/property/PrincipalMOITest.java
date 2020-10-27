package gigadot.chom.compchem.property;

/**
 *
 * @author Weerapong
 */
public class PrincipalMOITest {

    private final PMOI pmoi = new PMOI();

    public PrincipalMOITest() {
        pmoi.setMOIIn_amu_rbohr2(3766.72418, 3773.14415, 4025.22781);
        pmoi.setPrincipalAxis(0, -0.08473, 0.78447, -0.61435);
        pmoi.setPrincipalAxis(1, -0.12927, 0.60270, 0.78743);
        pmoi.setPrincipalAxis(2, 0.98798, 0.14614, 0.05034);
    }
}