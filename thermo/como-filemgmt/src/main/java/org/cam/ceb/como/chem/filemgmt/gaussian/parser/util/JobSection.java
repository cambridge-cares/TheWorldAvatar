package org.cam.ceb.como.chem.filemgmt.gaussian.parser.util;

import java.util.List;

/**
 *
 * @author wp214
 */
public class JobSection extends StringList {

    private final boolean normalTerminated;
    private final int order;

    public JobSection(List<String> lines, int jobOrder, boolean normalTerminated) {
        super(lines);
        this.order = jobOrder;
        this.normalTerminated = normalTerminated;
    }

    /**
     * Get the value of order
     *
     * @return the value of order
     */
    public int getOrder() {
        return order;
    }

    /**
     * Get the value of normalTerminated
     *
     * @return the value of normalTerminated
     */
    public boolean getNormalTerminated() {
        return normalTerminated;
    }
}
