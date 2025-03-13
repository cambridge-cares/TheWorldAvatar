package uk.ac.cam.cares.jps.timeline.model.bottomsheet;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.timeline.model.trajectory.TrajectorySegment;

public class Session {
    private final String sessionId;
    private final String sessionTitle;
    private final List<ActivityItem> activityItemList;
    private List<ActivityItem> shownList;
    private final List<ActivityItem> EMPTY_LIST = new ArrayList<>();

    public Session(String sessionId, List<ActivityItem> activityItemList, String sessionTitle) {
        this.sessionId = sessionId;
        this.activityItemList = activityItemList;
        this.shownList = activityItemList;
        this.sessionTitle = sessionTitle;
    }

    public String getSessionId() {
        return sessionId;
    }

    public List<ActivityItem> getShownList() {
        return shownList;
    }

    public void setShownListAsActivityList() {
        this.shownList = this.activityItemList;
    }

    public void setShownListAsEmptyList() {
        this.shownList = EMPTY_LIST;
    }

    public String getSessionTitle() {
        return sessionTitle;
    }

    public boolean containsSegment(TrajectorySegment segment) {
        if(sessionId.equals(segment.sessionId())) {
            for (ActivityItem item : this.activityItemList) {
                if (item.getId() == segment.id()) {
                    return true;
                }
            }
            return false;
        }
        else {
            return false;
        }
    }

}