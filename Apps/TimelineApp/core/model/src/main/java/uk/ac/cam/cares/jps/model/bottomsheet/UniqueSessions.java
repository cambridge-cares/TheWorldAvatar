package uk.ac.cam.cares.jps.model;

import java.util.ArrayList;
import java.util.List;

public class UniqueSessions {
    private final String sessionId;
    private final String sessionTitle;
    private final List<ActivityItem> activityItemList;
    private List<ActivityItem> shownList;
    private final List<ActivityItem> EMPTY_LIST = new ArrayList<>();

    public UniqueSessions(String sessionId, List<ActivityItem> activityItemList, String sessionTitle) {
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

}