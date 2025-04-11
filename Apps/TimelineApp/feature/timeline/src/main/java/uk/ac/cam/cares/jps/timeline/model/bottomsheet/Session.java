package uk.ac.cam.cares.jps.timeline.model.bottomsheet;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.timeline.model.trajectory.TrajectorySegment;

public class Session {
    private final String sessionId;
    private final String sessionTitle;
    private final List<TrajectorySegment> trajectorySegments;
    private final List<ActivityItem> activityList;

    public Session(String sessionId, String sessionTitle, List<TrajectorySegment> trajectorySegments) {
        this.sessionId = sessionId;
        this.trajectorySegments = trajectorySegments;
        this.activityList = parseTrajectorySegment(trajectorySegments);
        this.sessionTitle = sessionTitle;
    }

    private List<ActivityItem> parseTrajectorySegment(List<TrajectorySegment> trajectorySegments) {
        List<ActivityItem> activities = new ArrayList<>();

        for(TrajectorySegment segment : trajectorySegments) {
            activities.add(segment.getActivity());
        }

        return activities;
    }

    public List<TrajectorySegment> getTrajectorySegments() { return this.trajectorySegments;}

    public List<ActivityItem> getActivityList() { return this.activityList;}

    public String getSessionId() {
        return sessionId;
    }

    public String getSessionTitle() {
        return sessionTitle;
    }

    public boolean containsSegment(TrajectorySegment segment) {
        if(segment == null) {
            return false;
        }
        if(sessionId.equals(segment.getSessionId())) {
            for (ActivityItem item : this.activityList) {
                if (item.getId() == segment.getId()) {
                    return true;
                }
            }
        }
        return false;
    }

}