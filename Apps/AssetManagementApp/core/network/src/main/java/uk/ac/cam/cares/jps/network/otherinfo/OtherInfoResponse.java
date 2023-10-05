package uk.ac.cam.cares.jps.network.otherinfo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.ac.cam.cares.jps.model.building.Room;
import uk.ac.cam.cares.jps.model.building.Workspace;

public class OtherInfoResponse {
    Map<String, HashMap<String, String>> otherInfo = new HashMap<>();
    List<Workspace> workspaces = new ArrayList<>();
    List<Room> rooms = new ArrayList<>();

    public Map<String, HashMap<String, String>> getOtherInfo() {
        return otherInfo;
    }

    public List<Workspace> getWorkspaces() {
        return workspaces;
    }

    public List<Room> getRooms() {
        return rooms;
    }
}
