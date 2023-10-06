package uk.ac.cam.cares.jps.network.otherinfo;

import java.util.HashMap;
import java.util.Map;

import uk.ac.cam.cares.jps.model.building.Room;
import uk.ac.cam.cares.jps.model.building.Workspace;

public class OtherInfoResponse {
    Map<String, HashMap<String, String>> otherInfo = new HashMap<>();
    HashMap<String, Workspace> workspaces = new HashMap<>();
    HashMap<String, Room> rooms = new HashMap<>();

    public Map<String, HashMap<String, String>> getOtherInfo() {
        return otherInfo;
    }

    public HashMap<String, Workspace> getWorkspaces() {
        return workspaces;
    }

    public HashMap<String, Room> getRooms() {
        return rooms;
    }
}
