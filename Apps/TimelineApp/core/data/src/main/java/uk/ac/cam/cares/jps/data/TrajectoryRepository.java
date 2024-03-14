package uk.ac.cam.cares.jps.data;

import uk.ac.cam.cares.jps.network.TrajectoryNetworkSource;

public class TrajectoryRepository{
    private TrajectoryNetworkSource trajectoryNetworkSource;

    public TrajectoryRepository(TrajectoryNetworkSource trajectoryNetworkSource) {
        this.trajectoryNetworkSource = trajectoryNetworkSource;
    }

    public void getTrajectory(RepositoryCallback<String> callback) {
        trajectoryNetworkSource.getTrajectory(callback::onSuccess, volleyError -> callback.onFailure(new Throwable("Failed to get trajectory")));
    }
}
