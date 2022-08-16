package uk.ac.cam.cares.jps.aws;

import java.util.HashSet;
import java.util.Set;
import javax.ws.rs.core.Application;


public class ResourceLoader extends Application {
    public static final String WATCHER_SERVLET = "AsynchronousWatcherService";

    @Override
    public Set<Class<?>> getClasses() {
        final Set<Class<?>> classes = new HashSet<Class<?>>();

        classes.add(AsynchronousWatcherService.class);
        return classes;
    }
}