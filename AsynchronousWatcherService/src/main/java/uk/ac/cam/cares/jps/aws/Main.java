package uk.ac.cam.cares.jps.aws;

import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.catalina.core.StandardContext;
import org.apache.catalina.startup.Tomcat;
import org.glassfish.jersey.server.ResourceConfig;
import org.glassfish.jersey.servlet.ServletContainer;

public class Main {
    public static final int PORT = 8084;

    public static void main(String[] args) throws Exception {

        String webappDirLocation = "/";
        Tomcat tomcat = new Tomcat();
        tomcat.setPort(PORT);

        StandardContext ctx = (StandardContext) tomcat.addWebapp("/", new File(webappDirLocation).getAbsolutePath());

        Tomcat.addServlet(ctx, ResourceLoader.WATCHER_SERVLET, resourceConfig());
        ctx.addServletMapping("/*", ResourceLoader.WATCHER_SERVLET);

        try {
            tomcat.start();
            tomcat.getServer().await();
        } catch (Exception ex) {
            Logger.getLogger(Main.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            tomcat.destroy();
        }

    }

    private static ServletContainer resourceConfig() {
        return new ServletContainer(new ResourceConfig(
                new ResourceLoader().getClasses()));
    }
}
