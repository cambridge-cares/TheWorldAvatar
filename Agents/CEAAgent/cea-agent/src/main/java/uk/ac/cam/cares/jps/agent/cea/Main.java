package uk.ac.cam.cares.jps.agent.cea;

import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.catalina.core.StandardContext;
import org.apache.catalina.startup.Tomcat;

public class Main {
    public static final int PORT = 8084;

    public static void main(String[] args) throws Exception {

        String webappDirLocation = "/";
        Tomcat tomcat = new Tomcat();
        tomcat.setPort(PORT);

        StandardContext ctx = (StandardContext) tomcat.addWebapp("/", new File(webappDirLocation).getAbsolutePath());

        CEAAgent agent = new CEAAgent();
        Tomcat.addServlet(ctx, "CEAAgent", agent);
        ctx.addServletMappingDecoded("/*", "CEAAgent");

        try {
            tomcat.start();
            tomcat.getServer().await();
        } catch (Exception ex) {
            Logger.getLogger(Main.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            tomcat.destroy();
        }

    }

}