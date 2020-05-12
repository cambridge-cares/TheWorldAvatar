package uk.ac.cam.cares.jps.base.test;

import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import uk.ac.cam.cares.jps.base.slurm.job.configuration.SlurmJobProperty;
import uk.ac.cam.cares.jps.base.slurm.job.configuration.SpringConfiguration;



public class Testing {

	public static ApplicationContext applicationContext;
	public static SlurmJobProperty slurmJobProperty;
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		 if (applicationContext == null) {
				applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
			}
			if (slurmJobProperty == null) {
				slurmJobProperty = applicationContext.getBean(SlurmJobProperty.class);
			}
			System.out.println(slurmJobProperty.getHpcAddress());
	}

}
