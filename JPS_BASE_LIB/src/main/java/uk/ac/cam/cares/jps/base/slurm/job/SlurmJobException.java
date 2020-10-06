package uk.ac.cam.cares.jps.base.slurm.job;

/**
 * The exception class to catch all possible exceptions within the<br>
 * Slurm Job API. 
 * 
 * @author msff2
 *
 */
public class SlurmJobException extends Exception{
    public SlurmJobException() {
        super();
    }

    public SlurmJobException(String message) {
        super(message);
    }

    public SlurmJobException(String message, Throwable cause) {
        super(message, cause);
    }

    public SlurmJobException(Throwable cause) {
        super(cause);
    }

    protected SlurmJobException(String message, Throwable cause,
            boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
