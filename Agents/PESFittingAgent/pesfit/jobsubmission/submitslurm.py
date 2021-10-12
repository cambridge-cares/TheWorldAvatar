from pesfit.kgoperations.javagateway import jpsBaseLibGW
from pesfit.kgoperations.querykg import jpsBaseLib_view

jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.slurm.job.*")

def submitslurm():
    # submit the slurm job to the hpc
    JobSubmission = jpsBaseLib_view.JobSubmission
    JobSubmission.monitorJobs()