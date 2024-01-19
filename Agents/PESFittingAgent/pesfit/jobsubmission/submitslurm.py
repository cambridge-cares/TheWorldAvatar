from pesfit.kgoperations.javagateway import jpsBaseLibGW
from datetime import datetime
from pesfit.jobsubmission.slurminputs import slurminputs
import json
import os
import math

jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.slurm.job.*")
#jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.slurm.job.configuartion.*")

def submitslurm(conf_file):
    resource_path = os.path.relpath(os.path.join(os.path.dirname(__file__), '..' , 'resources'))
    #properties_file_path = os.path.join(resource_path, 'configuration.json')
    properties_file_path = conf_file
    jobSubmission = initAgentProperties(properties_file_path)

    slurminputs(conf_file)
    SlurmScript_obj = jpsBaseLib_view.java.io.File('Slurm.sh')
    Input_obj = jpsBaseLib_view.java.io.File('input.zip')
    timestamp = (datetime.timestamp(datetime.now()))
    output = jobSubmission.setUpJob(str(' '), SlurmScript_obj, Input_obj, int(timestamp) )
    workspace = jobSubmission.workspaceDirectory
    while True:
        jobSubmission.monitorJobs()

def initAgentProperties(properties_file_path):
    with open(properties_file_path) as json_file:
        Properties = json.load(json_file)
    jobSubmission = jpsBaseLib_view.JobSubmission(Properties['agent.class'], Properties['hpc.address'])
    jobSubmission.slurmJobProperty.setHpcServerLoginUserName(Properties['hpc.server.login.user.name'])
    jobSubmission.slurmJobProperty.setHpcServerLoginUserPassword(Properties['hpc.server.login.user.password'])
    jobSubmission.slurmJobProperty.setAgentPeriodicActionInterval(Properties['agent.periodic.action.interval'])
    jobSubmission.slurmJobProperty.setAgentInitialDelayToStartJobMonitoring(Properties['agent.initial.delay.to.start'])
    jobSubmission.slurmJobProperty.setMaxNumberOfHPCJobs(Properties['max.number.of.hpc.jobs'])
    jobSubmission.slurmJobProperty.setAgentCompletedJobsSpacePrefix(Properties['agent.completed.job.space.prefix'])
    jobSubmission.slurmJobProperty.setAgentFailedJobsSpacePrefix(Properties['agent.failed.job.space.prefix'])
    jobSubmission.slurmJobProperty.setAgentWorkspacePrefix(Properties['agent.class'])
    jobSubmission.slurmJobProperty.setAgentClass(Properties['agent.class'])
    jobSubmission.slurmJobProperty.setHpcAddress(Properties['hpc.address'])
    jobSubmission.slurmJobProperty.setInputFileName(Properties['input.file.name'])
    jobSubmission.slurmJobProperty.setInputFileExtension(Properties['input.file.extension'])
    jobSubmission.slurmJobProperty.setJsonInputFileName(Properties['json.input.file.name'])
    jobSubmission.slurmJobProperty.setJsonFileExtension(Properties['json.file.extension'])
    jobSubmission.slurmJobProperty.setSlurmScriptFileName(Properties['slurm.script.file.name'])
    jobSubmission.slurmJobProperty.setOutputFileName(Properties['output.file.name'])
    jobSubmission.slurmJobProperty.setOutputFileExtension(Properties['output.file.extension'])
    return jobSubmission