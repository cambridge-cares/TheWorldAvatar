package com.github.dockerjava.core.exec;

import com.github.dockerjava.api.command.RemovePodCmd;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.core.MediaType;
import com.github.dockerjava.core.WebTarget;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RemovePodCmdExec extends AbstrSyncDockerCmdExec<RemovePodCmd, Void> implements
        RemovePodCmd.Exec {

    private static final Logger LOGGER = LoggerFactory.getLogger(RemovePodCmdExec.class);

    public RemovePodCmdExec(WebTarget baseResource, DockerClientConfig dockerClientConfig) {
        super(baseResource, dockerClientConfig);
    }

    @Override
    protected Void execute(RemovePodCmd command) {
        WebTarget webTarget = getBaseResource().path("/pods/" + command.getPodId());

        LOGGER.trace("DELETE: {}", webTarget);
        webTarget.request().accept(MediaType.APPLICATION_JSON).delete();

        return null;
    }

}