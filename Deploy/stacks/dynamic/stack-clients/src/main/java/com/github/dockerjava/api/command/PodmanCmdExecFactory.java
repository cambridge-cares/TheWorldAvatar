package com.github.dockerjava.api.command;

import java.io.Closeable;
import java.io.IOException;

public interface PodmanCmdExecFactory extends Closeable {

    // pods
    public ListPodsCmd.Exec createListPodsCmdExec();

    @Override
    void close() throws IOException;
}
