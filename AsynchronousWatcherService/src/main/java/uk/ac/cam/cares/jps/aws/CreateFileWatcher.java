package uk.ac.cam.cares.jps.aws;

import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;

import java.io.File;
import java.io.IOException;
import java.nio.file.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

public class CreateFileWatcher extends Thread implements Watcher {
    private final File file;
    private final int timeout;
    private final AtomicBoolean stop = new AtomicBoolean(false);
    private WatcherCallback callback;
    private boolean anyFile = false;

    public CreateFileWatcher(File file, int timeout) {
        this.file = file;
        this.timeout = timeout;
    }

    public boolean isStopped() {
        return stop.get();
    }

    public void stopThread() {
        stop.set(true);
    }

    public void doOnChange() {
        if (callback != null) {
            callback.call();
        }
        stopThread();
    }

    public void setCallback(WatcherCallback cb) {
        callback = cb;
    }

    public void setWatchAnyFile(boolean flag) {
        anyFile = flag;
    }

    public WatcherCallback getCallback(String url, String json) {

        return () -> {
            HttpClient httpClient = HttpClientBuilder.create().build();
            try {
                HttpPost request = new HttpPost(url);
                StringEntity entity = new StringEntity(json, ContentType.APPLICATION_JSON);
                request.setEntity(entity);
                httpClient.execute(request);
            } catch (IOException e) {
                e.printStackTrace();
            }
        };
    }

    @Override
    public void run() {
        try {
            long startTime = System.currentTimeMillis();
            WatchService watcher = FileSystems.getDefault().newWatchService();
            Path path = file.toPath();

            if (!anyFile) {
                path = path.getParent();
            }

            if (path.toFile().isDirectory()) {
                path.register(watcher, StandardWatchEventKinds.ENTRY_CREATE);


                while (!isStopped()) {
                    WatchKey key;
                    try {
                        key = watcher.poll(25, TimeUnit.MILLISECONDS);
                    } catch (InterruptedException e) {
                        return;
                    }
                    if (key == null) {
                        if (timeout < (System.currentTimeMillis() - startTime)) {
                            stopThread();
                        }
                        Thread.yield();
                        continue;
                    }

                    for (WatchEvent<?> event : key.pollEvents()) {
                        WatchEvent.Kind<?> kind = event.kind();

                        @SuppressWarnings("unchecked")
                        WatchEvent<Path> ev = (WatchEvent<Path>) event;
                        Path filename = ev.context();

                        if (kind == StandardWatchEventKinds.OVERFLOW) {
                            Thread.yield();
                            continue;
                        } else if (kind == java.nio.file.StandardWatchEventKinds.ENTRY_CREATE ||
                                kind == StandardWatchEventKinds.ENTRY_MODIFY) {
                            if (!anyFile) {
                                if (filename.toString().equals(file.getName())) {
                                    doOnChange();
                                }
                            } else {
                                doOnChange();
                            }
                        }

                        boolean valid = key.reset();
                        if (!valid) {
                            break;
                        }
                    }

                    Thread.yield();
                }
                if (isStopped()) {
                    watcher.close();
                    Thread.currentThread().join();
                }
            }
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }
}