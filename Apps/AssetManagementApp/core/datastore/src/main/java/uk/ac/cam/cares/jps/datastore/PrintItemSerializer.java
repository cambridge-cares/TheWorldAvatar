package uk.ac.cam.cares.jps.datastore;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.datastore.core.Serializer;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import kotlin.Unit;
import kotlin.coroutines.Continuation;

public class PrintItemSerializer implements Serializer<PrintItem> {
    @Override
    public PrintItem getDefaultValue() {
        return PrintItem.getDefaultInstance();
    }

    @Nullable
    @Override
    public Object readFrom(@NonNull InputStream inputStream, @NonNull Continuation<? super PrintItem> continuation) {
        try {
            return PrintItem.parseFrom(inputStream);
        } catch (IOException e) {
            throw new RuntimeException("Cannot read proto.");
        }
    }

    @Nullable
    @Override
    public Object writeTo(PrintItem printItem, @NonNull OutputStream outputStream, @NonNull Continuation<? super Unit> continuation) {
        try {
            printItem.writeTo(outputStream);
            return null;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
