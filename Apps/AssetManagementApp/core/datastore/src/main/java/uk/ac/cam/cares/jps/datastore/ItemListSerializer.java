package uk.ac.cam.cares.jps.datastore;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.datastore.core.Serializer;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import kotlin.Unit;
import kotlin.coroutines.Continuation;

public class ItemListSerializer implements Serializer<ItemList> {
    @Override
    public ItemList getDefaultValue() {
        return ItemList.getDefaultInstance();
    }

    @Nullable
    @Override
    public Object readFrom(@NonNull InputStream inputStream, @NonNull Continuation<? super ItemList> continuation) {
        try {
            return ItemList.parseFrom(inputStream);
        } catch (IOException e) {
            throw new RuntimeException("Cannot read proto.");
        }
    }

    @Nullable
    @Override
    public Object writeTo(ItemList itemList, @NonNull OutputStream outputStream, @NonNull Continuation<? super Unit> continuation) {
        try {
            itemList.writeTo(outputStream);
            return null;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
