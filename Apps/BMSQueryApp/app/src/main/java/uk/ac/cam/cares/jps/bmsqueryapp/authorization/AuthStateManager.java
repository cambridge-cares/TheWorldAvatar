/*
 * Copyright 2017 The AppAuth for Android Authors. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
 * express or implied. See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.ac.cam.cares.jps.bmsqueryapp.authorization;

import android.content.Context;
import android.content.SharedPreferences;
import android.security.keystore.KeyGenParameterSpec;
import android.security.keystore.KeyProperties;
import android.util.Pair;

import androidx.annotation.AnyThread;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import net.openid.appauth.AuthState;
import net.openid.appauth.AuthorizationException;
import net.openid.appauth.AuthorizationResponse;
import net.openid.appauth.RegistrationResponse;
import net.openid.appauth.TokenResponse;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import java.lang.ref.WeakReference;
import java.nio.charset.StandardCharsets;
import java.security.KeyStore;
import java.util.Arrays;
import java.util.Base64;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.ReentrantLock;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;

/**
 * An example persistence mechanism for an {@link AuthState} instance.
 * This stores the instance in a shared preferences file, and provides thread-safe access and
 * mutation.
 */
public class AuthStateManager {

    private static final AtomicReference<WeakReference<AuthStateManager>> INSTANCE_REF =
            new AtomicReference<>(new WeakReference<>(null));

    private static final Logger LOGGER = LogManager.getLogger(AuthStateManager.class);
    private static final String SHARED_PREF_NAME = "AuthState";
    private static final String KEY_STATE = "state";

    private static final String KEY_STORE_NAME = "AndroidKeyStore";
    private static final String CIPHER_TRANSFORMATION = "AES/CBC/PKCS7Padding";
    private static final String KEY_ALIAS = "AuthStateKey";
    private static final String KEY_IV_LENGTH = "ivLength";

    private final SharedPreferences mPrefs;
    private final ReentrantLock mPrefsLock;
    private final AtomicReference<AuthState> mCurrentAuthState;

    @AnyThread
    public static AuthStateManager getInstance(@NonNull Context context) {
        AuthStateManager manager = INSTANCE_REF.get().get();
        if (manager == null) {
            manager = new AuthStateManager(context.getApplicationContext());
            INSTANCE_REF.set(new WeakReference<>(manager));
        }

        return manager;
    }

    private AuthStateManager(Context context) {
        mPrefs = context.getSharedPreferences(SHARED_PREF_NAME, Context.MODE_PRIVATE);
        mPrefsLock = new ReentrantLock();
        mCurrentAuthState = new AtomicReference<>();
    }

    @AnyThread
    @NonNull
    public AuthState getCurrent() {
        if (mCurrentAuthState.get() != null) {
            return mCurrentAuthState.get();
        }

        AuthState state = readState();
        if (mCurrentAuthState.compareAndSet(null, state)) {
            return state;
        } else {
            return mCurrentAuthState.get();
        }
    }

    @AnyThread
    @NonNull
    public AuthState replace(@NonNull AuthState state) {
        writeState(state);
        mCurrentAuthState.set(state);
        return state;
    }

    @AnyThread
    @NonNull
    public AuthState updateAfterAuthorization(
            @Nullable AuthorizationResponse response,
            @Nullable AuthorizationException ex) {
        AuthState current = getCurrent();
        current.update(response, ex);
        return replace(current);
    }

    @AnyThread
    @NonNull
    public AuthState updateAfterTokenResponse(
            @Nullable TokenResponse response,
            @Nullable AuthorizationException ex) {
        AuthState current = getCurrent();
        current.update(response, ex);
        return replace(current);
    }

    @AnyThread
    @NonNull
    public AuthState updateAfterRegistration(
            RegistrationResponse response,
            AuthorizationException ex) {
        AuthState current = getCurrent();
        if (ex != null) {
            return current;
        }

        current.update(response);
        return replace(current);
    }

    @AnyThread
    @NonNull
    private AuthState readState() {
        mPrefsLock.lock();
        try {
            String currentState = mPrefs.getString(KEY_STATE, null);
            String ivLength = mPrefs.getString(KEY_IV_LENGTH, null);
            if (currentState == null || ivLength == null) {
                return new AuthState();
            }

            try {
                // Base64 to decode encrypted string, because it is stored with Base64
                byte[] encryptedAuthState = Base64.getDecoder().decode(currentState);
                SecretKey secretKey = getSecretKey();
                byte[] decryptedAuthState = decryptData(encryptedAuthState, secretKey, Integer.parseInt(ivLength));

                // UTF-8 to convert bytes to Json string, because Base64 doesn't support special characters in Json
                return AuthState.jsonDeserialize(new String(decryptedAuthState, StandardCharsets.UTF_8));
            } catch (Exception ex) {
                LOGGER.warn("Failed to deserialize stored auth state - discarding");
                return new AuthState();
            }
        } finally {
            mPrefsLock.unlock();
        }
    }

    @AnyThread
    private void writeState(@Nullable AuthState state) {
        mPrefsLock.lock();
        try {
            SharedPreferences.Editor editor = mPrefs.edit();
            if (state == null) {
                editor.remove(KEY_STATE);
                editor.remove(KEY_IV_LENGTH);
            } else {
                // UTF8 for json string, because Base64 doesn't support special characters in Json string
                byte[] data = state.jsonSerializeString().getBytes(StandardCharsets.UTF_8);

                SecretKey secretKey = getSecretKey();
                Pair<byte[], Integer> result = encryptData(data, secretKey);
                byte[] encryptedData = result.first;
                Integer ivLength = result.second;

                // Base64 encoder for encrypted string, because UTF8 can cause encrypted data corruption
                editor.putString(KEY_STATE, Base64.getEncoder().encodeToString(encryptedData));
                editor.putString(KEY_IV_LENGTH, ivLength.toString());
            }

            if (!editor.commit()) {
                throw new IllegalStateException("Failed to write state to shared prefs");
            }
        } finally {
            mPrefsLock.unlock();
        }
    }

    @AnyThread
    private SecretKey getSecretKey() {
        try {
            KeyStore keyStore = KeyStore.getInstance(KEY_STORE_NAME);
            keyStore.load(null);

            if (keyStore.containsAlias(KEY_ALIAS)) {
                KeyStore.SecretKeyEntry secretKeyEntry = (KeyStore.SecretKeyEntry) keyStore.getEntry(KEY_ALIAS, null);
                return secretKeyEntry.getSecretKey();
            }

            KeyGenerator keyGenerator = KeyGenerator.getInstance(KeyProperties.KEY_ALGORITHM_AES, KEY_STORE_NAME);
            KeyGenParameterSpec.Builder builder = new KeyGenParameterSpec.Builder(KEY_ALIAS, KeyProperties.PURPOSE_ENCRYPT | KeyProperties.PURPOSE_DECRYPT)
                    .setBlockModes(KeyProperties.BLOCK_MODE_CBC)
                    .setEncryptionPaddings(KeyProperties.ENCRYPTION_PADDING_PKCS7)
                    .setRandomizedEncryptionRequired(true);

            keyGenerator.init(builder.build());
            return keyGenerator.generateKey();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @AnyThread
    private Pair<byte[], Integer> encryptData(byte[] serializedData, SecretKey secretKey) {
        try {
            Cipher cipher = Cipher.getInstance(CIPHER_TRANSFORMATION);
            cipher.init(Cipher.ENCRYPT_MODE, secretKey);

            byte[] iv = cipher.getIV();
            byte[] encryptedData = cipher.doFinal(serializedData);

            byte[] encryptedDataWithIv = new byte[iv.length + encryptedData.length];
            System.arraycopy(iv, 0, encryptedDataWithIv, 0, iv.length);
            System.arraycopy(encryptedData, 0, encryptedDataWithIv, iv.length, encryptedData.length);

            return new Pair<>(encryptedDataWithIv, iv.length);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @AnyThread
    private byte[] decryptData(byte[] serializedData, SecretKey secretKey, Integer ivLength) {
        try {
            byte[] iv = Arrays.copyOfRange(serializedData, 0, ivLength);
            byte[] encryptedData = Arrays.copyOfRange(serializedData, ivLength, serializedData.length);

            Cipher cipher = Cipher.getInstance(CIPHER_TRANSFORMATION);
            cipher.init(Cipher.DECRYPT_MODE, secretKey, new IvParameterSpec(iv));

            return cipher.doFinal(encryptedData);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

    }
}
