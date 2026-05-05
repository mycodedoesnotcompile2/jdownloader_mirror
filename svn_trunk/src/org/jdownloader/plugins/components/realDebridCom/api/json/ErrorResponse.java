package org.jdownloader.plugins.components.realDebridCom.api.json;

import org.appwork.storage.Storable;

public class ErrorResponse implements Storable {
    public static final org.appwork.storage.TypeRef<ErrorResponse> TYPE = new org.appwork.storage.TypeRef<ErrorResponse>(ErrorResponse.class) {
                                                                        };
    String                                                         error;
    int                                                            error_code;

    public ErrorResponse(/* Storable */) {
    }

    public String getError() {
        return error;
    }

    public int getError_code() {
        return error_code;
    }

    public void setError(String error) {
        this.error = error;
    }

    public void setError_code(int error_code) {
        this.error_code = error_code;
    }
}