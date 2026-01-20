package org.jdownloader.api.myjdownloader;

import java.io.IOException;

import org.appwork.utils.net.httpserver.HttpConnectionExceptionHandler;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;

public class InvalidMyJDownloaderRequest extends IOException implements HttpConnectionExceptionHandler {
    @Override
    @Deprecated
    public boolean handle(HttpResponse response) throws IOException {
        return handle(null, response);
    }

    @Override
    public boolean handle(HttpRequest request, HttpResponse response) throws IOException {
        return true;
    }
}
