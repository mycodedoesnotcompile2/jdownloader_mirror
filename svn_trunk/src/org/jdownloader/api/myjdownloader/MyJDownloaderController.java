package org.jdownloader.api.myjdownloader;

import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.zip.GZIPOutputStream;

import org.appwork.console.AbstractConsole;
import org.appwork.console.ConsoleDialog;
import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.remoteapi.RemoteAPIRequest;
import org.appwork.remoteapi.RemoteAPIResponse;
import org.appwork.shutdown.ExceptionShutdownRequest;
import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.shutdown.ShutdownVetoException;
import org.appwork.shutdown.ShutdownVetoListener;
import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.NullsafeAtomicReference;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.net.ChunkedOutputStream;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.HeaderCollection;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpserver.AbstractServerBasics;
import org.appwork.utils.net.httpserver.ContentSecurityPolicy;
import org.appwork.utils.net.httpserver.CorsHandler;
import org.appwork.utils.net.httpserver.HeaderValidationRules;
import org.appwork.utils.net.httpserver.ReferrerPolicy;
import org.appwork.utils.net.httpserver.ResponseSecurityHeaders;
import org.appwork.utils.net.httpserver.XContentTypeOptions;
import org.appwork.utils.net.httpserver.XFrameOptions;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.jdownloader.api.RemoteAPIConfig;
import org.jdownloader.api.myjdownloader.MyJDownloaderSettings.MyJDownloaderError;
import org.jdownloader.api.myjdownloader.api.MyJDownloaderAPI;
import org.jdownloader.api.myjdownloader.event.MyJDownloaderEvent;
import org.jdownloader.api.myjdownloader.event.MyJDownloaderEventSender;
import org.jdownloader.logging.LogController;
import org.jdownloader.myjdownloader.client.exceptions.MyJDownloaderException;
import org.jdownloader.myjdownloader.client.exceptions.UnconnectedException;
import org.jdownloader.myjdownloader.client.json.MyCaptchaChallenge;
import org.jdownloader.myjdownloader.client.json.MyCaptchaSolution;
import org.jdownloader.myjdownloader.client.json.MyCaptchaSolution.RESULT;
import org.jdownloader.settings.staticreferences.CFG_MYJD;
import org.jdownloader.translate._JDT;

public class MyJDownloaderController extends AbstractServerBasics implements ShutdownVetoListener, GenericConfigEventListener<Boolean> {
    private static final MyJDownloaderController INSTANCE = new MyJDownloaderController();

    public static MyJDownloaderController getInstance() {
        return INSTANCE;
    }

    private final NullsafeAtomicReference<MyJDownloaderConnectThread> thread = new NullsafeAtomicReference<MyJDownloaderConnectThread>(null);
    private final LogSource                                           logger;
    private final MyJDownloaderEventSender                            eventSender;

    public MyJDownloaderEventSender getEventSender() {
        return eventSender;
    }

    public boolean isConnected() {
        final MyJDownloaderConnectThread lThread = getConnectThread();
        return lThread != null && lThread.isAlive() && lThread.isConnected();
    }

    public boolean isActive() {
        final MyJDownloaderConnectThread lThread = getConnectThread();
        return lThread != null && lThread.isAlive();
    }

    public MyJDownloaderConnectionStatus getConnectionStatus() {
        final MyJDownloaderConnectThread lThread = getConnectThread();
        if (lThread != null) {
            return lThread.getConnectionStatus();
        } else {
            return MyJDownloaderConnectionStatus.UNCONNECTED;
        }
    }

    public long getRetryTimeStamp() {
        final MyJDownloaderConnectThread lThread = getConnectThread();
        if (lThread != null) {
            return lThread.getRetryTimeStamp();
        } else {
            return -1;
        }
    }

    public int getEstablishedConnections() {
        final MyJDownloaderConnectThread lThread = getConnectThread();
        if (lThread != null) {
            return lThread.getEstablishedConnections();
        } else {
            return 0;
        }
    }

    protected void stop() {
        final MyJDownloaderConnectThread lThread = thread.getAndSet(null);
        if (lThread != null) {
            ShutdownController.getInstance().removeShutdownVetoListener(this);
            new Thread("MyJDownloaderController:Stop:" + lThread) {
                {
                    setDaemon(true);
                }

                public void run() {
                    lThread.disconnect();
                };
            }.start();
        }
    }

    public final boolean isAlwaysConnectRequired() {
        final RemoteAPIConfig remoteAPIConfig = JsonConfig.create(RemoteAPIConfig.class);
        return Application.isHeadless() && (remoteAPIConfig.isHeadlessMyJDownloaderMandatory() || !remoteAPIConfig.isDeprecatedApiEnabled());
    }

    @Deprecated
    public static OutputStream getOutputStream(final RemoteAPIResponse response, final RemoteAPIRequest request, final boolean gzip, final boolean wrapJQuery) throws IOException {
        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_REQUEST_CACHE_CONTROL, "no-store, no-cache"));
        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "application/json"));
        // HttpLimitProviderInputStream server = HttpConnection.getInputStreamByType(request.getInputStream(),
        // HttpLimitProviderInputStream.class);
        // final HTTPBridge bridge = response.getRemoteAPI().getHTTPBridge(request, response);
        final boolean chunked;
        if (request.getHttpRequest().getServer().isChunkedEncodedResponseAllowed(request.getHttpRequest(), response.getHttpResponse())) {
            chunked = true;
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING, HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING_CHUNKED));
        } else {
            chunked = false;
        }
        if (gzip) {
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_ENCODING, "gzip"));
        }
        response.setResponseCode(ResponseCode.SUCCESS_OK);
        final OutputStream os;
        if (chunked) {
            os = new ChunkedOutputStream(response.getOutputStream(true));
        } else {
            os = response.getOutputStream(true);
        }
        final OutputStream uos;
        final GZIPOutputStream out;
        if (gzip) {
            uos = out = new GZIPOutputStream(os);
        } else {
            out = null;
            uos = os;
        }
        return new OutputStream() {
            boolean wrapperHeader = wrapJQuery && request != null && request.getJqueryCallback() != null;
            boolean wrapperEnd    = wrapJQuery && request != null && request.getJqueryCallback() != null;

            @Override
            public void close() throws IOException {
                this.wrapperEnd();
                if (out != null) {
                    out.finish();
                    out.flush();
                }
                uos.close();
            }

            @Override
            public void flush() throws IOException {
                uos.flush();
            }

            private void wrapperEnd() throws UnsupportedEncodingException, IOException {
                if (this.wrapperEnd) {
                    uos.write(")".getBytes("UTF-8"));
                    this.wrapperEnd = false;
                }
            }

            private void wrapperHeader() throws UnsupportedEncodingException, IOException {
                if (this.wrapperHeader) {
                    uos.write(request.getJqueryCallback().getBytes("UTF-8"));
                    uos.write("(".getBytes("UTF-8"));
                    this.wrapperHeader = false;
                }
            }

            @Override
            public void write(final byte[] b) throws IOException {
                this.wrapperHeader();
                uos.write(b);
            }

            @Override
            public void write(final int b) throws IOException {
                this.wrapperHeader();
                uos.write(b);
            }
        };
    }

    private MyJDownloaderController() {
        super("my.jdownloader");
        logger = LogController.getInstance().getLogger(MyJDownloaderController.class.getName());
        eventSender = new MyJDownloaderEventSender();
        configureBasics();
        if (isAlwaysConnectRequired() || CFG_MYJD.AUTO_CONNECT_ENABLED.isEnabled()) {
            start();
        }
        CFG_MYJD.AUTO_CONNECT_ENABLED.getEventSender().addListener(this);
    }

    protected void configureBasics() {
        setAllowedMethods(RequestMethod.GET, RequestMethod.POST, RequestMethod.HEAD, RequestMethod.OPTIONS);
        HashMap<String, String> mandatory = new HashMap<String, String>();
        HashMap<String, String> forbidden = new HashMap<String, String>();
        HeaderValidationRules header = new HeaderValidationRules(mandatory, forbidden);
        setHeaderValidationRules(header);
        CorsHandler cors = new CorsHandler() {
            {
                setMaxAge(TimeUnit.MINUTES.toMillis(30));
                setAllowMethods(RequestMethod.GET, RequestMethod.POST, RequestMethod.OPTIONS);
                setAllowHeadersFromRequest(true);
            }

            @Override
            protected String findAllowedOrigin(String requestOrigin, HeaderCollection requestHeaders, HeaderCollection responseHeaders) {
                if (requestOrigin != null) {
                    return requestOrigin;
                } else if (StringUtils.containsIgnoreCase(requestHeaders.getValue(HTTPConstants.HEADER_REQUEST_HOST), "my.jdownloader.org")) {
                    return "https://my.jdownloader.org";
                } else {
                    return "*";
                }
            }
        };
        setCorsHandler(cors);
        final RemoteAPIConfig cfg = JsonConfig.create(RemoteAPIConfig.class);
        ResponseSecurityHeaders securityHeaders = new ResponseSecurityHeaders() {
            @Override
            public ContentSecurityPolicy getContentSecurityPolicy() {
                if (StringUtils.isEmpty(cfg.getLocalAPIServerHeaderContentSecurityPolicy())) {
                    return null;
                }
                try {
                    ContentSecurityPolicy ret = ContentSecurityPolicy.fromHeaderString(cfg.getLocalAPIServerHeaderContentSecurityPolicy());
                    return ret;
                } catch (Exception e) {
                    LogV3.log(e);
                    return new ContentSecurityPolicy() {
                        public String toHeaderString() {
                            return cfg.getLocalAPIServerHeaderContentSecurityPolicy();
                        };
                    };
                }
            }

            @Override
            public ReferrerPolicy getReferrerPolicy() {
                String value = cfg.getLocalAPIServerHeaderReferrerPolicy();
                return ReferrerPolicy.get(value);
            }

            @Override
            public XContentTypeOptions getXContentTypeOptions() {
                return super.getXContentTypeOptions();
            }

            @Override
            public XFrameOptions getXFrameOptions() {
                return XFrameOptions.DENY;
            }
        };
        setResponseSecurityHeaders(securityHeaders);
    }

    protected void start() {
        stop();
        String email = CFG_MYJD.CFG.getEmail();
        String password = CFG_MYJD.CFG.getPassword();
        boolean validFlag = validateAndVerifyLogins(email, password, isAlwaysConnectRequired());
        if (!validFlag && isAlwaysConnectRequired()) {
            while (true) {
                try {
                    if (askLoginsOnConsole(true)) {
                        email = CFG_MYJD.CFG.getEmail();
                        password = CFG_MYJD.CFG.getPassword();
                        validFlag = true;
                        break;
                    }
                } catch (DialogNoAnswerException e) {
                    ShutdownController.getInstance().requestShutdown(new ExceptionShutdownRequest(e, false, false));
                }
            }
        }
        if (validFlag || validateAndVerifyLogins(email, password, false)) {
            final MyJDownloaderConnectThread lthread = new MyJDownloaderConnectThread(this);
            lthread.setEmail(email);
            lthread.setPassword(password);
            lthread.setDeviceName(CFG_MYJD.CFG.getDeviceName());
            if (thread.compareAndSet(null, lthread)) {
                ShutdownController.getInstance().addShutdownVetoListener(this);
                lthread.start();
            }
        }
    }

    public String getCurrentDeviceName() {
        final MyJDownloaderConnectThread th = getConnectThread();
        if (th == null || !th.isAlive() || !th.isConnected()) {
            return null;
        } else {
            return th.getDeviceName();
        }
    }

    public String getCurrentEmail() {
        final MyJDownloaderConnectThread th = getConnectThread();
        if (th == null || !th.isAlive() || !th.isConnected()) {
            return null;
        } else {
            return th.getEmail();
        }
    }

    public String getCurrentPassword() {
        final MyJDownloaderConnectThread th = getConnectThread();
        if (th == null || !th.isAlive() || !th.isConnected()) {
            return null;
        } else {
            return th.getPassword();
        }
    }

    public MyJDownloaderConnectThread getConnectThread() {
        return thread.get();
    }

    @Override
    public void onShutdown(ShutdownRequest request) {
        try {
            stop();
        } catch (final Throwable e) {
        }
    }

    @Override
    public void onShutdownVeto(ShutdownRequest request) {
    }

    @Override
    public void onShutdownVetoRequest(ShutdownRequest request) throws ShutdownVetoException {
    }

    @Override
    public long getShutdownVetoPriority() {
        return 0;
    }

    public LogSource getLogger() {
        return logger;
    }

    @Override
    public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
    }

    @Override
    public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
        if (isAlwaysConnectRequired() || CFG_MYJD.AUTO_CONNECT_ENABLED.isEnabled()) {
            start();
        }
    }

    public void onError(MyJDownloaderError error) {
        if (error == null) {
            error = MyJDownloaderError.NONE;
        }
        CFG_MYJD.CFG.setLatestError(error);
        switch (error) {
        case ACCOUNT_UNCONFIRMED:
            stop();
            if (isAlwaysConnectRequired()) {
                synchronized (AbstractConsole.LOCK) {
                    final ConsoleDialog cd = new ConsoleDialog("MyJDownloader");
                    cd.start();
                    try {
                        cd.printLines(_JDT.T.MyJDownloaderController_onError_account_unconfirmed());
                        cd.waitToContinue();
                        ShutdownController.getInstance().requestShutdown();
                    } finally {
                        cd.end();
                    }
                }
            } else {
                UIOManager.I().showConfirmDialog(UIOManager.BUTTONS_HIDE_CANCEL, "MyJDownloader", _JDT.T.MyJDownloaderController_onError_account_unconfirmed());
            }
            break;
        case OUTDATED:
            stop();
            if (isAlwaysConnectRequired()) {
                synchronized (AbstractConsole.LOCK) {
                    final ConsoleDialog cd = new ConsoleDialog("MyJDownloader");
                    cd.start();
                    try {
                        cd.printLines(_JDT.T.MyJDownloaderController_onError_outdated());
                        cd.waitToContinue();
                        ShutdownController.getInstance().requestShutdown();
                    } finally {
                        cd.end();
                    }
                }
            } else {
                UIOManager.I().showConfirmDialog(UIOManager.BUTTONS_HIDE_CANCEL, "MyJDownloader", _JDT.T.MyJDownloaderController_onError_outdated());
            }
            break;
        case EMAIL_INVALID:
        case BAD_LOGINS:
            stop();
            if (isAlwaysConnectRequired()) {
                start();
            } else {
                UIOManager.I().showConfirmDialog(UIOManager.BUTTONS_HIDE_CANCEL, "MyJDownloader", _JDT.T.MyJDownloaderController_onError_badlogins());
            }
            break;
        default:
            break;
        }
    }

    public boolean askLoginsOnConsole(final boolean verifyLogins) throws DialogNoAnswerException {
        synchronized (AbstractConsole.LOCK) {
            final ConsoleDialog cd = new ConsoleDialog("MyJDownloader");
            cd.start();
            try {
                cd.printLines(_JDT.T.MyJDownloaderController_onError_badlogins());
                cd.waitYesOrNo(0, "Enter Logins", "Exit JDownloader");
                final String email = cd.ask("Please Enter your MyJDownloader Email:");
                if (new Regex(email, "..*?@.*?\\..+").matches()) {
                    final String password = cd.askHidden("Please Enter your MyJDownloader Password(not visible):");
                    if (validateAndVerifyLogins(email, password, verifyLogins)) {
                        CFG_MYJD.EMAIL.setValue(email.trim());
                        CFG_MYJD.PASSWORD.setValue(password);
                        return true;
                    }
                }
                cd.println("Invalid Logins");
                return false;
            } finally {
                cd.end();
            }
        }
    }

    public void fireConnectionStatusChanged(MyJDownloaderConnectionStatus status, int connections) {
        switch (status) {
        case CONNECTED:
        case PENDING:
            CFG_MYJD.CFG.setLatestError(MyJDownloaderError.NONE);
            break;
        default:
            break;
        }
        eventSender.fireEvent(new MyJDownloaderEvent(this, MyJDownloaderEvent.Type.CONNECTION_STATUS_UPDATE, status, connections));
    }

    public void disconnect() {
        stop();
    }

    public void connect() {
        start();
    }

    public boolean validateAndVerifyLogins(final String email, final String password, final boolean verifyLogins) {
        if (StringUtils.isEmpty(password) || StringUtils.isEmpty(email) || !new Regex(email, "..*?@.*?\\..+").matches()) {
            return false;
        } else {
            if (!verifyLogins) {
                return true;
            } else {
                final AtomicBoolean validFlag = new AtomicBoolean(false);
                final MyJDownloaderAPI api = new MyJDownloaderAPI() {
                    public LogSource getLogger() {
                        return MyJDownloaderController.this.getLogger();
                    };
                };
                final Thread validateThread = new Thread("validateLogins") {
                    @Override
                    public void run() {
                        try {
                            api.connect(email, password);
                            validFlag.set(true);
                            api.disconnect();
                        } catch (Exception e) {
                            api.getLogger().log(e);
                        }
                    }
                };
                validateThread.setDaemon(true);
                validateThread.start();
                try {
                    validateThread.join();
                } catch (InterruptedException e) {
                    api.getLogger().log(e);
                }
                return validFlag.get();
            }
        }
    }

    public boolean isLoginValid(final boolean verifyLogin) {
        return validateAndVerifyLogins(CFG_MYJD.CFG.getEmail(), CFG_MYJD.CFG.getPassword(), verifyLogin);
    }

    /**
     * Call this method to send a push request
     *
     * @param captchasPending
     */
    public void pushCaptchaFlag(boolean captchasPending) {
        if (isConnected()) {
            final MyJDownloaderConnectThread th = getConnectThread();
            if (th != null) {
                th.pushCaptchaNotification(captchasPending);
            }
        }
    }

    public MyCaptchaSolution pushChallenge(MyCaptchaChallenge ch) throws MyJDownloaderException {
        final MyJDownloaderConnectThread th = getConnectThread();
        if (th == null) {
            throw new UnconnectedException();
        } else {
            if (th.isAlive()) {
                switch (th.getConnectionStatus()) {
                case CONNECTED:
                case PENDING:
                    return th.pushChallenge(ch);
                default:
                    return null;
                }
            } else {
                return null;
            }
        }
    }

    public boolean isRemoteCaptchaServiceEnabled() {
        final MyJDownloaderConnectThread th = getConnectThread();
        return th != null && th.isChallengeExchangeEnabled();
    }

    public MyCaptchaSolution getChallengeResponse(String id) throws MyJDownloaderException {
        final MyJDownloaderConnectThread th = getConnectThread();
        if (th == null) {
            throw new UnconnectedException();
        } else {
            if (th.isAlive()) {
                switch (th.getConnectionStatus()) {
                case CONNECTED:
                case PENDING:
                    return th.getChallengeResponse(id);
                default:
                    return null;
                }
            } else {
                return null;
            }
        }
    }

    public boolean isSessionValid(final String sessionToken) {
        final MyJDownloaderConnectThread ct = getConnectThread();
        return ct != null && ct.isSessionValid(sessionToken);
    }

    public boolean sendChallengeFeedback(String id, RESULT correct) throws MyJDownloaderException {
        final MyJDownloaderConnectThread th = getConnectThread();
        if (th == null) {
            throw new UnconnectedException();
        } else {
            if (th.isAlive()) {
                switch (th.getConnectionStatus()) {
                case CONNECTED:
                case PENDING:
                    return th.sendChallengeFeedback(id, correct);
                default:
                    return false;
                }
            } else {
                return false;
            }
        }
    }

    public void terminateSession(String connectToken) throws MyJDownloaderException {
        final MyJDownloaderConnectThread ct = getConnectThread();
        if (ct != null) {
            ct.terminateSession(connectToken);
        }
    }

    @Override
    public List<HttpRequestHandler> getHandler() {
        return null;
    }

    @Override
    public boolean isChunkedEncodedResponseAllowed(HttpRequest httpRequest, HttpResponse httpResponse) {
        return true;
    }
}
