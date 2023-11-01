package jd.http;

import java.util.concurrent.atomic.AtomicLong;

import org.appwork.exceptions.WTFException;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.Hash;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;

public class DigestAuthentication extends Authentication {
    // https://www.greenbytes.de/tech/webdav/rfc7616.html#rfc.section.3.9.1
    // https://tools.ietf.org/html/rfc2617
    // https://tools.ietf.org/html/rfc7615
    protected final String nonce;

    public String getNonce() {
        return this.nonce;
    }

    public String getAlgorithm() {
        return this.algorithm;
    }

    public String getQop() {
        return this.qop;
    }

    protected final String algorithm;
    protected final String qop;
    protected final String cnonce = Hash.getMD5(Long.toString(System.nanoTime()));
    protected final String opaque;

    public String getOpaque() {
        return this.opaque;
    }

    public String getCnonce() {
        return this.cnonce;
    }

    public long getNc() {
        return this.nc.get();
    }

    protected final AtomicLong nc = new AtomicLong(1);

    public DigestAuthentication(String host, String username, String password, String realm, final String nonce, final String algorithm, final String qop, final String opaque) {
        super(false, host, username, password, realm);
        this.nonce = nonce;
        this.qop = qop;
        this.algorithm = algorithm;
        this.opaque = opaque;
    }

    protected String getNextNc() {
        final String ret = Long.toHexString(this.nc.incrementAndGet());
        if (ret.length() < 8) {
            return String.format("%0" + (8 - ret.length()) + "d", 0) + ret;
        } else {
            return ret;
        }
    }

    protected String hashWithAlgorithm(final String value) {
        if (StringUtils.equalsIgnoreCase(this.getAlgorithm(), "MD5")) {
            return Hash.getMD5(value);
        } else if (StringUtils.equalsIgnoreCase(this.getAlgorithm(), "SHA-256")) {
            return Hash.getSHA256(value);
        } else {
            throw new WTFException("Unsupported algorithm:" + this.getAlgorithm());
        }
    }

    public static DigestAuthentication build(Browser browser, Request request, final String realm, final String username, final String password) {
        if (StringUtils.isNotEmpty(username) || StringUtils.isNotEmpty(password)) {
            final String wwwAuthenticate = request.getResponseHeader(HTTPConstants.HEADER_RESPONSE_WWW_AUTHENTICATE);
            final String qop = new Regex(wwwAuthenticate, "qop\\s*=\\s*\"(.*?)\"").getMatch(0);
            if (StringUtils.equalsIgnoreCase(qop, "auth")) {
                final String nonce = new Regex(wwwAuthenticate, "nonce\\s*=\\s*\"(.*?)\"").getMatch(0);
                String algorithm = new Regex(wwwAuthenticate, "algorithm\\s*=\\s*(MD5|SHA-256)").getMatch(0);
                if (algorithm == null) {
                    algorithm = new Regex(wwwAuthenticate, "algorithm\\s*=\\s*\"(MD5|SHA-256)\"").getMatch(0);
                }
                if (nonce != null && algorithm != null) {
                    final String opaque = new Regex(wwwAuthenticate, "opaque\\s*=\\s*\"(.*?)\"").getMatch(0);
                    return new DigestAuthentication(request.getURL().getHost(), username, password, realm, nonce, algorithm, qop, opaque);
                }
            }
        }
        return null;
    }

    @Override
    public boolean authorize(Browser browser, Request request) {
        if (StringUtils.endsWithCaseInsensitive(request.getURL().getHost(), this.getHost()) && !this.isProxyAuthentication()) {
            final String HA1 = this.hashWithAlgorithm(StringUtils.valueOrEmpty(this.getUsername()) + ":" + StringUtils.valueOrEmpty(this.getRealm()) + ":" + StringUtils.valueOrEmpty(this.getPassword()));
            final String HA2 = this.hashWithAlgorithm(request.getRequestMethod() + ":" + request.getURL().getPath());
            final String nextNc = this.getNextNc();
            final String response = this.hashWithAlgorithm(HA1 + ":" + this.getNonce() + ":" + nextNc + ":" + this.getCnonce() + ":" + this.getQop() + ":" + HA2);
            String auth = "username=\"" + StringUtils.valueOrEmpty(this.getUsername()) + "\", realm=\"" + StringUtils.valueOrEmpty(this.getRealm()) + "\", nonce=\"" + this.getNonce() + "\", uri=\"" + request.getURL().getPath() + "\", algorithm=" + this.getAlgorithm() + ", response=\"" + response + "\", qop=\"" + this.getQop() + "\", nc=" + nextNc + ", cnonce=\"" + this.getCnonce() + "\"";
            if (this.getOpaque() != null) {
                auth += ", opaque=\"" + this.getOpaque() + "\"";
            }
            request.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Digest " + auth);
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean retry(Browser browser, Request request) {
        // TODO: add renegotiation support
        return false;
    }
}
