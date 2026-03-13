function FindProxyForURL(url, host) {

    // HTTP proxy for testing: request to httpproxy.appwork.org returns HTTP proxy
    if (shExpMatch(host, "httpproxy.appwork.org") || shExpMatch(host, "*.httpproxy.appwork.org"))
        return "PROXY localhost:3128";

    // use SOCKS for specific domains
    if (shExpMatch(host, "*.avlditest.lan"))
        return "SOCKS localhost:9999";
    if (shExpMatch(host, "*.wackerneusongroup-services.com"))
        return "SOCKS localhost:9999";

    // by default use SOCKS
    return "SOCKS localhost:9999";
}