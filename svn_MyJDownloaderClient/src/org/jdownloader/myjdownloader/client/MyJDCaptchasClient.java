/**
 * 
 * ====================================================================================================================================================
 *         "My JDownloader Client" License
 *         The "My JDownloader Client" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany   
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 * 	
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header. 	
 * 	
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the 
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 * 	
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.jdownloader.myjdownloader.client;

import java.util.List;

import org.jdownloader.myjdownloader.client.exceptions.MyJDownloaderException;
import org.jdownloader.myjdownloader.client.json.JSonRequest;
import org.jdownloader.myjdownloader.client.json.MyCaptchaChallenge;
import org.jdownloader.myjdownloader.client.json.MyCaptchaSolution;
import org.jdownloader.myjdownloader.client.json.MyCaptchaSolutionsListResponse;
import org.jdownloader.myjdownloader.client.json.SuccessfulResponse;

public class MyJDCaptchasClient<GenericType> {
    
    private final AbstractMyJDClient<GenericType> api;
    
    public MyJDCaptchasClient(final AbstractMyJDClient<GenericType> abstractMyJDClient) {
        this.api = abstractMyJDClient;
    }
    
    public boolean abort(final String ID) throws MyJDownloaderException {
        return this.remove(ID, MyCaptchaSolution.RESULT.ABORT);
    }
    
    public MyCaptchaSolution get(final String ID) throws MyJDownloaderException {
        final List<MyCaptchaSolution> ret = this.get(new String[] { ID });
        if (ret != null && ret.size() == 1) { return ret.get(0); }
        return null;
    }
    
    public List<MyCaptchaSolution> get(final String IDs[]) throws MyJDownloaderException {
        final SessionInfo sessionInfo = this.api.getSessionInfo();
        final String url = "/my/captchas/get?sessiontoken=" + this.api.urlencode(sessionInfo.getSessionToken());
        final JSonRequest re = new JSonRequest();
        re.setParams(new Object[] { IDs });
        re.setUrl(url);
        final List<MyCaptchaSolution> list = this.api.callServer(url, re, sessionInfo, MyCaptchaSolutionsListResponse.class).getList();
        return list;
    }
    
    public boolean invalidate(final String ID) throws MyJDownloaderException {
        return this.remove(ID, MyCaptchaSolution.RESULT.WRONG);
    }
    
    public List<MyCaptchaSolution> list() throws MyJDownloaderException {
        return this.get((String[]) null);
    }
    
    public boolean remove(final String ID, final MyCaptchaSolution.RESULT result) throws MyJDownloaderException {
        final SessionInfo sessionInfo = this.api.getSessionInfo();
        final String url = "/my/captchas/remove?sessiontoken=" + this.api.urlencode(sessionInfo.getSessionToken());
        final JSonRequest re = new JSonRequest();
        re.setParams(new Object[] { ID, result });
        re.setUrl(url);
        return this.api.callServer(url, re, sessionInfo, SuccessfulResponse.class).isSuccessful();
    }
    
    public boolean isEnabled() throws MyJDownloaderException {
        final SessionInfo sessionInfo = this.api.getSessionInfo();
        final String url = "/my/captchas/isEnabled?sessiontoken=" + this.api.urlencode(sessionInfo.getSessionToken());
        final JSonRequest re = new JSonRequest();
        re.setUrl(url);
        return this.api.callServer(url, re, sessionInfo, SuccessfulResponse.class).isSuccessful();
    }
    
    public MyCaptchaSolution solve(final MyCaptchaChallenge myCaptchaChallenge) throws MyJDownloaderException {
        final SessionInfo sessionInfo = this.api.getSessionInfo();
        final String url = "/my/captchas/solve?sessiontoken=" + this.api.urlencode(sessionInfo.getSessionToken());
        final JSonRequest re = new JSonRequest();
        re.setParams(new Object[] { myCaptchaChallenge });
        re.setUrl(url);
        final List<MyCaptchaSolution> list = this.api.callServer(url, re, sessionInfo, MyCaptchaSolutionsListResponse.class).getList();
        if (list != null && list.size() == 1) { return list.get(0); }
        return null;
    }
    
    public boolean timeout(final String ID) throws MyJDownloaderException {
        return this.remove(ID, MyCaptchaSolution.RESULT.TIMEOUT);
    }
    
    public boolean validate(final String ID) throws MyJDownloaderException {
        return this.remove(ID, MyCaptchaSolution.RESULT.CORRECT);
    }
    
}
