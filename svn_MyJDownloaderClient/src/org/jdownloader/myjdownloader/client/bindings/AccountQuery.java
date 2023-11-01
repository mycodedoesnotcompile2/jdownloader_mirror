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
package org.jdownloader.myjdownloader.client.bindings;

import java.util.HashSet;

import org.jdownloader.myjdownloader.client.json.AbstractJsonData;

public class AccountQuery extends AbstractJsonData {

	public AccountQuery(/* storable */) {

	}

	/**
	 * @param startAt
	 * @param maxResults
	 * @param userName
	 * @param validUntil
	 * @param trafficLeft
	 * @param trafficMax
	 * @param enabled
	 * @param valid
	 */
	public AccountQuery(final int startAt, final int maxResults,
			final boolean userName, final boolean validUntil,
			final boolean trafficLeft, final boolean trafficMax,
			final boolean enabled, final boolean valid) {
		super();
		this.startAt = startAt;
		this.maxResults = maxResults;
		this.userName = userName;
		this.validUntil = validUntil;
		this.trafficLeft = trafficLeft;
		this.trafficMax = trafficMax;
		this.enabled = enabled;
		this.valid = valid;
	}

	private boolean userName = false;
	private boolean validUntil = false;
	private boolean trafficLeft = false;
	private boolean error = false;
	private boolean trafficMax = false;
	/**
	 * only return these ids. if null all ids will be returned
	 */
	private HashSet<Long> UUIDList = null;

	public HashSet<Long> getUUIDList() {
		return UUIDList;
	}

	public void setUUIDList(final HashSet<Long> ids) {
		this.UUIDList = ids;
	}

	public boolean isUserName() {
		return userName;
	}

	public void setUserName(final boolean userName) {
		this.userName = userName;
	}

	public boolean isValidUntil() {
		return validUntil;
	}

	public void setValidUntil(final boolean validUntil) {
		this.validUntil = validUntil;
	}

	public boolean isTrafficLeft() {
		return trafficLeft;
	}

	public void setTrafficLeft(final boolean trafficLeft) {
		this.trafficLeft = trafficLeft;
	}

	public boolean isTrafficMax() {
		return trafficMax;
	}

	public void setTrafficMax(final boolean trafficMax) {
		this.trafficMax = trafficMax;
	}

	private boolean enabled = false;

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(final boolean enabled) {
		this.enabled = enabled;
	}

	public boolean isValid() {
		return valid;
	}

	public void setValid(final boolean valid) {
		this.valid = valid;
	}

	private boolean valid = false;

	private int startAt = 0;

	public int getStartAt() {
		return startAt;
	}

	public void setStartAt(final int startAt) {
		this.startAt = startAt;
	}

	private int maxResults = -1;

	public void setMaxResults(final int maxResults) {
		this.maxResults = maxResults;
	}

	public int getMaxResults() {
		return maxResults;
	}

	public boolean isError() {
		return error;
	}

	public void setError(final boolean error) {
		this.error = error;
	}

}