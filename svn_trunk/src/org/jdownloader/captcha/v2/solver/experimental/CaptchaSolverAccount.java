package org.jdownloader.captcha.v2.solver.experimental;

/** This is a mockup for a possible captcha solver account class to be used in the future. */
public class CaptchaSolverAccount {
    /**
     * Enum representing different currencies.
     */
    public enum CURRENCY {
        EUR,
        USD
    }

    /**
     * Enum representing different error states for a captcha solver account.
     */
    public enum CaptchaSolverAccountError {
        TEMP_DISABLED,
        INVALID,
        PLUGIN_ERROR,
        ZERO_BALANCE
    }

    private String                    hoster;
    private boolean                   enabled;
    private String                    username;
    private String                    password;
    private String                    apikey;
    private double                    balance;
    private CURRENCY                  currency;
    private CaptchaSolverAccountError accountError;
    private transient volatile long   tmpDisabledTimeout = -1;
    private String                    errorString;

    /**
     * Default constructor.
     */
    public CaptchaSolverAccount() {
        this.enabled = false;
        this.balance = 0.0;
        this.currency = CURRENCY.USD;
    }

    /**
     * Gets the hoster domain.
     *
     * @return The hoster domain (e.g., "2captcha.com")
     */
    public String getHoster() {
        return hoster;
    }

    /**
     * Sets the hoster domain.
     *
     * @param hoster
     *            The hoster domain to set
     */
    public void setHoster(String hoster) {
        this.hoster = hoster;
    }

    /**
     * Checks if the account is enabled.
     *
     * @return true if the account is enabled, false otherwise
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * Sets whether the account is enabled.
     *
     * @param enabled
     *            The enabled state to set
     */
    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * Gets the username.
     *
     * @return The username
     */
    public String getUsername() {
        return username;
    }

    /**
     * Sets the username.
     *
     * @param username
     *            The username to set
     */
    public void setUsername(String username) {
        this.username = username;
    }

    /**
     * Gets the password.
     *
     * @return The password
     */
    public String getPassword() {
        return password;
    }

    /**
     * Sets the password.
     *
     * @param password
     *            The password to set
     */
    public void setPassword(String password) {
        this.password = password;
    }

    /**
     * Gets the API key.
     *
     * @return The API key
     */
    public String getApikey() {
        return apikey;
    }

    /**
     * Sets the API key.
     *
     * @param apikey
     *            The API key to set
     */
    public void setApikey(String apikey) {
        this.apikey = apikey;
    }

    /**
     * Gets the account balance.
     *
     * @return The account balance
     */
    public double getBalance() {
        return balance;
    }

    /**
     * Sets the account balance.
     *
     * @param balance
     *            The account balance to set
     */
    public void setBalance(double balance) {
        this.balance = balance;
    }

    /**
     * Sets both the account balance and currency.
     *
     * @param balance
     *            The account balance to set
     * @param currency
     *            The currency to set
     */
    public void setBalance(double balance, CURRENCY currency) {
        this.balance = balance;
        this.currency = currency;
    }

    /**
     * Gets the currency.
     *
     * @return The currency
     */
    public CURRENCY getCurrency() {
        return currency;
    }

    /**
     * Sets the currency.
     *
     * @param currency
     *            The currency to set
     */
    public void setCurrency(CURRENCY currency) {
        this.currency = currency;
    }

    /**
     * Gets the account error.
     *
     * @return The account error
     */
    public CaptchaSolverAccountError getAccountError() {
        return accountError;
    }

    /**
     * Sets the account error.
     *
     * @param accountError
     *            The account error to set
     */
    public void setAccountError(CaptchaSolverAccountError accountError) {
        this.accountError = accountError;
    }

    /**
     * Gets the temporary disabled timeout.
     *
     * @return The temporary disabled timeout
     */
    public long getTmpDisabledTimeout() {
        return tmpDisabledTimeout;
    }

    /**
     * Sets the temporary disabled timeout.
     *
     * @param tmpDisabledTimeout
     *            The temporary disabled timeout to set
     */
    public void setTmpDisabledTimeout(long tmpDisabledTimeout) {
        this.tmpDisabledTimeout = tmpDisabledTimeout;
    }

    /**
     * Gets the error string.
     *
     * @return The error string
     */
    public String getErrorString() {
        return errorString;
    }

    /**
     * Sets the error string.
     *
     * @param errorString
     *            The error string to set
     */
    public void setErrorString(String errorString) {
        this.errorString = errorString;
    }

    /**
     * Sets the account error with timeout and error message.
     *
     * @param error
     *            The AccountError to set
     * @param timeout
     *            The timeout value in milliseconds
     * @param errorString
     *            The error message
     */
    public void setError(final CaptchaSolverAccountError error, final long timeout, String errorString) {
        this.accountError = error;
        this.tmpDisabledTimeout = timeout;
        this.errorString = errorString;
    }
}