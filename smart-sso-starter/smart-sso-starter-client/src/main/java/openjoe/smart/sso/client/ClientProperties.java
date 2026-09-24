package openjoe.smart.sso.client;

import openjoe.smart.sso.client.constant.ClientConstant;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.core.Ordered;

@ConfigurationProperties("smart.sso")
public class ClientProperties {

    /**
     * 服务端地址
     */
    private String serverUrl;

    /**
     * 应用Id
     */
    private String clientId;

    /**
     * 应用密钥
     */
    private String clientSecret;

    /**
     * 拦截urls，默认拦截全路径
     */
    private String[] urlPatterns = {ClientConstant.URL_FUZZY_MATCH};

    /**
     * 忽略拦截urls
     */
    private String[] excludeUrls;

    /**
     * 过滤器排序，默认10
     */
    private int order = 10;

    /**
     * 客户端注销地址
     */
    private String logoutPath = "/logout";

    /**
     * 客户端Filter容器名称
     */
    private String name = "clientContainer";

    /**
     * 存放在cookie或者Header中的token名称前缀
     */
    private String tokenNamePrefix = "smart-sso-token-";

    /**
     * 是否前后端分离（默认false）
     */
    private Boolean h5Enabled = false;

    /**
     * 内嵌服务端模式：server-url 留空且本应用同时是 SSO 服务端时由程序自动判定，不通过配置指定
     */
    private boolean embeddedServer;

    /**
     * 内嵌服务端模式下，服务端之间 HTTP 调用使用的本机地址
     * （默认自动推导为 http(s)://127.0.0.1:{实际端口}{context-path}，可显式覆盖）
     */
    private String internalServerUrl;

    public String getServerUrl() {
        return serverUrl;
    }

    public void setServerUrl(String serverUrl) {
        this.serverUrl = serverUrl;
    }

    public String getClientId() {
        return clientId;
    }

    public void setClientId(String clientId) {
        this.clientId = clientId;
    }

    public String getClientSecret() {
        return clientSecret;
    }

    public void setClientSecret(String clientSecret) {
        this.clientSecret = clientSecret;
    }

    public String[] getUrlPatterns() {
        return urlPatterns;
    }

    public void setUrlPatterns(String[] urlPatterns) {
        this.urlPatterns = urlPatterns;
    }

    public String[] getExcludeUrls() {
        return excludeUrls;
    }

    public void setExcludeUrls(String[] excludeUrls) {
        this.excludeUrls = excludeUrls;
    }

    public int getOrder() {
        return order;
    }

    public void setOrder(int order) {
        this.order = order;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getTokenNamePrefix() {
        return tokenNamePrefix;
    }

    public void setTokenNamePrefix(String tokenNamePrefix) {
        this.tokenNamePrefix = tokenNamePrefix;
    }

    public String getLogoutPath() {
        return logoutPath;
    }

    public void setLogoutPath(String logoutPath) {
        this.logoutPath = logoutPath;
    }

    public Boolean getH5Enabled() {
        return h5Enabled;
    }

    public void setH5Enabled(Boolean h5Enabled) {
        this.h5Enabled = h5Enabled;
    }

    public boolean isEmbeddedServer() {
        return embeddedServer;
    }

    /**
     * 由 ClientServerAddressResolver 在启动时判定，不作为配置项对外暴露
     */
    void setEmbeddedServer(boolean embeddedServer) {
        this.embeddedServer = embeddedServer;
    }

    public String getInternalServerUrl() {
        return internalServerUrl;
    }

    public void setInternalServerUrl(String internalServerUrl) {
        this.internalServerUrl = internalServerUrl;
    }
}