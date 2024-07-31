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
     * 排序，默认最高优先级
     */
    private int order = Ordered.HIGHEST_PRECEDENCE;

    /**
     * 客户端注销地址
     */
    private String logoutPath = "/logout";

    /**
     * 客户端Filter容器名称
     */
    private String name = "clientContainer";

    /**
     * 存cookie中token名称
     */
    private String cookieName = "TOKEN";

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

    public String getCookieName() {
        return cookieName;
    }

    public void setCookieName(String cookieName) {
        this.cookieName = cookieName;
    }

    public String getLogoutPath() {
        return logoutPath;
    }

    public void setLogoutPath(String logoutPath) {
        this.logoutPath = logoutPath;
    }
}