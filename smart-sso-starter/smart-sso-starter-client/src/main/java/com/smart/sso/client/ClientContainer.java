package com.smart.sso.client;

import com.smart.sso.client.constant.ClientConstant;
import com.smart.sso.client.filter.AbstractClientFilter;
import com.smart.sso.client.token.TokenStorage;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * smart-sso容器中心
 *
 * @author Joe
 */
public class ClientContainer implements Filter {

    private TokenStorage tokenStorage;

    private ClientProperties properties;

    private AbstractClientFilter[] filters;

    public ClientContainer(ClientProperties properties, TokenStorage tokenStorage) {
        this.properties = properties;
        this.tokenStorage = tokenStorage;
    }

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
        if (filters == null || filters.length == 0) {
            throw new IllegalArgumentException("filters不能为空");
        }
        for (AbstractClientFilter filter : filters) {
            filter.setProperties(properties);
            filter.setTokenStorage(tokenStorage);
            filter.init(filterConfig);
        }
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {
        HttpServletRequest httpRequest = (HttpServletRequest) request;
        if (isExcludeUrl(httpRequest.getServletPath())) {
            chain.doFilter(request, response);
            return;
        }

        HttpServletResponse httpResponse = (HttpServletResponse) response;
        for (AbstractClientFilter filter : filters) {
            if (!filter.isAccessAllowed(httpRequest, httpResponse)) {
                return;
            }
        }
        chain.doFilter(request, response);
    }

    private boolean isExcludeUrl(String url) {
        if (properties.getExcludeUrls() == null || properties.getExcludeUrls().length == 0) {
            return false;
        }

        Map<Boolean, List<String>> map = Arrays.stream(properties.getExcludeUrls())
                .collect(Collectors.partitioningBy(u -> u.endsWith(ClientConstant.URL_FUZZY_MATCH)));
        List<String> urlList = map.get(false);
        // 优先精确匹配
        if (urlList.contains(url)) {
            return true;
        }
        urlList = map.get(true);
        // 再进行模糊匹配
        for (String matchUrl : urlList) {
            if (url.startsWith(matchUrl.replace(ClientConstant.URL_FUZZY_MATCH, ""))) {
                return true;
            }
        }
        return false;
    }

    @Override
    public void destroy() {
        if (filters == null || filters.length == 0) {
            return;
        }
        for (AbstractClientFilter filter : filters) {
            filter.destroy();
        }
    }

    public void setFilters(AbstractClientFilter... filters) {
        this.filters = filters;
    }
}