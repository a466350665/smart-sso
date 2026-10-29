package openjoe.smart.sso.client;

import openjoe.smart.sso.client.constant.ClientConstant;
import org.springframework.util.StringUtils;

import java.util.Arrays;

/**
 * Shared URL scope matcher for the filter and MVC authorization interceptor.
 */
public final class SsoUrlScopeMatcher {

    private final String[] urlPatterns;
    private final String[] excludeUrls;

    public SsoUrlScopeMatcher(ClientProperties properties) {
        this.urlPatterns = properties.getUrlPatterns();
        this.excludeUrls = properties.getExcludeUrls();
    }

    public boolean isControlled(String path) {
        return matches(urlPatterns, path) && !matches(excludeUrls, path);
    }

    private boolean matches(String[] patterns, String path) {
        if (patterns == null || patterns.length == 0) {
            return false;
        }
        String normalizedPath = normalize(path);
        return Arrays.stream(patterns)
                .filter(StringUtils::hasText)
                .map(String::trim)
                .anyMatch(pattern -> matches(pattern, normalizedPath));
    }

    private boolean matches(String pattern, String path) {
        if (ClientConstant.URL_FUZZY_MATCH.equals(pattern)) {
            return true;
        }
        if (pattern.endsWith(ClientConstant.URL_FUZZY_MATCH)) {
            return path.startsWith(pattern.substring(0, pattern.length() - ClientConstant.URL_FUZZY_MATCH.length()));
        }
        return pattern.equals(path);
    }

    private String normalize(String path) {
        if (!StringUtils.hasText(path)) {
            return "/";
        }
        return path.startsWith("/") ? path : "/" + path;
    }
}
