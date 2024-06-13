package com.smart.sso.base.util;

import org.springframework.util.StringUtils;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * cookie操作工具
 *
 * @author Joe
 */
public class CookieUtils {

    private CookieUtils() {
    }

    /**
     * 按名称获取cookie
     *
     * @param request
     * @param name
     * @return
     */
    public static Cookie getCookie(String name, HttpServletRequest request) {
        Cookie[] cookies = request.getCookies();
        if (cookies == null || !StringUtils.hasLength(name)) {
            return null;
        }

        for (Cookie cookie : cookies) {
            if (name.equals(cookie.getName())) {
                return cookie;
            }
        }

        return null;
    }

    /**
     * 按名称获取cookie中的值
     *
     * @param request
     * @param name
     * @return
     */
    public static String getCookieValue(String name, HttpServletRequest request) {
        Cookie cookie = getCookie(name, request);
        return cookie == null ? null : cookie.getValue();
    }

    /**
     * 添加cookie
     *
     * @param name
     * @param value
     * @param path
     * @param request
     * @param response
     */
    public static void addCookie(String name, String value, String path, HttpServletRequest request,
                                 HttpServletResponse response) {
        Cookie cookie = new Cookie(name, value);
        if (path != null) {
            cookie.setPath(path);
        }
        if ("https".equals(request.getScheme())) {
            cookie.setSecure(true);
        }
        cookie.setHttpOnly(true);
        response.addCookie(cookie);
    }

    /**
     * 更新cookie中的值
     *
     * @param name
     * @param value
     * @param request
     */
    public static void updateCookie(String name, String value, HttpServletRequest request) {
        Cookie cookie = CookieUtils.getCookie(name, request);
        cookie.setValue(value);
    }

    /**
     * 清除cookie
     *
     * @param name
     * @param path
     * @param response
     */
    public static void removeCookie(String name, String path, HttpServletResponse response) {

        Cookie cookie = new Cookie(name, null);

        if (path != null) {
            cookie.setPath(path);
        }
        cookie.setMaxAge(-1000);
        response.addCookie(cookie);
    }
}
