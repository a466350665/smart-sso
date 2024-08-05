package openjoe.smart.sso.client.util;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class ClientContextHolder {

    private static final ThreadLocal<HttpServletRequest> REQUEST_HOLDER = new ThreadLocal<>();
    private static final ThreadLocal<HttpServletResponse> RESPONSE_HOLDER = new ThreadLocal<>();

    public static void reset() {
        REQUEST_HOLDER.remove();
        RESPONSE_HOLDER.remove();
    }

    public static void set(HttpServletRequest request, HttpServletResponse response) {
        REQUEST_HOLDER.set(request);
        RESPONSE_HOLDER.set(response);
    }

    public static HttpServletRequest getRequest() {
        return REQUEST_HOLDER.get();
    }

    public static HttpServletResponse getResponse() {
        return RESPONSE_HOLDER.get();
    }
}