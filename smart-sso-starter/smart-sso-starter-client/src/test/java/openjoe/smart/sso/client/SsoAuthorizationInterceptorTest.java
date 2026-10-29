package openjoe.smart.sso.client;

import openjoe.smart.sso.base.annotation.RequiresPermission;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.base.entity.TokenUser;
import openjoe.smart.sso.client.constant.ClientConstant;
import openjoe.smart.sso.client.util.ClientContextHolder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.web.method.HandlerMethod;

import java.lang.reflect.Method;
import java.util.Collections;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

class SsoAuthorizationInterceptorTest {

    private final ClientProperties properties = properties();
    private final SsoAuthorizationInterceptor interceptor =
            new SsoAuthorizationInterceptor(new SsoUrlScopeMatcher(properties));
    private final TestController controller = new TestController();

    @AfterEach
    void tearDown() {
        ClientContextHolder.reset();
    }

    @Test
    void permitsUserWithRequiredPermission() throws Exception {
        MockHttpServletResponse response = createContext("/api/user", Set.of("user:read"));

        assertThat(interceptor.preHandle(request("/api/user"), response, handler("secured"))).isTrue();
        assertThat(response.getContentAsString()).isEmpty();
    }

    @Test
    void rejectsUserWithoutRequiredPermission() throws Exception {
        MockHttpServletResponse response = createContext("/api/user", Collections.emptySet());

        assertThat(interceptor.preHandle(request("/api/user"), response, handler("secured"))).isFalse();
        assertThat(response.getStatus()).isEqualTo(403);
        assertThat(response.getContentAsString()).contains(ClientConstant.NO_PERMISSION);
    }

    @Test
    void permitsUnannotatedControlledMethodAfterLogin() throws Exception {
        MockHttpServletResponse response = createContext("/api/public-profile", Collections.emptySet());

        assertThat(interceptor.preHandle(request("/api/public-profile"), response, handler("unannotated"))).isTrue();
        assertThat(response.getContentAsString()).isEmpty();
    }

    @Test
    void skipsPermissionAnnotationOutsideConfiguredScope() throws Exception {
        MockHttpServletResponse response = new MockHttpServletResponse();

        assertThat(interceptor.preHandle(request("/outside"), response, handler("secured"))).isTrue();
        assertThat(response.getContentAsString()).isEmpty();
    }

    private ClientProperties properties() {
        ClientProperties value = new ClientProperties();
        value.setUrlPatterns(new String[]{"/api/*"});
        value.setExcludeUrls(new String[]{"/api/public/*"});
        return value;
    }

    private MockHttpServletRequest request(String path) {
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.setServletPath(path);
        return request;
    }

    private MockHttpServletResponse createContext(String path, Set<String> permissions) {
        MockHttpServletRequest request = request(path);
        MockHttpServletResponse response = new MockHttpServletResponse();
        ClientContextHolder.create(request, response);
        TokenUser user = new TokenUser();
        user.setId(1L);
        ClientContextHolder.setUser(user);
        ClientContextHolder.setPermission(new TokenPermission(permissions, Collections.emptyList()));
        return response;
    }

    private HandlerMethod handler(String methodName) throws NoSuchMethodException {
        Method method = TestController.class.getMethod(methodName);
        return new HandlerMethod(controller, method);
    }

    static class TestController {
        @RequiresPermission("user:read")
        public void secured() {
        }

        public void unannotated() {
        }
    }
}
