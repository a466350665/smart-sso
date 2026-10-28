package openjoe.smart.sso.client.filter;

import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.client.ClientProperties;
import openjoe.smart.sso.client.constant.ClientConstant;
import openjoe.smart.sso.client.util.ClientContextHolder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import java.util.Collections;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

class PermissionFilterTest {

    private final PermissionFilter filter = new PermissionFilter(new ClientProperties());

    @AfterEach
    void tearDown() {
        ClientContextHolder.reset();
    }

    @Test
    void permitsAssignedPath() throws Exception {
        MockHttpServletResponse response = createContext(
                new TokenPermission(Set.of("/admin/user/list"), Collections.emptySet()),
                "/admin/user/list");

        assertThat(filter.isAccessAllowed()).isTrue();
        assertThat(response.getContentAsString()).isEmpty();
    }

    @Test
    void rejectsExplicitlyDeniedPathWithForbiddenStatus() throws Exception {
        MockHttpServletResponse response = createContext(
                new TokenPermission(Collections.emptySet(), Set.of("/admin/user/list")),
                "/admin/user/list");

        assertThat(filter.isAccessAllowed()).isFalse();
        assertThat(response.getStatus()).isEqualTo(403);
        assertThat(response.getContentAsString()).contains(ClientConstant.NO_PERMISSION);
    }

    @Test
    void permitsUnmappedPath() throws Exception {
        MockHttpServletResponse response = createContext(
                new TokenPermission(Collections.emptySet(), Collections.emptySet()),
                "/admin/unmapped");

        assertThat(filter.isAccessAllowed()).isTrue();
        assertThat(response.getStatus()).isEqualTo(200);
        assertThat(response.getContentAsString()).isEmpty();
    }

    private MockHttpServletResponse createContext(TokenPermission permission, String servletPath) {
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.setServletPath(servletPath);
        MockHttpServletResponse response = new MockHttpServletResponse();
        ClientContextHolder.create(request, response);
        ClientContextHolder.setPermission(permission);
        return response;
    }
}
