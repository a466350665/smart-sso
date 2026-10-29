package openjoe.smart.sso.server;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import openjoe.smart.sso.base.annotation.RequiresPermission;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.server.controller.admin.AdminController;
import openjoe.smart.sso.server.entity.Permission;
import openjoe.smart.sso.server.service.PermissionService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
class PermissionRouteCoverageTest {

    private static final Long SERVER_APP_ID = 1L;
    private static final Long DEMO_APP_ID = 82L;
    private static final Long ADMIN_USER_ID = 2L;

    @Autowired
    private PermissionService permissionService;

    @Autowired
    private RequestMappingHandlerMapping handlerMapping;

    @Test
    void enabledPermissionCodesAreStableAndMenusKeepSeparateUrls() {
        List<Permission> permissions = permissionService.list(Wrappers.<Permission>lambdaQuery()
                .eq(Permission::getAppId, SERVER_APP_ID)
                .eq(Permission::getIsEnable, true));

        assertThat(permissions).extracting(Permission::getCode)
                .doesNotContain("/admin/user/save", "/admin/app/save");
        Set<String> codes = permissions.stream().map(Permission::getCode).collect(Collectors.toSet());
        assertThat(codes)
                .contains("app:read", "organization:read", "permission:read", "user:read",
                        "user:write", "user:delete", "menu:user")
                .doesNotContain("app:credential-read", "app:all-read", "app:code-validate",
                        "organization:all-read", "organization:item-read", "permission:item-read",
                        "user:item-read", "user:account-validate", "user:organization-read", "user:role-read");
        assertThat(permissions.stream()
                .filter(p -> "menu".equals(p.getType()))
                .allMatch(p -> p.getUrl() != null && p.getUrl().startsWith("/")))
                .isTrue();
        assertThat(permissions.stream()
                .filter(p -> "function".equals(p.getType()))
                .allMatch(p -> p.getUrl() == null))
                .isTrue();
    }

    @Test
    void adminControllerMethodsUseStablePermissionAnnotations() {
        Map<String, HandlerMethod> methods = handlerMapping.getHandlerMethods().entrySet().stream()
                .filter(entry -> isAdminController(entry.getValue()))
                .collect(Collectors.toMap(entry -> entry.getKey().toString(), Map.Entry::getValue));

        assertThat(methods).isNotEmpty();
        assertThat(methods.values().stream()
                .filter(method -> method.getBeanType() != AdminController.class)
                .filter(method -> method.getMethodAnnotation(RequiresPermission.class) == null)
                .map(method -> method.getBeanType().getSimpleName() + "#" + method.getMethod().getName()))
                .isEmpty();

        Set<String> codes = functionPermissionCodes(SERVER_APP_ID);
        methods.values().stream()
                .map(method -> method.getMethodAnnotation(RequiresPermission.class))
                .filter(annotation -> annotation != null)
                .map(RequiresPermission::value)
                .forEach(code -> assertThat(codes).contains(code));
    }

    @Test
    void defaultAdministratorHasEveryEnabledPermissionCode() {
		Set<String> expected = enabledPermissionCodes(SERVER_APP_ID);
		TokenPermission actual = permissionService.getUserPermission(ADMIN_USER_ID, SERVER_APP_ID);
		assertThat(actual.getPermissionSet()).containsExactlyInAnyOrderElementsOf(expected);
        assertThat(actual.getMenuList()).extracting(menu -> menu.getUrl())
                .contains("/admin/user.html", "/admin/role.html");
    }

    @Test
    void demoAdministratorHasEveryEnabledPermissionCode() {
        Set<String> expected = enabledPermissionCodes(DEMO_APP_ID);
        TokenPermission actual = permissionService.getUserPermission(ADMIN_USER_ID, DEMO_APP_ID);
        assertThat(actual.getPermissionSet()).containsExactlyInAnyOrderElementsOf(expected);
        assertThat(actual.getMenuList()).extracting(menu -> menu.getUrl())
                .containsExactly("/index.html");
    }

    private boolean isAdminController(HandlerMethod handlerMethod) {
        return AdminController.class.getPackageName().equals(handlerMethod.getBeanType().getPackageName());
    }

    private Set<String> functionPermissionCodes(Long appId) {
        return permissionService.list(Wrappers.<Permission>lambdaQuery()
                        .eq(Permission::getAppId, appId)
                        .eq(Permission::getIsEnable, true)
                        .eq(Permission::getType, "function"))
                .stream()
                .map(Permission::getCode)
                .collect(Collectors.toSet());
    }

    private Set<String> enabledPermissionCodes(Long appId) {
        return permissionService.list(Wrappers.<Permission>lambdaQuery()
                        .eq(Permission::getAppId, appId)
                        .eq(Permission::getIsEnable, true))
                .stream()
                .map(Permission::getCode)
                .collect(Collectors.toSet());
    }
}
