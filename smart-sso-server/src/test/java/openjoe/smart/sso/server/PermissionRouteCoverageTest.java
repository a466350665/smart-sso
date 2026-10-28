package openjoe.smart.sso.server;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.server.controller.admin.AdminController;
import openjoe.smart.sso.server.entity.Permission;
import openjoe.smart.sso.server.service.PermissionService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.io.ClassPathResource;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.mvc.method.RequestMappingInfo;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.sql.DataSource;

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

    @Autowired
    private DataSource dataSource;

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Test
    void everyAdminControllerRouteIsRegisteredAsPermission() {
        Set<String> permissionCodes = permissionCodes(SERVER_APP_ID);
        Set<String> adminRoutes = handlerMapping.getHandlerMethods().entrySet().stream()
                .filter(entry -> isAdminController(entry.getValue()))
                .map(Map.Entry::getKey)
                .map(RequestMappingInfo::getPatternValues)
                .flatMap(Set::stream)
                .collect(Collectors.toSet());

        assertThat(adminRoutes).hasSize(37);
        assertThat(permissionCodes).containsAll(adminRoutes);
    }

    @Test
    void everyProtectedPageIsRegisteredAsPermission() {
        assertThat(permissionCodes(SERVER_APP_ID)).contains(
                "/", "/admin/admin.html",
                "/admin/app.html",
                "/admin/login-user.html",
                "/admin/organization.html",
                "/admin/permission.html", "/admin/profile.html",
                "/admin/role.html", "/admin/role-permission.html",
                "/admin/user.html", "/admin/user-role.html");
        assertThat(permissionCodes(SERVER_APP_ID)).doesNotContain(
                "/index.html",
                "/admin/app-edit.html", "/admin/user-edit.html",
                "/admin/role-edit.html", "/admin/organization-edit.html");
        assertThat(permissionCodes(DEMO_APP_ID)).contains("/", "/index.html", "/userinfo");
    }

    @Test
    void defaultAdministratorHasEveryEnabledPermission() {
        assertHasEveryPermission(ADMIN_USER_ID, SERVER_APP_ID);
        assertHasEveryPermission(ADMIN_USER_ID, DEMO_APP_ID);
    }

    @Test
    void saveRoutesUseEditPermissionNames() {
        Map<String, String> namesByCode = permissionService.list(Wrappers.<Permission>lambdaQuery()
                        .eq(Permission::getAppId, SERVER_APP_ID))
                .stream()
                .collect(Collectors.toMap(Permission::getCode, Permission::getName));

        assertThat(namesByCode).containsEntry("/admin/app/save", "应用编辑")
                .containsEntry("/admin/user/save", "用户编辑")
                .containsEntry("/admin/role/save", "角色编辑")
                .containsEntry("/admin/permission/save", "权限编辑")
                .containsEntry("/admin/organization/save", "机构编辑");
    }

    @Test
    void productionUpgradeScriptIsIdempotent() {
        jdbcTemplate.update("INSERT INTO sso_permission "
                        + "(id, app_id, parent_id, name, code, sort, icon, type, is_enable, create_time, update_time) "
                        + "VALUES (?, ?, NULL, ?, ?, ?, '', 'function', TRUE, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)",
                999L, SERVER_APP_ID, "Legacy admin page", "/index.html", 99);
        jdbcTemplate.update("INSERT INTO sso_role_permission (role_id, permission_id, app_id) VALUES (?, ?, ?)",
                999L, 999L, SERVER_APP_ID);

        ResourceDatabasePopulator populator = new ResourceDatabasePopulator(
                new ClassPathResource("upgrade-permission-fail-closed.sql"));

        populator.execute(dataSource);
        populator.execute(dataSource);

        assertThat(permissionCodes(SERVER_APP_ID)).hasSize(48);
        assertThat(permissionCodes(DEMO_APP_ID)).hasSize(10);
        assertThat(jdbcTemplate.queryForObject(
                "SELECT COUNT(*) FROM sso_permission WHERE app_id=1 AND code='/index.html'", Integer.class))
                .isZero();
        assertThat(jdbcTemplate.queryForObject(
                "SELECT COUNT(*) FROM sso_role_permission rp "
                        + "JOIN sso_permission p ON p.id=rp.permission_id "
                        + "WHERE rp.role_id=999 AND rp.app_id=1 AND p.code='/admin/admin.html'", Integer.class))
                .isEqualTo(1);
        assertHasEveryPermission(ADMIN_USER_ID, SERVER_APP_ID);
        assertHasEveryPermission(ADMIN_USER_ID, DEMO_APP_ID);
    }

    private boolean isAdminController(HandlerMethod handlerMethod) {
        return AdminController.class.getPackageName()
                .equals(handlerMethod.getBeanType().getPackageName());
    }

    private Set<String> permissionCodes(Long appId) {
        List<Permission> permissions = permissionService.list(Wrappers.<Permission>lambdaQuery()
                .eq(Permission::getAppId, appId)
                .eq(Permission::getIsEnable, true));
        return permissions.stream().map(Permission::getCode).collect(Collectors.toSet());
    }

    private void assertHasEveryPermission(Long userId, Long appId) {
        Set<String> expected = permissionCodes(appId);
        TokenPermission actual = permissionService.getUserPermission(userId, appId);
        assertThat(actual.getPermissionSet()).containsExactlyInAnyOrderElementsOf(expected);
        assertThat(actual.getNoPermissionSet()).isEmpty();
    }
}
