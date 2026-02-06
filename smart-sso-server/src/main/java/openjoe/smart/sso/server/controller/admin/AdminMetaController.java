package openjoe.smart.sso.server.controller.admin;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import openjoe.smart.sso.server.dto.PermissionDTO;
import openjoe.smart.sso.server.entity.App;
import openjoe.smart.sso.server.entity.Office;
import openjoe.smart.sso.server.entity.User;
import openjoe.smart.sso.server.service.AppService;
import openjoe.smart.sso.server.service.OfficeService;
import openjoe.smart.sso.server.service.RoleService;
import openjoe.smart.sso.server.service.UserService;
import openjoe.smart.stage.core.entity.Result;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.List;

@Tag(name = "后台管理元数据")
@RestController
@RequestMapping("/admin/meta")
public class AdminMetaController {

    @Autowired
    private AppService appService;
    @Autowired
    private OfficeService officeService;
    @Autowired
    private RoleService roleService;
    @Autowired
    private UserService userService;

    @Operation(summary = "应用列表")
    @ResponseBody
    @RequestMapping(value = "/apps", method = RequestMethod.GET)
    public Result<List<App>> apps() {
        return Result.success(appService.selectAll(true));
    }

    @Operation(summary = "机构列表")
    @ResponseBody
    @RequestMapping(value = "/offices", method = RequestMethod.GET)
    public Result<List<Office>> offices(
            @RequestParam(required = false) Long excludeId) {
        return Result.success(officeService.selectList(true, null, excludeId, "--"));
    }

    @Operation(summary = "用户角色列表")
    @ResponseBody
    @RequestMapping(value = "/roles", method = RequestMethod.GET)
    public Result<List<PermissionDTO>> roles(@RequestParam(required = false) Long userId) {
        return Result.success(roleService.getRoleList(userId));
    }

    @Operation(summary = "用户信息")
    @ResponseBody
    @RequestMapping(value = "/user", method = RequestMethod.GET)
    public Result<User> user(@RequestParam Long id) {
        return Result.success(userService.getById(id));
    }
}
