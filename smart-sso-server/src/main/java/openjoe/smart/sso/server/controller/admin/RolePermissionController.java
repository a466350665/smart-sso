package openjoe.smart.sso.server.controller.admin;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import openjoe.smart.sso.server.service.RolePermissionService;
import openjoe.smart.sso.server.util.ConvertUtils;
import openjoe.smart.stage.core.entity.Result;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * @author Joe
 */
@Tag(name = "用户角色关系管理")
@RestController
@RequestMapping("/admin/role-permission")
@SuppressWarnings("rawtypes")
public class RolePermissionController {

	@Autowired
	private RolePermissionService rolePermissionService;

	@Operation(summary = "初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String edit(@RequestParam Long roleId) {
		return "/";
	}

    @Operation(summary = "角色授权提交")
	@ResponseBody
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public Result save(
			@RequestParam Long appId,
			@RequestParam Long roleId,
			@RequestParam(required = false) String permissionIds) {
		rolePermissionService.allocate(appId, roleId, ConvertUtils.convertToIdList(permissionIds));
		return Result.success().setMessage("授权成功");
	}
}
