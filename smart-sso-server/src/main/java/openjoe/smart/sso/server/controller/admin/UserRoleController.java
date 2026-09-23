package openjoe.smart.sso.server.controller.admin;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import openjoe.smart.sso.server.dto.PermissionDTO;
import openjoe.smart.sso.server.service.RoleService;
import openjoe.smart.sso.server.service.UserRoleService;
import openjoe.smart.sso.server.util.ConvertUtils;
import openjoe.smart.stage.core.entity.Result;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.List;

/**
 * @author Joe
 */
@Tag(name = "用户角色管理")
@RestController
@RequestMapping("/admin/user-role")
public class UserRoleController {

	@Autowired
	private UserRoleService userRoleService;
	@Autowired
	private RoleService roleService;

	@Operation(summary = "初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute(@RequestParam Long userId) {
		return "/";
	}

	@Operation(summary = "用户角色列表")
	@ResponseBody
	@RequestMapping(value = "/roles", method = RequestMethod.GET)
	public Result<List<PermissionDTO>> roles(@RequestParam(required = false) Long userId) {
		return Result.success(roleService.getRoleList(userId));
	}

	@Operation(summary = "新增/修改提交")
	@ResponseBody
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public Result save(
			@RequestParam Long userId,
			@RequestParam(required = false) String roleIds) {
	    userRoleService.allocate(userId, ConvertUtils.convertToIdList(roleIds));
		return Result.success();
	}
}
