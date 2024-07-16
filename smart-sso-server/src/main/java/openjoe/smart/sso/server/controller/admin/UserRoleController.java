package openjoe.smart.sso.server.controller.admin;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import openjoe.smart.sso.server.service.RoleService;
import openjoe.smart.sso.server.service.UserRoleService;
import openjoe.smart.sso.server.service.UserService;
import openjoe.smart.sso.server.util.ConvertUtils;
import openjoe.smart.stage.core.entity.Result;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * @author Joe
 */
@Api(tags = "用户角色管理")
@Controller
@RequestMapping("/admin/userRole")
public class UserRoleController {

	@Autowired
	private UserService userService;
	@Autowired
	private RoleService roleService;
	@Autowired
	private UserRoleService userRoleService;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute(
			@RequestParam Long userId,
	        Model model) {
		model.addAttribute("user", userService.getById(userId));
		model.addAttribute("roleList", roleService.getRoleList(userId));
		return "/admin/userRole";
	}

	@ApiOperation("新增/修改提交")
	@ResponseBody
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public Result save(
			@RequestParam Long userId,
			@RequestParam(required = false) String roleIds) {
	    userRoleService.allocate(userId, ConvertUtils.convertToIdList(roleIds));
		return Result.success();
	}
}