package openjoe.smart.sso.server.controller.admin;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import openjoe.smart.sso.server.entity.Role;
import openjoe.smart.sso.server.service.RoleService;
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
@Tag(name = "角色管理")
@Controller
@RequestMapping("/admin/role")
@SuppressWarnings("rawtypes")
public class RoleController {

	@Autowired
	private RoleService roleService;

    @Operation(summary = "初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute(Model model) {
		return "/admin/role";
	}

    @Operation(summary = "新增/修改页")
	@RequestMapping(value = "/edit", method = RequestMethod.GET)
	public String edit(@RequestParam(required = false) Long id, Model model) {
		Role role;
		if (id == null) {
			role = new Role();
			role.setIsEnable(true);
		}
		else {
			role = roleService.getById(id);
		}
		model.addAttribute("role", role);
		return "/admin/role-edit";
	}

    @Operation(summary = "列表")
	@ResponseBody
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public Result list(
			@RequestParam(required = false) String name,
			@RequestParam Long current,
			@RequestParam Long size) {
		return Result.success(roleService.selectPage(name, current, size));
	}

    @Operation(summary = "启用/禁用")
	@ResponseBody
	@RequestMapping(value = "/enable", method = RequestMethod.POST)
	public Result enable(
			@RequestParam String ids,
			@RequestParam Boolean isEnable) {
		roleService.enable(isEnable, ConvertUtils.convertToIdList(ids));
		return Result.success();
	}

    @Operation(summary = "新增/修改提交")
	@ResponseBody
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public Result save(
			@RequestParam(required = false) Long id,
			@RequestParam String name,
			@RequestParam Integer sort,
			@RequestParam(required = false) String description,
			@RequestParam Boolean isEnable) {
		Role role;
		if (id == null) {
			role = new Role();
		}
		else {
			role = roleService.getById(id);
		}
		role.setName(name);
		role.setSort(sort);
		role.setDescription(description);
		role.setIsEnable(isEnable);
		roleService.saveOrUpdate(role);
		return Result.success();
	}

    @Operation(summary = "删除")
	@ResponseBody
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public Result delete(
			@RequestParam String ids) {
		roleService.deleteByIds(ConvertUtils.convertToIdList(ids));
		return Result.success();
	}
}