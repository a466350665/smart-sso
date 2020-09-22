package com.smart.sso.server.controller.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.mvc.controller.BaseController;
import com.smart.mvc.model.Page;
import com.smart.mvc.model.Result;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.server.model.Role;
import com.smart.sso.server.service.RoleService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

/**
 * @author Joe
 */
@Api(tags = "角色管理")
@Controller
@RequestMapping("/admin/role")
@SuppressWarnings("rawtypes")
public class RoleController extends BaseController {

	@Autowired
	private RoleService roleService;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute(Model model) {
		return "/admin/role";
	}

	@ApiOperation("新增/修改页")
	@RequestMapping(value = "/edit", method = RequestMethod.GET)
	public String edit(@ValidateParam(name = "id") Integer id, Model model) {
		Role role;
		if (id == null) {
			role = new Role();
		}
		else {
			role = roleService.get(id);
		}
		model.addAttribute("role", role);
		return "/admin/roleEdit";
	}

	@ApiOperation("列表")
	@ResponseBody
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public Result list(
			@ValidateParam(name = "角色名")String name,
			@ValidateParam(name = "开始页码", defaultValue = DEFAULT_PAGE_NO) Integer pageNo,
	        @ValidateParam(name = "显示条数", defaultValue = DEFAULT_PAGE_SIZE) Integer pageSize) {
		return Result.createSuccess(roleService.selectPage(name, Page.create(pageNo, pageSize)));
	}

	@ApiOperation("启用/禁用")
	@ResponseBody
	@RequestMapping(value = "/enable", method = RequestMethod.POST)
	public Result enable(
	    @ValidateParam(name = "ids", value = { Validator.NOT_BLANK }) String ids,
			@ValidateParam(name = "是否启用", value = { Validator.NOT_BLANK }) Boolean isEnable) {
		roleService.enable(isEnable, convertToIdList(ids));
		return Result.success();
	}

	@ApiOperation("新增/修改提交")
	@ResponseBody
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public Result save(
	        @ValidateParam(name = "id") Integer id,
			@ValidateParam(name = "角色名", value = { Validator.NOT_BLANK }) String name,
			@ValidateParam(name = "排序", value = { Validator.NOT_BLANK }) Integer sort,
			@ValidateParam(name = "描述") String description,
			@ValidateParam(name = "是否启用", value = { Validator.NOT_BLANK }) Boolean isEnable) {
		Role role;
		if (id == null) {
			role = new Role();
		}
		else {
			role = roleService.get(id);
		}
		role.setName(name);
		role.setSort(sort);
		role.setDescription(description);
		role.setIsEnable(isEnable);
		roleService.save(role);
		return Result.success();
	}
	
	@ApiOperation("删除")
	@ResponseBody
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public Result delete(
	    @ValidateParam(name = "ids", value = { Validator.NOT_BLANK }) String ids) {
		roleService.deleteByIds(convertToIdList(ids));
		return Result.success();
	}
}