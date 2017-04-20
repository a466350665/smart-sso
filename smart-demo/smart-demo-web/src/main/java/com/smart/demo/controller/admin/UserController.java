package com.smart.demo.controller.admin;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;

import javax.annotation.Resource;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.demo.model.User;
import com.smart.demo.service.UserService;
import com.smart.mvc.controller.BaseController;
import com.smart.mvc.model.Pagination;
import com.smart.mvc.model.Result;
import com.smart.mvc.model.ResultCode;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;

/**
 * @author Joe
 */
@Api(tags = "管理员管理")
@Controller
@RequestMapping("/admin/user")
public class UserController extends BaseController {

	@Resource
	private UserService userService;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute() {
		return "/admin/user";
	}
	
	@ApiOperation("列表")
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public @ResponseBody Result list(
			@ApiParam(value = "登录名") String account,
			@ApiParam(value = "开始页码", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer pageNo,
			@ApiParam(value = "显示条数", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer pageSize) {
		return Result.createSuccessResult().setData(
				userService.findPaginationByAccount(account, new Pagination<User>(pageNo, pageSize)));
	}
	
	@ApiOperation("验证登录名")
	@RequestMapping(value = "/validateAccount", method = RequestMethod.POST)
	public @ResponseBody Result validateAccount(
			@ApiParam(value = "id") Integer id,
			@ApiParam(value = "登录名", required = true) @ValidateParam({ Validator.NOT_BLANK }) String account) {
		Result result = Result.createSuccessResult();
		User user = userService.findByAccount(account);
		if (null != user && !user.getId().equals(id)) {
			result.setCode(ResultCode.ERROR).setMessage("登录名已存在");
		}
		return result;
	}

	@ApiOperation("新增/修改页")
	@RequestMapping(value = "/edit", method = RequestMethod.GET)
	public String edit(@ApiParam(value = "id") Integer id, Model model) {
		User user;
		if (id == null) {
			user = new User();
		}
		else {
			user = userService.get(id);
		}
		model.addAttribute("user", user);
		return "/admin/userEdit";
	}

	@ApiOperation("新增/修改提交")
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public @ResponseBody Result save(
			@ApiParam(value = "id") Integer id,
			@ApiParam(value = "登录名", required = true) @ValidateParam({ Validator.NOT_BLANK }) String account) {
		Result result = null;
		if (!(result = validateAccount(id, account)).isSuccess()) {
			return result;
		}
		User user;
		if (id == null) {
			user = new User();
		}
		else {
			user = userService.get(id);
		}
		user.setAccount(account);
		userService.save(user);
		return Result.createSuccessResult();
	}

	@ApiOperation("删除")
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public @ResponseBody Result delete(
			@ApiParam(value = "ids", required = true) @ValidateParam({ Validator.NOT_BLANK }) String ids) {
		userService.deleteById(getAjaxIds(ids));
		return Result.createSuccessResult();
	}
}