package com.smart.sso.server.controller.admin;

import java.util.Date;
import java.util.List;

import javax.annotation.Resource;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.mvc.config.ConfigUtils;
import com.smart.mvc.exception.ValidateException;
import com.smart.mvc.model.Pagination;
import com.smart.mvc.model.Result;
import com.smart.mvc.model.ResultCode;
import com.smart.mvc.util.StringUtils;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.server.controller.common.BaseController;
import com.smart.sso.server.model.Office;
import com.smart.sso.server.model.User;
import com.smart.sso.server.provider.PasswordProvider;
import com.smart.sso.server.service.OfficeService;
import com.smart.sso.server.service.UserService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;

/**
 * @author Joe
 */
@Api(tags = "用户管理")
@Controller
@RequestMapping("/admin/user")
public class UserController extends BaseController {

	@Resource
	private UserService userService;
	@Resource
	private OfficeService officeService;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute(Model model) {
		return "/admin/user";
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
		model.addAttribute("officeList", officeService.findByParams(true, null, null, "----"));
		return "/admin/userEdit";
	}

	@ApiOperation("列表")
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public @ResponseBody Result list(
			@ApiParam(value = "登录名") String account,
			@ApiParam(value = "姓名") String name,
			@ApiParam(value = "机构ID") Integer officeId,
			@ApiParam(value = "开始页码", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer pageNo,
			@ApiParam(value = "显示条数", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer pageSize) {
		return Result.createSuccessResult()
				.setData(userService.findPagination(account, name, officeId, new Pagination<User>(pageNo, pageSize)));
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

	@ApiOperation("启用/禁用")
	@RequestMapping(value = "/enable", method = RequestMethod.POST)
	public @ResponseBody Result enable(
			@ApiParam(value = "ids", required = true) @ValidateParam({ Validator.NOT_BLANK }) String ids,
			@ApiParam(value = "是否启用", required = true) @ValidateParam({ Validator.NOT_BLANK }) Boolean isEnable) {
		userService.enable(isEnable, getAjaxIds(ids));
		return Result.createSuccessResult();
	}

	@ApiOperation("新增/修改提交")
	@ApiResponse(response = Result.class, code = 200, message = "success")
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public @ResponseBody Result save(
			@ApiParam(value = "id") Integer id,
			@ApiParam(value = "机构ID", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer officeId,
			@ApiParam(value = "姓名") String name,
			@ApiParam(value = "登录名", required = true) @ValidateParam({ Validator.NOT_BLANK }) String account,
			@ApiParam(value = "密码 ") String password,
			@ApiParam(value = "是否启用", required = true) @ValidateParam({ Validator.NOT_BLANK }) Boolean isEnable) {
		User user;
		if (id == null) {
			if (StringUtils.isBlank(password)) {
				throw new ValidateException("密码不能为空");
			}
			user = new User();
			user.setCreateTime(new Date());
		}
		else {
			user = userService.get(id);
		}
		user.setOfficeId(officeId);
		user.setName(name);
		user.setAccount(account);
		if (StringUtils.isNotBlank(password)) {
			user.setPassword(PasswordProvider.encrypt(password));
		}
		user.setIsEnable(isEnable);
		userService.save(user);
		return Result.createSuccessResult();
	}

	@ApiOperation("重置密码")
	@RequestMapping(value = "/resetPassword", method = RequestMethod.POST)
	public @ResponseBody Result resetPassword(
			@ApiParam(value = "ids", required = true) @ValidateParam({ Validator.NOT_BLANK }) String ids) {
		userService.resetPassword(PasswordProvider.encrypt(ConfigUtils.getProperty("system.reset.password")), getAjaxIds(ids));
		return Result.createSuccessResult();
	}

	@ApiOperation("删除")
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public @ResponseBody Result delete(
			@ApiParam(value = "ids", required = true) @ValidateParam({ Validator.NOT_BLANK }) String ids) {
		userService.deleteById(getAjaxIds(ids));
		return Result.createSuccessResult();
	}
	
	@ApiOperation("机构树")
	@RequestMapping(value = "/office/tree", method = RequestMethod.GET)
	public @ResponseBody List<Office> officeTree() {
		List<Office> list = officeService.findByParams(true, null, null, "");
		Office office = new Office();
		office.setId(null);
		office.setParentId(-1);
		office.setName("机构");
		list.add(0, office);
		return list;
	}
}