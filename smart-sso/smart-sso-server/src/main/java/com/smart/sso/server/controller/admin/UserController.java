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
import com.smart.mvc.controller.BaseController;
import com.smart.mvc.exception.ValidateException;
import com.smart.mvc.model.Result;
import com.smart.mvc.model.Pagination;
import com.smart.mvc.model.ResultCode;
import com.smart.mvc.provider.PasswordProvider;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.server.model.App;
import com.smart.sso.server.model.User;
import com.smart.sso.server.service.AppService;
import com.smart.sso.server.service.RoleService;
import com.smart.sso.server.service.UserRoleService;
import com.smart.sso.server.service.UserService;
import com.smart.util.StringUtils;

/**
 * 管理员管理
 * 
 * @author Joe
 */
@Controller
@RequestMapping("/admin/user")
public class UserController extends BaseController {

	@Resource
	private UserService userService;
	@Resource
	private AppService appService;
	@Resource
	private RoleService roleService;
	@Resource
	private UserRoleService userRoleService;

	@RequestMapping(method = RequestMethod.GET)
	public String execute(Model model) {
		model.addAttribute("appList", getAppList());
		return "/admin/user";
	}

	@RequestMapping(value = "/edit", method = RequestMethod.GET)
	public String edit(@ValidateParam(name = "id") Integer id, Model model) {
		User user;
		if (id == null) {
			user = new User();
		}
		else {
			user = userService.get(id);
		}
		model.addAttribute("user", user);
		model.addAttribute("appList", getAppList());
		return "/admin/userEdit";
	}

	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public @ResponseBody Result list(@ValidateParam(name = "登录名 ") String account,
			@ValidateParam(name = "应用ID ") Integer appId,
			@ValidateParam(name = "开始页码", validators = { Validator.NOT_BLANK }) Integer pageNo,
			@ValidateParam(name = "显示条数 ", validators = { Validator.NOT_BLANK }) Integer pageSize) {
		return Result.createSuccessResult().setData(userService.findPaginationByAccount(account, appId, new Pagination<User>(pageNo, pageSize)));
	}

	@RequestMapping(value = "/validateAccount", method = RequestMethod.POST)
	public @ResponseBody Result validateAccount(
			@ValidateParam(name = "id") Integer id,
			@ValidateParam(name = "登录名 ", validators = { Validator.NOT_BLANK }) String account) {
		Result result = Result.createSuccessResult();
		if (StringUtils.isNotBlank(account)) {
			User user = userService.findByAccount(account);
			if (null != user && !user.getId().equals(id)) {
				result.setCode(ResultCode.ERROR).setMessage("登录名已存在");
			}
		}
		return result;
	}

	@RequestMapping(value = "/enable", method = RequestMethod.POST)
	public @ResponseBody Result enable(@ValidateParam(name = "ids", validators = { Validator.NOT_BLANK })String ids,
			@ValidateParam(name = "是否启用 ", validators = { Validator.NOT_BLANK }) Boolean isEnable) {
		userService.enable(isEnable, getAjaxIds(ids));
		return Result.createSuccessResult();
	}

	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public @ResponseBody Result save(@ValidateParam(name = "ID") Integer id,
			@ValidateParam(name = "登录名", validators = { Validator.NOT_BLANK }) String account,
			@ValidateParam(name = "密码 ") String password,
			@ValidateParam(name = "是否启用 ", validators = { Validator.NOT_BLANK }) Boolean isEnable) {
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
		user.setAccount(account);
		if (StringUtils.isNotBlank(password)) {
			user.setPassword(PasswordProvider.encrypt(password));
		}
		user.setIsEnable(isEnable);
		userService.save(user);
		return Result.createSuccessResult();
	}

	@RequestMapping(value = "/resetPassword", method = RequestMethod.POST)
	public @ResponseBody Result resetPassword(
			@ValidateParam(name = "ids", validators = { Validator.NOT_BLANK }) String ids) {
		userService.resetPassword(PasswordProvider.encrypt(ConfigUtils.getProperty("system.init.password")), getAjaxIds(ids));
		return Result.createSuccessResult();
	}

	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public @ResponseBody Result delete(@ValidateParam(name = "ids", validators = { Validator.NOT_BLANK }) String ids) {
		userService.deleteById(getAjaxIds(ids));
		return Result.createSuccessResult();
	}

	private List<App> getAppList() {
		return appService.findByAll(null);
	}
}