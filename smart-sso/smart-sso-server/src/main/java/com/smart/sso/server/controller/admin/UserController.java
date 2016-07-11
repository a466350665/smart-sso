package com.smart.sso.server.controller.admin;

import java.util.Date;
import java.util.List;

import javax.annotation.Resource;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.ssm.config.ConfigUtils;
import com.smart.ssm.controller.BaseController;
import com.smart.ssm.exception.ValidateException;
import com.smart.ssm.model.JSONResult;
import com.smart.ssm.model.Pagination;
import com.smart.ssm.model.ResultCode;
import com.smart.ssm.provider.PasswordProvider;
import com.smart.ssm.validator.Validator;
import com.smart.ssm.validator.annotation.ValidateParam;
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
	public @ResponseBody JSONResult list(@ValidateParam(name = "登录名 ") String account,
			@ValidateParam(name = "应用ID ") Integer appId,
			@ValidateParam(name = "开始页码", validators = { Validator.NOT_BLANK }) Integer pageNo,
			@ValidateParam(name = "显示条数 ", validators = { Validator.NOT_BLANK }) Integer pageSize) {
		return new JSONResult(userService.findPaginationByAccount(account, appId, new Pagination<User>(pageNo, pageSize)));
	}

	@RequestMapping(value = "/validateCode", method = RequestMethod.POST)
	public @ResponseBody JSONResult validateAccount(
			@ValidateParam(name = "id", validators = { Validator.NOT_BLANK }) Integer id,
			@ValidateParam(name = "登录名 ", validators = { Validator.NOT_BLANK }) String account,
			@ValidateParam(name = "应用ID ", validators = { Validator.NOT_BLANK }) Integer appId) {
		JSONResult result = new JSONResult();
		if (StringUtils.isNotBlank(account)) {
			User user = userService.findByAccount(account);
			if (null != user && !user.getId().equals(id)) {
				result.setStatus(ResultCode.ERROR);
				result.setMessage("登录名已存在");
			}
		}
		return result;
	}

	@RequestMapping(value = "/enable", method = RequestMethod.POST)
	public @ResponseBody JSONResult enable(@ValidateParam(name = "ids", validators = { Validator.NOT_BLANK })String ids,
			@ValidateParam(name = "是否启用 ", validators = { Validator.NOT_BLANK }) Boolean isEnable) {
		userService.enable(isEnable, getAjaxIds(ids));
		return new JSONResult();
	}

	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public @ResponseBody JSONResult save(@ValidateParam(name = "ID") Integer id,
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
		userService.saveOrUpdate(user);
		return new JSONResult();
	}

	@RequestMapping(value = "/resetPassword", method = RequestMethod.POST)
	public @ResponseBody JSONResult resetPassword(
			@ValidateParam(name = "ids", validators = { Validator.NOT_BLANK }) String ids) {
		userService.resetPassword(PasswordProvider.encrypt(ConfigUtils.getProperty("system.init.password")), getAjaxIds(ids));
		return new JSONResult();
	}

	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public @ResponseBody JSONResult delete(@ValidateParam(name = "ids", validators = { Validator.NOT_BLANK }) String ids) {
		return new JSONResult(userService.deleteById(getAjaxIds(ids)));
	}

	private List<App> getAppList() {
		List<App> appList = null;
		if (appList == null) {
			appList = appService.findByAll(null);
		}
		return appList;
	}
}