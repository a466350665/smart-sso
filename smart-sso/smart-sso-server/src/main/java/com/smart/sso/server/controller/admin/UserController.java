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
import com.smart.mvc.model.Pagination;
import com.smart.mvc.model.Result;
import com.smart.mvc.model.ResultCode;
import com.smart.mvc.provider.PasswordProvider;
import com.smart.mvc.util.StringUtils;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.server.model.App;
import com.smart.sso.server.model.User;
import com.smart.sso.server.model.UserApp;
import com.smart.sso.server.service.AppService;
import com.smart.sso.server.service.RoleService;
import com.smart.sso.server.service.UserAppService;
import com.smart.sso.server.service.UserRoleService;
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
	private AppService appService;
	@Resource
	private RoleService roleService;
	@Resource
	private UserAppService userAppService;
	@Resource
	private UserRoleService userRoleService;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute(Model model) {
		model.addAttribute("appList", getAppList(null));
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
		model.addAttribute("appList", getAppList(user.getId()));
		return "/admin/userEdit";
	}

	@ApiOperation("列表")
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public @ResponseBody Result list(
			@ApiParam(value = "登录名") String account,
			@ApiParam(value = "应用id") Integer appId,
			@ApiParam(value = "开始页码", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer pageNo,
			@ApiParam(value = "显示条数", required = true) @ValidateParam({ Validator.NOT_BLANK }) Integer pageSize) {
		return Result.createSuccessResult().setData(userService.findPaginationByAccount(account, appId, new Pagination<User>(pageNo, pageSize)));
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
			@ApiParam(value = "登录名", required = true) @ValidateParam({ Validator.NOT_BLANK }) String account,
			@ApiParam(value = "密码 ") String password,
			@ApiParam(value = "是否启用", required = true) @ValidateParam({ Validator.NOT_BLANK }) Boolean isEnable,
			@ApiParam(value = "应用ids") String appIds) {
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
		userService.save(user, getAjaxIds(appIds));
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
	
	private List<App> getAppList(Integer userId) {
		List<App> list = appService.findByAll(null);
		if (userId != null) {
			for (App app : list) {
				UserApp userApp = userAppService.findByUserAppId(userId, app.getId());
				if (null != userApp) {
					app.setIsChecked(true);
				}
				else {
					app.setIsChecked(false);
				}
			}
		}
		return list;
	}
}