package com.smart.sso.server.controller.admin;

import com.smart.core.entity.Result;
import com.smart.sso.client.util.SessionUtils;
import com.smart.sso.server.controller.BaseController;
import com.smart.sso.server.service.UserService;
import com.smart.sso.server.validator.ValidateParam;
import com.smart.sso.server.validator.Validator;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import javax.servlet.http.HttpServletRequest;

/**
 * @author Joe
 */
@Api(tags = "个人中心")
@Controller
@RequestMapping("/admin/profile")
@SuppressWarnings("rawtypes")
public class ProfileController extends BaseController {

	@Autowired
	private UserService userService;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute(Model model, HttpServletRequest request) {
		model.addAttribute("user", userService.getById(SessionUtils.getUserId(request)));
		return "/admin/profile";
	}

	@ApiOperation("修改密码提交")
	@ResponseBody
	@RequestMapping(value = "/savePassword", method = RequestMethod.POST)
	public Result save(
			@ValidateParam(name = "新密码", value = { Validator.NOT_BLANK }) String newPassword,
			@ValidateParam(name = "确认密码", value = { Validator.NOT_BLANK }) String confirmPassword,
			HttpServletRequest request) {
		userService.updatePassword(SessionUtils.getUserId(request), newPassword);
		return Result.createSuccess().setMessage("修改成功");
	}
}