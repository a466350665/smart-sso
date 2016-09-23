package com.smart.base.controller.admin;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.base.service.UserService;
import com.smart.mvc.controller.BaseController;
import com.smart.mvc.model.Result;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.client.ApplicationUtils;
import com.smart.sso.rpc.AuthenticationRpcService;

/**
 * 管理员管理
 * 
 * @author Joe
 */
@Controller
@RequestMapping("/admin/profile")
public class ProfileController extends BaseController {

	@Resource
	private UserService userService;
	@Resource
	private AuthenticationRpcService authenticationRpcService;

	@RequestMapping(method = RequestMethod.GET)
	public String execute(Model model, HttpServletRequest request) {
		model.addAttribute("user", ApplicationUtils.getSessionUser(request).getProfile());
		return "/admin/profile";
	}

	@RequestMapping(value = "/savePassword", method = RequestMethod.POST)
	public @ResponseBody Result save(
			@ValidateParam(name = "新密码", validators = { Validator.NOT_BLANK }) String newPassword,
			@ValidateParam(name = "确认密码", validators = { Validator.NOT_BLANK }) String confirmPassword,
			HttpServletRequest request) {
		if (newPassword.equals(confirmPassword)
				&& authenticationRpcService.updatePassword(ApplicationUtils.getSessionUser(request).getToken(),
						newPassword))
			return Result.createSuccessResult().setMessage("修改成功");
		else
			return Result.createErrorResult().setMessage("修改失败");
	}
}