package com.smart.demo.controller.admin;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.demo.service.UserService;
import com.smart.mvc.controller.BaseController;
import com.smart.mvc.model.Result;
import com.smart.mvc.model.ResultCode;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.client.ApplicationUtils;
import com.smart.sso.rpc.AuthenticationRpcService;

/**
 * @author Joe
 */
@Api(tags = "个人中心")
@Controller
@RequestMapping("/admin/profile")
public class ProfileController extends BaseController {

	@Resource
	private UserService userService;
	@Resource
	private AuthenticationRpcService authenticationRpcService;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute(Model model, HttpServletRequest request) {
		model.addAttribute("user", ApplicationUtils.getSessionUser(request).getProfile());
		return "/admin/profile";
	}

	@ApiOperation("修改密码提交")
	@RequestMapping(value = "/savePassword", method = RequestMethod.POST)
	public @ResponseBody Result save(
			@ApiParam(value = "新密码", required = true) @ValidateParam({ Validator.NOT_BLANK }) String newPassword,
			@ApiParam(value = "确认密码", required = true) @ValidateParam({ Validator.NOT_BLANK }) String confirmPassword,
			HttpServletRequest request) {
		if (newPassword.equals(confirmPassword)
				&& authenticationRpcService.updatePassword(ApplicationUtils.getSessionUser(request).getToken(),
						newPassword))
			return Result.createSuccessResult().setMessage("修改成功");
		else
			return Result.create(ResultCode.VALIDATE_ERROR).setMessage("修改失败");
	}
}