package openjoe.smart.sso.server.controller.admin;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import openjoe.smart.sso.client.util.TokenUtils;
import openjoe.smart.sso.server.service.UserService;
import openjoe.smart.stage.core.entity.Result;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import javax.servlet.http.HttpServletRequest;

/**
 * @author Joe
 */
@Api(tags = "个人中心")
@Controller
@RequestMapping("/admin/profile")
public class ProfileController {

	@Autowired
	private UserService userService;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute(Model model, HttpServletRequest request) {
		model.addAttribute("user", userService.getById(TokenUtils.getUserId(request)));
		return "/admin/profile";
	}

	@ApiOperation("修改密码提交")
	@ResponseBody
	@RequestMapping(value = "/save-password", method = RequestMethod.POST)
	public Result save(
			@RequestParam String newPassword,
			@RequestParam String confirmPassword,
			HttpServletRequest request) {
		userService.updatePassword(TokenUtils.getUserId(request), newPassword);
		return Result.success().setMessage("修改成功");
	}
}