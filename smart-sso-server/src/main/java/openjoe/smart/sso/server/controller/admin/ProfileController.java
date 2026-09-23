package openjoe.smart.sso.server.controller.admin;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import openjoe.smart.sso.client.util.ClientContextHolder;
import openjoe.smart.sso.server.service.UserService;
import openjoe.smart.stage.core.entity.Result;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * @author Joe
 */
@Tag(name = "个人中心")
@RestController
@RequestMapping("/admin/profile")
public class ProfileController {

	@Autowired
	private UserService userService;

	@Operation(summary = "修改密码提交")
	@ResponseBody
	@RequestMapping(value = "/save-password", method = RequestMethod.POST)
	public Result save(@RequestParam String newPassword) {
		userService.updatePassword(ClientContextHolder.getUserId(), newPassword);
		return Result.success().setMessage("修改成功");
	}
}
