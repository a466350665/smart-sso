package openjoe.smart.sso.server.controller.admin;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import openjoe.smart.sso.server.stage.core.Result;
import openjoe.smart.sso.server.stage.exception.ApplicationException;
import openjoe.smart.sso.server.entity.Office;
import openjoe.smart.sso.server.entity.User;
import openjoe.smart.sso.server.enums.ErrorCodeEnum;
import openjoe.smart.sso.server.service.OfficeService;
import openjoe.smart.sso.server.service.UserService;
import openjoe.smart.sso.server.util.ConvertUtils;
import openjoe.smart.sso.server.util.PasswordHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.Date;
import java.util.List;

/**
 * @author Joe
 */
@Api(tags = "用户管理")
@Controller
@RequestMapping("/admin/user")
@SuppressWarnings("rawtypes")
public class UserController {

    @Value("${system.reset.password:123456}")
    private String resetPassword;
	@Autowired
	private UserService userService;
	@Autowired
	private OfficeService officeService;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute(Model model) {
		return "/admin/user";
	}

	@ApiOperation("新增/修改页")
	@RequestMapping(value = "/edit", method = RequestMethod.GET)
	public String edit(@RequestParam(required = false) Long id,
					   @RequestParam(required = false) Long officeId,
					   Model model) {
		User user;
		if (id == null) {
			user = new User();
			user.setIsEnable(true);
			user.setOfficeId(officeId);
		}
		else {
			user = userService.getById(id);
		}
		model.addAttribute("user", user);
		model.addAttribute("officeList", officeService.selectList(true, null, null, "--"));
		return "/admin/user-edit";
	}

	@ApiOperation("列表")
	@ResponseBody
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public Result list(
			@RequestParam(required = false) String account,
			@RequestParam(required = false) String name,
			@RequestParam(required = false) Long officeId,
			@RequestParam Long current,
			@RequestParam Long size) {
		return Result.success(userService.selectPage(account, name, officeId, current, size));
	}

	@ApiOperation("验证登录名")
	@ResponseBody
	@RequestMapping(value = "/validate-account", method = RequestMethod.POST)
	public Result validateAccount(
	        @RequestParam(required = false) Long id,
			@RequestParam String account) {
		User user = userService.selectByAccount(account);
		if (null != user && !user.getId().equals(id)) {
		    throw new ApplicationException(ErrorCodeEnum.E1004);
		}
		return Result.success();
	}

	@ApiOperation("启用/禁用")
	@ResponseBody
	@RequestMapping(value = "/enable", method = RequestMethod.POST)
	public Result enable(
			@RequestParam String ids,
			@RequestParam Boolean isEnable) {
		userService.enable(isEnable, ConvertUtils.convertToIdList(ids));
		return Result.success();
	}

	@ApiOperation("新增/修改提交")
	@ResponseBody
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public Result save(
	        @RequestParam(required = false) Long id,
			@RequestParam Long officeId,
			@RequestParam(required = false) String name,
			@RequestParam String account,
			@RequestParam(required = false) String password,
			@RequestParam Boolean isEnable) {
		User user;
		if (id == null) {
			if (!StringUtils.hasLength(password)) {
				throw new ApplicationException(ErrorCodeEnum.E1001);
			}
			user = new User();
			user.setCreateTime(new Date());
		}
		else {
			user = userService.getById(id);
		}
		user.setOfficeId(officeId);
		user.setName(name);
		user.setAccount(account);
		if (StringUtils.hasLength(password)) {
			user.setPassword(PasswordHelper.encrypt(password));
		}
		user.setIsEnable(isEnable);
		user.setLoginCount(0);
		userService.saveOrUpdate(user);
		return Result.success();
	}

	@ApiOperation("重置密码")
	@ResponseBody
	@RequestMapping(value = "/reset-password", method = RequestMethod.POST)
	public Result resetPassword(
			@RequestParam String ids) {
		userService.resetPassword(PasswordHelper.encrypt(resetPassword), ConvertUtils.convertToIdList(ids));
		return Result.success();
	}

	@ApiOperation("删除")
	@ResponseBody
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public Result delete(
			@RequestParam String ids) {
		userService.deleteByIds(ConvertUtils.convertToIdList(ids));
		return Result.success();
	}
	
	@ApiOperation("机构树")
	@ResponseBody
	@RequestMapping(value = "/office/tree", method = RequestMethod.GET)
	public List<Office> officeTree() {
		return officeService.selectList(true, null, null, "--");
	}
}