package com.smart.sso.server.controller.admin;

import com.smart.core.entity.Result;
import com.smart.core.enums.ResultEnum;
import com.smart.sso.server.controller.BaseController;
import com.smart.sso.server.exception.ValidateException;
import com.smart.sso.server.model.Office;
import com.smart.sso.server.model.User;
import com.smart.sso.server.service.OfficeService;
import com.smart.sso.server.service.UserService;
import com.smart.sso.server.util.PasswordHelper;
import com.smart.sso.server.validator.ValidateParam;
import com.smart.sso.server.validator.Validator;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
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
public class UserController extends BaseController {

    @Value("${system.reset.password}")
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
	public String edit(@ValidateParam(name = "id") Integer id, Model model) {
		User user;
		if (id == null) {
			user = new User();
		}
		else {
			user = userService.getById(id);
		}
		model.addAttribute("user", user);
		model.addAttribute("officeList", officeService.selectList(true, null, null, "----"));
		return "/admin/userEdit";
	}

	@ApiOperation("列表")
	@ResponseBody
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public Result list(
			@ValidateParam(name = "登录名") String account,
			@ValidateParam(name = "姓名") String name,
			@ValidateParam(name = "机构ID") Integer officeId,
			@ValidateParam(name = "开始页码", defaultValue = DEFAULT_PAGE_NO) Integer pageNo,
	        @ValidateParam(name = "显示条数", defaultValue = DEFAULT_PAGE_SIZE) Integer pageSize) {
		return Result.createSuccess(userService.selectPage(account, name, officeId, pageNo, pageSize));
	}

	@ApiOperation("验证登录名")
	@ResponseBody
	@RequestMapping(value = "/validateAccount", method = RequestMethod.POST)
	public Result validateAccount(
	        @ValidateParam(name = "id") Integer id,
			@ValidateParam(name = "登录名", value = { Validator.NOT_BLANK }) String account) {
		User user = userService.selectByAccount(account);
		if (null != user && !user.getId().equals(id)) {
		    return Result.create(ResultEnum.ERROR.getCode(), "登录名已存在");
		}
		return Result.success();
	}

	@ApiOperation("启用/禁用")
	@ResponseBody
	@RequestMapping(value = "/enable", method = RequestMethod.POST)
	public Result enable(
	    @ValidateParam(name = "ids", value = { Validator.NOT_BLANK }) String ids,
			@ValidateParam(name = "是否启用", value = { Validator.NOT_BLANK }) Boolean isEnable) {
		userService.enable(isEnable, convertToIdList(ids));
		return Result.success();
	}

	@ApiOperation("新增/修改提交")
	@ResponseBody
	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public Result save(
	        @ValidateParam(name = "id") Integer id,
			@ValidateParam(name = "机构ID", value = { Validator.NOT_BLANK }) Integer officeId,
			@ValidateParam(name = "姓名") String name,
			@ValidateParam(name = "登录名", value = { Validator.NOT_BLANK }) String account,
			@ValidateParam(name = "密码 ") String password,
			@ValidateParam(name = "是否启用", value = { Validator.NOT_BLANK }) Boolean isEnable) {
		User user;
		if (id == null) {
			if (StringUtils.isEmpty(password)) {
				throw new ValidateException("密码不能为空");
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
		if (!StringUtils.isEmpty(password)) {
			user.setPassword(PasswordHelper.encrypt(password));
		}
		user.setIsEnable(isEnable);
		user.setLoginCount(0);
		userService.save(user);
		return Result.success();
	}

	@ApiOperation("重置密码")
	@ResponseBody
	@RequestMapping(value = "/resetPassword", method = RequestMethod.POST)
	public Result resetPassword(
	    @ValidateParam(name = "ids", value = { Validator.NOT_BLANK }) String ids) {
		userService.resetPassword(PasswordHelper.encrypt(resetPassword), convertToIdList(ids));
		return Result.success();
	}

	@ApiOperation("删除")
	@ResponseBody
	@RequestMapping(value = "/delete", method = RequestMethod.POST)
	public Result delete(
	    @ValidateParam(name = "ids", value = { Validator.NOT_BLANK }) String ids) {
		userService.deleteByIds(convertToIdList(ids));
		return Result.success();
	}
	
	@ApiOperation("机构树")
	@ResponseBody
	@RequestMapping(value = "/office/tree", method = RequestMethod.GET)
	public List<Office> officeTree() {
		List<Office> list = officeService.selectList(true, null, null, "");
		Office office = new Office();
		office.setId(null);
		office.setParentId(-1);
		office.setName("机构");
		list.add(0, office);
		return list;
	}
}